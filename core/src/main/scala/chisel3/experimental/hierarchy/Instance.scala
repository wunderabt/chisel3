// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.hierarchy

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.language.experimental.macros

import chisel3._
import chisel3.internal.BaseModule.{ModuleClone, IsClone, InstantiableClone}
import chisel3.internal.sourceinfo.{InstTransform, SourceInfo}
import chisel3.experimental.BaseModule

case class Instance[+A] private [chisel3] (cloned: Either[A, IsClone[A]]) {
  def definition: A = cloned match {
    case Left(value: A) => value
    case Right(i: IsClone[A]) => i._proto
  }
  def getInnerDataContext: Option[BaseModule] = cloned match {
    case Left(value: BaseModule)        => Some(value)
    case Left(value: IsInstantiable)    => None
    case Right(i: BaseModule)           => Some(i)
    case Right(i: InstantiableClone[_]) => i._parent
  }
  def getClonedParent: Option[BaseModule] = cloned match {
    case Left(value: BaseModule) => value._parent
    case Right(i: BaseModule)           => i._parent
    case Right(i: InstantiableClone[_]) => i._parent
  }

  private [chisel3] val cache = HashMap[Data, Data]()

  def apply[B, C](that: A => B)(implicit lookup: Lookupable[B]): lookup.C = {
    lookup.lookup(that, this)
  }
  def toTarget = cloned match {
    case Left(x: BaseModule) => x.toTarget
    case Right(x: chisel3.internal.BaseModule.ModuleClone[_]) => x.toTarget
    case Right(x: chisel3.internal.BaseModule.InstanceClone[_]) => x.toTarget
    case other => throw new Exception(s"toTarget is not supported on $this")
  }
  def toAbsoluteTarget = cloned match {
    case Left(x: BaseModule) => x.toAbsoluteTarget
    case Right(x: chisel3.internal.BaseModule.ModuleClone[_]) => x.toAbsoluteTarget
    case Right(x: chisel3.internal.BaseModule.InstanceClone[_]) => x.toAbsoluteTarget
    case other => throw new Exception(s"toAbsoluteTarget is not supported on $this")
  }
}
/** Factory methods for constructing [[Instance]]s */
object Instance extends SourceInfoDoc {
  /** A wrapper method that all Module instantiations must be wrapped in
    * (necessary to help Chisel track internal state).
    *
    * @param bc the Module being created
    * @return the input module `m` with Chisel metadata properly set
    */
  def apply[T <: BaseModule](bc: Definition[T]): Instance[T] = macro InstTransform.apply[T]

  /** @group SourceInfoTransformMacro */
  def do_apply[T <: BaseModule](bc: Definition[T])(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Instance[T] = {
    val ports = experimental.CloneModuleAsRecord(bc.module)
    val clone = ports._parent.get.asInstanceOf[ModuleClone[T]]
    clone._madeFromDefinition = true
    //println(s"In do_apply: ports=$ports")
    new Instance(Right(clone))
  }


  /** Wrap a [[BaseModule]] or any other [[IsInstantiable]] as an [[Instance]]
    *
    * The returned object refers to the exact instance passed to this method--it is **not** a new instance.
    * To create a copy or new instance, use [[Instance$.apply]] on a [[Definition]]
    *
    * @param inst the object to wrap as an Instance
    * @return An [[Instance]] wrapping the argument
    */
  def wrap[T <: IsInstantiable](inst: T): Instance[T] = new Instance(Left(inst))
}
