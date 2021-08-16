// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.hierarchy

import scala.language.experimental.macros

import chisel3._
import chisel3.internal.{Builder, DynamicContext}
import chisel3.internal.sourceinfo.{InstTransform, SourceInfo}
import chisel3.experimental.BaseModule

object Definition extends SourceInfoDoc {
  /** A wrapper method that all Definition instantiations must be wrapped in
    * (necessary to help Chisel track internal state).
    *
    * @param bc the Module being created
    *
    * @return the input module Definition with Chisel metadata properly set
    */
  def apply[T <: RawModule](bc: => T): Definition[T] = macro InstTransform.apply[T]

  def do_apply[T <: RawModule](bc: => T) (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Definition[T] = {
    val dynamicContext = new DynamicContext(Nil)
    Builder.globalNamespace.copyTo(dynamicContext.globalNamespace)
    val (ir, module) = Builder.build(Module(bc), dynamicContext)
    Builder.components ++= ir.components
    Builder.annotations ++= ir.annotations
    module._circuit = Builder.currentModule
    dynamicContext.globalNamespace.copyTo(Builder.globalNamespace)
    new Definition(module, "blah")
  }
}

case class Definition[T <: BaseModule] private (module: T, other: String)
