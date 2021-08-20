// SPDX-License-Identifier: Apache-2.0

package chisel3.experimental.hierarchy

import chisel3.experimental.BaseModule
import chisel3.internal.sourceinfo.SourceInfo
import chisel3.internal.BaseModule.{InstanceClone, InstantiableClone, IsClone, ModuleClone}

import scala.annotation.implicitNotFound
import chisel3._
import chisel3.experimental.dataview.isView
import chisel3.internal.{Builder, ViewBinding, ViewParent}

trait IsLookupable

trait IsInstantiable

@implicitNotFound("@public is only legal within a class marked @instantiable and only on vals of type" +
  " Data, BaseModule, IsInstantiable, IsLookupable, or Instance[_], or in an Iterable or Option")
sealed trait Lookupable[-B] {
  type C
  def lookup[A](that: A => B, ih: Instance[A]): C
}

object Lookupable {

  private def cloneDataToContext[T <: Data](child: T, context: BaseModule)
                                           (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T = {
    internal.requireIsHardware(child, "cross module reference type")
    child._parent match {
      case None => child
      case Some(parent) =>
        val newParent = cloneModuleToContext(Left(parent), context)
        newParent match {
          case Left(p) if p == parent => child
          case Right(m: BaseModule) =>
            val newChild = child.cloneTypeFull
            newChild.setRef(child.getRef, true)
            newChild.bind(internal.CrossModuleBinding)
            newChild.setAllParents(Some(m))
            newChild
        }
    }
  }
  // The business logic of lookupData
  // Also called by cloneViewToContext which potentially needs to lookup stuff from ioMap or the cache
  private def doLookupData[A, B <: Data](data: B, ih: Instance[A], ioMap: Option[Map[Data, Data]], context: Option[BaseModule])
                                        (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): B = {
    data match {
      case x: Data if ioMap.nonEmpty && ioMap.get.contains(x) => ioMap.get(x).asInstanceOf[B]
      case x: Data if ih.cache.contains(x) => ih.cache(x).asInstanceOf[B]
      case _ =>
        assert(context.nonEmpty) // TODO is this even possible? Better error message here
        cloneDataToContext(data, context.get)
    }
  }
  private def cloneViewToContext[A, B <: Data](data: B, ih: Instance[A], ioMap: Option[Map[Data, Data]], context: Option[BaseModule])
                                           (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): B = {
    val newBinding = data.topBinding match {
      case ViewBinding(target) => ViewBinding(doLookupData(target, ih, ioMap, context))
    }
    val result = data.cloneTypeFull
    result.bind(newBinding)
    // TODO unify these two lines with `.viewAs`
    result.setAllParents(Some(ViewParent))
    result.forceName(None, "view", Builder.viewNamespace)
    result
  }
  private def cloneModuleToContext[T <: BaseModule](child: Either[T, IsClone[T]], context: BaseModule)
                          (implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): Either[T, IsClone[T]] = {
    def rec[A <: BaseModule](m: A): Either[A, IsClone[A]] = {
      (m, context) match {
        case (c, ctx) if ctx == c => Left(c)
        case (c, ctx: IsClone[_]) if ctx.isACloneOf(c) => Right(ctx.asInstanceOf[IsClone[A]])
        case (c, ctx) if c._parent.isEmpty => Left(c)
        case (_, _) => 
          cloneModuleToContext(Left(m._parent.get), context) match {
            case Left(p) => Left(m)
            case Right(p: BaseModule) =>
              val newChild = Module.do_apply(new internal.BaseModule.InstanceClone(m, () => m.instanceName))
              newChild._parent = Some(p)
              Right(newChild)
          }
      }
    }
    child match {
      case Left(m) => rec(m)
      case Right(m: ModuleClone[_]) =>
        rec(m) match {
          case Left(mx) => Right(mx)
          case Right(i: InstanceClone[_]) =>
            val newChild = Module.do_apply(new InstanceClone(m._proto, () => m.instanceName))
            newChild._parent = i._parent
            Right(newChild)
        }
      case Right(m: InstanceClone[_]) =>
        rec(m) match {
          case Left(mx) => Right(mx)
          case Right(i: InstanceClone[_]) =>
            val newChild = Module.do_apply(new InstanceClone(m._proto, () => m.instanceName))
            newChild._parent = i._parent
            Right(newChild)
        }
    }
  }
  implicit def lookupInstance[B <: BaseModule](implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[Instance[B]] {
    type C = Instance[B]
    def lookup[A](that: A => Instance[B], ih: Instance[A]): C = {
      val ret = that(ih.definition)
      ih.cloned match {
        // If ih is just a normal module, no changing of context is necessary
        case Left(_)  => new Instance(ret.cloned)
        case Right(_) => new Instance(cloneModuleToContext(ret.cloned, ih.getInnerDataContext.get))
      }
    }
  }
  implicit def lookupModule[B <: BaseModule](implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[B] {
    type C = Instance[B]
    def lookup[A](that: A => B, ih: Instance[A]): C = {
      val ret = that(ih.definition)
      ih.cloned match {
        // If ih is just a normal module, no changing of context is necessary
        case Left(_)  => new Instance(Left(ret))
        case Right(_) => new Instance(cloneModuleToContext(Left(ret), ih.getInnerDataContext.get))
      }
    }
  }
  implicit def lookupData[B <: Data](implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[B] {
    type C = B
    def lookup[A](that: A => B, ih: Instance[A]): C = {
      val ret = that(ih.definition)
      val ioMap: Option[Map[Data, Data]] = ih.cloned match {
        case Right(x: ModuleClone[_]) => Some(x.ioMap)
        case Left(x: BaseModule) => Some(x.getChiselPorts.map { case (_, data) => data -> data }.toMap)
        case _ => None
      }
      if (isView(ret)) {
        cloneViewToContext(ret, ih, ioMap, ih.getInnerDataContext)
      } else {
        doLookupData(ret, ih, ioMap, ih.getInnerDataContext)
      }

    }
  }
  implicit def lookupIterable[B, F[_] <: Iterable[_]](implicit sourceInfo: SourceInfo, compileOptions: CompileOptions, lookupable: Lookupable[B]) = new Lookupable[F[B]] {
    type C = F[lookupable.C]
    def lookup[A](that: A => F[B], ih: Instance[A]): C = {
      import ih._
      val ret = that(definition).asInstanceOf[Iterable[B]]
      ret.map{ x: B => lookupable.lookup[A](_ => x, ih) }.asInstanceOf[C]
    }
  }
  implicit def lookupOption[B](implicit sourceInfo: SourceInfo, compileOptions: CompileOptions, lookupable: Lookupable[B]) = new Lookupable[Option[B]] {
    type C = Option[lookupable.C]
    def lookup[A](that: A => Option[B], ih: Instance[A]): C = {
      import ih._
      val ret = that(definition)
      ret.map{ x: B => lookupable.lookup[A](_ => x, ih) }
    }
  }
  implicit def lookupIsLookupable[B <: IsLookupable](implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[B] {
    type C = B
    def lookup[A](that: A => B, ih: Instance[A]): C = that(ih.definition)
  }
  implicit def lookupIsInstantiable[B <: IsInstantiable](implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[B] {
    type C = Instance[B]
    def lookup[A](that: A => B, ih: Instance[A]): C = {
      val ret = that(ih.definition)
      val cloned = new InstantiableClone(ret)
      cloned._parent = ih.getInnerDataContext
      new Instance(Right(cloned))
    }
  }
  // TODO should these be vals?
  implicit def lookupString(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[String] {
    type B = String
    type C = String
    def lookup[A](that: A => B, ih: Instance[A]): C = that(ih.definition)
  }
  implicit def lookupInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[Int] {
    type B = Int
    type C = Int
    def lookup[A](that: A => B, ih: Instance[A]): C = that(ih.definition)
  }
  implicit def lookupBoolean(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[Boolean] {
    type B = Boolean
    type C = Boolean
    def lookup[A](that: A => B, ih: Instance[A]): C = that(ih.definition)
  }
  implicit def lookupBigInt(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions) = new Lookupable[BigInt] {
    type B = BigInt
    type C = BigInt
    def lookup[A](that: A => B, ih: Instance[A]): C = that(ih.definition)
  }
}
