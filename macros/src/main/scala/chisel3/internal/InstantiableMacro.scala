// SPDX-License-Identifier: Apache-2.0

package chisel3.internal

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox


private[chisel3] object instantiableMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val result = {
      val (clz, objOpt) = annottees.map(_.tree).toList match {
        case Seq(c, o) => (c, Some(o))
        case Seq(c) => (c, None)
      }
      val (newClz, implicitClz, tpname) = clz match {
        case clz @ q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
          val name2 = TypeName(tpname.+(c.freshName()))
          val extensions = scala.collection.mutable.ArrayBuffer[c.universe.Tree]()
          val newBodies = stats.flatMap {
            case x @ q"@public val $tpname: $tpe = $name" if tpname.toString() == name.toString() =>
              extensions += atPos(x.pos)(q"def $tpname = module(_.$tpname)")
              Nil
            case x @ q"@public val $tpname: $tpe = $_" =>
              extensions += atPos(x.pos)(q"def $tpname = module(_.$tpname)")
              Seq(x)
            case x @ q"@public lazy val $tpname: $tpe = $_" =>
              extensions += atPos(x.pos)(q"def $tpname = module(_.$tpname)")
              Seq(x)
            case other => Seq(other)
          }
          (q""" $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..${newBodies.toSeq} } """,
           q"""implicit class $name2(module: chisel3.experimental.hierarchy.Instance[$tpname]) { ..$extensions } """,
           tpname)
      }
      val newObj = objOpt match {
        case None => q"""object ${tpname.toTermName} { $implicitClz } """
        case Some(obj @ q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }") =>
          q"""
            $mods object $tname extends { ..$earlydefns } with ..$parents { $self =>
              $implicitClz
              ..$body
            }
          """
      }
      q"""
        $newClz

        $newObj
      """
    }
    c.Expr[Any](result)
  }
}

private[chisel3] class instantiable extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro instantiableMacro.impl
}
private[chisel3] class public extends StaticAnnotation
