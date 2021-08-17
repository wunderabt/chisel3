// SPDX-License-Identifier: Apache-2.0

package chiselTests.experimental.hierarchy

import chisel3._
import _root_.firrtl.annotations._
import chiselTests.ChiselRunners

trait Utils extends ChiselRunners with chiselTests.Utils {
  import Annotations._
  def check(bc: => RawModule, target: IsMember, tag: String, printOutput: Boolean = false): Unit = {
    val (output, annotations) = getFirrtlAndAnnos(bc)
    if (printOutput) println(output.serialize)
    val anno = MarkAnnotation(target, tag)
    assert(annotations.contains(anno), s"${annotations} does not contain $anno!")
  }
  def check(bc: => RawModule, instance: String, of: String): Unit = {
    val (output, _) = getFirrtlAndAnnos(bc)
    val insts = output.serialize.split("\n").map(_.split("\\s+").toList).collect {
      case _ :: "inst" :: x :: "of" :: y :: _ => (x, y)
    }
    assert(insts.contains((instance, of)), s"""Output does not contain $instance of $of! Contains the following:${"\n"}${insts.mkString("\n")}""")
  }
  def check(bc: => RawModule, targets: Seq[(IsMember, String)]): Unit = {
    val (_, annotations) = getFirrtlAndAnnos(bc)
    targets.foreach { case (target, tag) =>
      val anno = MarkAnnotation(target, tag)
      assert(annotations.contains(anno), s"${annotations} does not contain $anno!")
    }
  }
  // TODO promote to standard API (in FIRRTL) and perhaps even implement with a macro
  implicit class Str2RefTarget(str: String) {
    def rt: ReferenceTarget = Target.deserialize(str).asInstanceOf[ReferenceTarget]
    def it: InstanceTarget = Target.deserialize(str).asInstanceOf[InstanceTarget]
    def mt: ModuleTarget = Target.deserialize(str).asInstanceOf[ModuleTarget]
    def ct: CircuitTarget = Target.deserialize(str).asInstanceOf[CircuitTarget]
  }
}
