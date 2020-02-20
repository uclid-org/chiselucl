package chiselucl
package util

import firrtl.annotations._

package object annotations {

  trait ModuleLevel {
    def serializeUCL: String
  }

  case class BMCAnnotation(val steps: BigInt) extends NoTargetAnnotation

  case class UclidAssumptionAnnotation(val target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] with ModuleLevel {
    def targets = Seq(target)
    def duplicate(t: ReferenceTarget) = this.copy(t)
    def serializeUCL = s"assume assume_${target.ref} : ${target.ref};"
  }

  case class UclidAssertAnnotation(val target: ReferenceTarget) extends SingleTargetAnnotation[ReferenceTarget] with ModuleLevel {
    def targets = Seq(target)
    def duplicate(t: ReferenceTarget) = this.copy(t)
    def serializeUCL = s"assert assert_${target.ref} : ${target.ref};"
  }
}
