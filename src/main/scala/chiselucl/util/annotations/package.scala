package chiselucl
package util

import firrtl.annotations._
import firrtl.transforms.DontTouchAllTargets

package object annotations {

  case class DebugLevel(debug: Boolean) extends NoTargetAnnotation

  trait ModuleLevelProperty {
    def serializeUCL: String
  }

  case class BMCAnnotation(steps: BigInt) extends NoTargetAnnotation

  case class UclidAssumptionAnnotation(target: ReferenceTarget)
      extends SingleTargetAnnotation[ReferenceTarget]
      with DontTouchAllTargets
      with ModuleLevelProperty {
    def targets = Seq(target)
    def duplicate(t: ReferenceTarget) = this.copy(t)
    def serializeUCL = s"assume assume_${target.ref} : ${target.ref};"
  }

  case class UclidAssertAnnotation(target: ReferenceTarget)
      extends SingleTargetAnnotation[ReferenceTarget]
      with DontTouchAllTargets
      with ModuleLevelProperty {
    def targets = Seq(target)
    def duplicate(t: ReferenceTarget) = this.copy(t)
    def serializeUCL = s"assert assert_${target.ref} : ${target.ref};"
  }
}
