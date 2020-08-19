package chiselucl

import firrtl.RenameMap
import firrtl.annotations._
import firrtl.transforms.DontTouchAllTargets

import properties.ir._
import properties.transforms._

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

  case class UclidLTLAnnotation(name: String, formula: LTLFormula)
      extends Annotation
      with DontTouchAllTargets
      with ModuleLevelProperty {

    def update(renames: RenameMap) = Seq(this.copy(formula = LTLCompiler.rename(formula, renames)))

    override def getTargets: Seq[Target] = {
      var postOrder: List[ReferenceTarget] = Nil
      LTLCompiler.transformLeaves(formula) {
        case lrt @ LeafReferenceTarget(rt) =>
          postOrder = rt :: postOrder
          lrt
        case leaf => leaf
      }
      postOrder
    }

    def serializeUCL = {
      val finalizedNames = LTLCompiler.finalize(formula)
      val formulaString = LTLCompiler.serializeUCL(finalizedNames)
      s"property[LTL] ${name}: ${formulaString};"
    }
  }
}
