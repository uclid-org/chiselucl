// See LICENSE for license details.

package chiselucl

import firrtl.RenameMap
import firrtl.annotations._
import firrtl.transforms.{HasDontTouches, DontTouchAllTargets}

import properties.ir._
import properties.compiler._

// TODO: add source locators to uclid emission, including on annotation-based properties

package object annotations {

  case class DebugLevel(debug: Boolean) extends NoTargetAnnotation

  trait ModuleLevelProperty {
    def serializeUCL: String
    def enclosingModule: ModuleTarget
  }

  case class BMCAnnotation(steps: BigInt) extends NoTargetAnnotation

  case class UclidAssumptionAnnotation(target: ReferenceTarget)
      extends SingleTargetAnnotation[ReferenceTarget]
      with DontTouchAllTargets
      with ModuleLevelProperty {
    def targets = Seq(target)
    def duplicate(t: ReferenceTarget) = this.copy(t)
    def serializeUCL = s"assume assume_${target.ref} : ${target.ref};"
    def enclosingModule: ModuleTarget = target.moduleTarget
  }

  case class UclidAssertAnnotation(target: ReferenceTarget)
      extends SingleTargetAnnotation[ReferenceTarget]
      with DontTouchAllTargets
      with ModuleLevelProperty {
    def targets = Seq(target)
    def duplicate(t: ReferenceTarget) = this.copy(t)
    def serializeUCL = s"invariant assert_${target.ref} : ${target.ref};"
    def enclosingModule: ModuleTarget = target.moduleTarget
  }

  case class UclidLTLAnnotation(name: String, module: ModuleTarget, formula: LTLFormula)
      extends Annotation
      with HasDontTouches
      with ModuleLevelProperty {

    def dontTouches: Iterable[ReferenceTarget] = LTLCompiler.getTargets(formula)

    def update(renames: RenameMap) = {
      val newFormula = LTLCompiler.rename(formula, renames)
      renames(module).collect {
        case newMod: ModuleTarget => UclidLTLAnnotation(name, newMod, newFormula)
      }
    }

    override def getTargets: Seq[Target] = {
      val fTargets: Seq[Target] = LTLCompiler.getTargets(formula)
      module +: fTargets
    }

    def serializeUCL = {
      val finalizedNames = LTLCompiler.finalize(formula)
      val formulaString = LTLCompiler.serializeUCL(finalizedNames)
      s"property[LTL] ${name}: ${formulaString};"
    }

    def enclosingModule: ModuleTarget = module
  }
}
