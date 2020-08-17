package chiselucl
package verification

import chisel3._
import chisel3.experimental.{requireIsHardware}

//TODO: Need to add more elements to this language so that we can 
// guard signals more precisely

//TODO: This needs to be moved to somewhere in the chisel/firrtl repos, since 
//          we want to expose a common verification front-end. Note that we 
//          also want people to be able to easily write their own verification
//          frontends, so we might also want the verification passes to be
//          aware of the specs they are using

//TODO: Consult Albert on this

package object lang {

  sealed trait VerificationFormula

  object GuardSignal {
    def apply[T <: Data](data: T)(implicit compileOptions: CompileOptions): Unit = {
      if (compileOptions.checkSynthesizable) {
        requireIsHardware(data, "Signal used in verification formula")
      }
      dontTouch(data)
    }
  }
  

  object Assume extends VerificationFormula {
    def apply(condition: Bool): Bool = apply(condition, None)
    def apply(condition: Bool, name: String): Bool = apply(condition, Some(name))
    def apply(condition: Bool, name: Option[String])(implicit compileOptions: CompileOptions): Bool = {
      name.foreach { condition.suggestName(_) }
      GuardSignal(condition)
      //TODO: This needs to be modified to make UCLID agnostic
      //annotate(new ChiselAnnotation { def toFirrtl = UclidAssumptionAnnotation(condition.toTarget) })
      condition
    }
  }

  object Assert extends VerificationFormula {
    def apply(condition: Bool): Bool = apply(condition, None)
    def apply(condition: Bool, name: String): Bool = apply(condition, Some(name))
    def apply(condition: Bool, name: Option[String])(implicit compileOptions: CompileOptions): Bool = {
      name.foreach { condition.suggestName(_) }
      GuardSignal(condition)
      //TODO: This needs to be modifies to make UCLID agnostic
      //annotate(new ChiselAnnotation { def toFirrtl = UclidAssertAnnotation(condition.toTarget) })
      condition
    }

  }

}
