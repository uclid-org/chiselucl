// See LICENSE for license details.

package chiselucl

import scala.language.implicitConversions

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate, requireIsHardware}

import annotations._
import properties.ir._

package object properties {

  implicit def bool2AtomicProposition(condition: Bool)(implicit compileOptions: CompileOptions): AtomicProposition = {
    if (compileOptions.checkSynthesizable) {
      chisel3.experimental.requireIsHardware(condition, "Signal used in Uclid property/invariant/assumption")
    }
    AtomicProposition(LeafChiselBool(condition))
  }

  object Assume {
    def apply(condition: Bool): Bool = apply(condition, None)
    def apply(condition: Bool, name: String): Bool = apply(condition, Some(name))
    def apply(condition: Bool, name: Option[String])(implicit compileOptions: CompileOptions): Bool = {
      name.foreach { condition.suggestName(_) }
      if (compileOptions.checkSynthesizable) {
        requireIsHardware(condition, "Signal used in Uclid property/invariant/assumption")
      }
      annotate(new ChiselAnnotation { def toFirrtl = UclidAssumptionAnnotation(condition.toTarget) })
      condition
    }
    def initialReset(reset: Reset): Unit = {
      // TODO: should check for port-type binding
      annotate(new ChiselAnnotation { def toFirrtl = UclidInitialAssumptionAnnotation(reset.toTarget, true) })
    }
    def initiallyTrue(flag: Bool): Unit = initially(flag, true)
    def initiallyFalse(flag: Bool): Unit = initially(flag, false)
    private def initially(flag: Bool, value: Boolean): Unit = {
      // TODO: should check for port-type binding
      annotate(new ChiselAnnotation { def toFirrtl = UclidInitialAssumptionAnnotation(flag.toTarget, value) })
    }
  }

  object Assert {
    def apply(condition: Bool): Bool = apply(condition, None)
    def apply(condition: Bool, name: String): Bool = apply(condition, Some(name))
    def apply(condition: Bool, name: Option[String])(implicit compileOptions: CompileOptions): Bool = {
      name.foreach { condition.suggestName(_) }
      if (compileOptions.checkSynthesizable) {
        requireIsHardware(condition, "Signal used in Uclid property/invariant/assumption")
      }
      annotate(new ChiselAnnotation { def toFirrtl = UclidAssertAnnotation(condition.toTarget) })
      condition
    }
  }

  object FreeConstant {
    def apply[T <: Data](t: T): T = {
      val constReg = Reg(t)
      constReg := constReg
      annotate(new ChiselAnnotation { def toFirrtl = UclidFreeConstantAnnotation(constReg.toTarget) })
      constReg
    }
  }
}
