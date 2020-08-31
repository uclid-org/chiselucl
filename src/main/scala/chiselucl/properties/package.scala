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

}
