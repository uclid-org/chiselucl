package chiselucl

import scala.language.implicitConversions

import chisel3._

import properties.ir._

package object properties {
  implicit def bool2AtomicProposition(condition: Bool)(implicit compileOptions: CompileOptions): AtomicProposition = {
    if (compileOptions.checkSynthesizable) {
      chisel3.experimental.requireIsHardware(condition, "Signal used in Uclid property/invariant/assumption")
    }
    AtomicProposition(LeafChiselBool(condition))
  }
}
