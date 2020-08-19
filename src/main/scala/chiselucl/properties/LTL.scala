package chiselucl
package properties

import annotations._
import properties.ir._
import properties.transforms._

object LTL {
  def apply(formula: LTLFormula, name: String): Unit = {
    chisel3.experimental.annotate(new chisel3.experimental.ChiselAnnotation {
      def toFirrtl = UclidLTLAnnotation(name, LTLCompiler.captureRefTargets(formula))
    })
  }
}

object AP {
  def apply(condition: chisel3.Bool): AtomicProposition = {
    bool2AtomicProposition(condition)
  }
}
