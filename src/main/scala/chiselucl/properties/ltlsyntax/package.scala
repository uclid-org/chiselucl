package chiselucl
package properties

import annotations._
import properties.ir._
import properties.transforms._

package object ltlsyntax {
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

  object G {
    def apply(condition: LTLFormula): UnaryLTLOp = ir.Globally(condition)
  }

  object X {
    def apply(condition: LTLFormula): UnaryLTLOp = ir.Next(condition)
  }

  object U {
    def apply(condition: LTLFormula): UnaryLTLOp = ir.Until(condition)
  }

  object F {
    def apply(condition: LTLFormula): UnaryLTLOp = ir.Finally(condition)
  }

  object R {
    def apply(condition: LTLFormula): UnaryLTLOp = ir.Release(condition)
  }

  object W {
    def apply(condition: LTLFormula): UnaryLTLOp = ir.WeakUntil(condition)
  }
}
