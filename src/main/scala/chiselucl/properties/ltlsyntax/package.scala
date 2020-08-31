// See LICENSE for license details.

package chiselucl
package properties

import annotations._
import properties.ir._
import properties.compiler._

package object ltlsyntax {
  object LTL {
    def apply(formula: LTLFormula, name: String): Unit = {
      chisel3.experimental.annotate(new chisel3.experimental.ChiselAnnotation {
        def toFirrtl = {
          val capturedFormula = LTLCompiler.captureRefTargets(formula)
          LTLCompiler.getTargets(capturedFormula) match {
            case rt :: tail => UclidLTLAnnotation(name, rt.moduleTarget, capturedFormula)
            case Nil =>
              throw new Exception("LTL properties must include at least one signal")
          }
        }
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
