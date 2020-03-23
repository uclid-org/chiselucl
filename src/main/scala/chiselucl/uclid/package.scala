package chiselucl

import util.annotations._

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate, requireIsHardware}

package object uclid {

  object GuardSignal {
    def apply[T <: Data](data: T)(implicit compileOptions: CompileOptions): Unit = {
      if (compileOptions.checkSynthesizable) {
        requireIsHardware(data, "Signal used in Uclid property/invariant/assumption")
      }
      dontTouch(data)
    }
  }

  object Assume {
    def apply(condition: Bool): Bool = apply(condition, None)
    def apply(condition: Bool, name: String): Bool = apply(condition, Some(name))
    def apply(condition: Bool, name: Option[String])(implicit compileOptions: CompileOptions): Bool = {
      condition.getClass.getMethods.map(m => println(m.getName))
      name.foreach { condition.suggestName(_) }
      GuardSignal(condition)
      annotate(new ChiselAnnotation { def toFirrtl = UclidAssumptionAnnotation(condition.toTarget) })
      condition
    }
  }

  object Assert {
    def apply(condition: Boolean): Bool = apply(condition, None)
    def apply(condition: Bool, name: String): Bool = apply(condition, Some(name))
    def apply(condition: Bool, name: Option[String])(implicit compileOptions: CompileOptions): Bool = {
      name.foreach { condition.suggestName(_) }
      GuardSignal(condition)
      annotate(new ChiselAnnotation { def toFirrtl = UclidAssertAnnotation(condition.toTarget) })
      condition
    }
  }

}
