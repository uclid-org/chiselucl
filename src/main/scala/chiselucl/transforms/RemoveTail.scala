// See LICENSE for license details.

package chiselucl
package transforms

import firrtl._
import firrtl.ir._
import firrtl.Mappers._

object RemoveTail {
  def removeTailE(expr: Expression): Expression = expr.map(removeTailE) match {
    case DoPrim(PrimOps.Tail, Seq(e), Seq(amt), tpe) =>
      val top = bitWidth(e.tpe) - amt
      DoPrim(PrimOps.Bits, Seq(e), Seq(top - 1, 0), tpe)
    case other => other
  }

  def removeTailS(stmt: Statement): Statement = stmt.map(removeTailS).map(removeTailE)

  def removeTailM(mod: DefModule): DefModule = mod.map(removeTailS)
}

/** Remove tail operations from low firrtl */
class RemoveTail extends Transform {
  import RemoveTail._
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val c = state.circuit.map(removeTailM)
    state.copy(circuit = c)
  }
}
