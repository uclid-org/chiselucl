// See LICENSE for license details.

package chiselucl
package backend

import firrtl._
import firrtl.ir._
import firrtl.Utils.kind
import firrtl.Mappers._

object SimplifyRegUpdate {
  def onStmt(namespace: Namespace)(stmt: Statement): Statement = stmt.map(onStmt(namespace)) match {
    case Connect(info, lhs, rhs) if kind(lhs) == RegKind =>
      //Fix to ensure that redundant `.*next_0` are not being generated. 
      //TODO: Verify this fix.
      if (!namespace.contains(s"${lhs.serialize}_next")) {
        val node = DefNode(info, namespace.newName(s"${lhs.serialize}_next"), rhs)
        //TODO: Verify that this bug fix is correct
        //val connect = Connect(info, lhs, WRef(node.name))
        val connect = Connect(info, lhs, WRef.apply(node))
        Block(Seq(node, connect))
      } else {
        stmt
      }
    case other => other
  }
  def onModule(mod: DefModule): DefModule = {
    val namespace = Namespace(mod)
    mod.map(onStmt(namespace))
  }
}

/** Makes RHS of connections to registers be a ref to a node
  * This ensures there's no logic in the RHS of connections to registers
  */
class SimplifyRegUpdate extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val c = state.circuit.map(SimplifyRegUpdate.onModule)
    state.copy(circuit = c)
  }
}
