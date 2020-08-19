package chiselucl
package properties

import chisel3._

trait LTLFormula

trait UnaryLTLOp extends LTLFormula {
  val operand: LTLFormula
  val symbol: String
}

trait BinaryLTLOp extends LTLFormula {
  val lOperand: LTLFormula
  val rOperand: LTLFormula
  val symbol: String
}

case class AtomicProposition(value: Bool) extends LTLFormula

case class Globally(operand: LTLFormula) extends UnaryLTLOp {
  val symbol = "G"
}

case class Next(operand: LTLFormula) extends UnaryLTLOp {
  val symbol = "X"
}

case class Until(operand: LTLFormula) extends UnaryLTLOp {
  val symbol = "U"
}

case class Finally(operand: LTLFormula) extends UnaryLTLOp {
  val symbol = "F"
}

case class Release(operand: LTLFormula) extends UnaryLTLOp {
  val symbol = "R"
}

case class WeakUntil(operand: LTLFormula) extends UnaryLTLOp {
  val symbol = "W"
}

case class Implies(lOperand: LTLFormula, rOperand: LTLFormula) extends BinaryLTLOp {
  val symbol = "==>"
}
