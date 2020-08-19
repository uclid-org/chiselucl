package chiselucl
package properties
package ir

import chisel3.Bool
import firrtl.annotations.ReferenceTarget

sealed trait LTLFormula {
  def ==>(that: LTLFormula): BinaryLTLOp = Implies(this, that)
  def <==>(that: LTLFormula): BinaryLTLOp= Iff(this, that)
  def &&(that: LTLFormula): BinaryLTLOp = Conjunction(this, that)
  def ||(that: LTLFormula): BinaryLTLOp = Disjunction(this, that)
  def unary_~ : UnaryLTLOp = Inverse(this)
}

sealed trait LeafBooleanValue extends LTLFormula

case class RawUclidExpression(rawUclid: String) extends LeafBooleanValue

case class LeafChiselBool(boolExpr: Bool) extends LeafBooleanValue

case class LeafReferenceTarget(target: ReferenceTarget) extends LeafBooleanValue

case class AtomicProposition(value: LeafBooleanValue) extends LTLFormula

case class UnaryLTLOp(op: UnaryLTLOperator, operand: LTLFormula) extends LTLFormula

case class BinaryLTLOp(op: BinaryLTLOperator, lOperand: LTLFormula, rOperand: LTLFormula) extends LTLFormula

sealed trait UnaryLTLOperator {
  def apply(operand: LTLFormula): UnaryLTLOp = UnaryLTLOp(this, operand)
}

case object Inverse extends UnaryLTLOperator
case object Globally extends UnaryLTLOperator
case object Next extends UnaryLTLOperator
case object Until extends UnaryLTLOperator
case object Finally extends UnaryLTLOperator
case object Release extends UnaryLTLOperator
case object WeakUntil extends UnaryLTLOperator

sealed trait BinaryLTLOperator {
  def apply(lOperand: LTLFormula, rOperand: LTLFormula): BinaryLTLOp = BinaryLTLOp(this, lOperand, rOperand)
}

case object Implies extends BinaryLTLOperator
case object Iff extends BinaryLTLOperator
case object Conjunction extends BinaryLTLOperator
case object Disjunction extends BinaryLTLOperator
