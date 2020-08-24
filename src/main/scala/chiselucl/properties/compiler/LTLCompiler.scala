package chiselucl
package properties
package compiler

import firrtl.RenameMap
import firrtl.annotations.ReferenceTarget

import properties.ir._

object LTLCompiler {
  type LeafFunc = LeafBooleanValue => LeafBooleanValue
  def transformLeaves(ltl: LTLFormula)(f: LeafFunc): LTLFormula = {
    ltl match {
      case AtomicProposition(leaf) =>
        AtomicProposition(f(leaf))
      case u: UnaryLTLOp =>
        u.copy(operand = transformLeaves(u.operand)(f))
      case b: BinaryLTLOp =>
        b.copy(lOperand = transformLeaves(b.lOperand)(f), rOperand = transformLeaves(b.rOperand)(f))
      case _ => ltl
    }
  }

  def getTargets(ltl: LTLFormula): Seq[ReferenceTarget] = {
    var postOrder: List[ReferenceTarget] = Nil
    transformLeaves(ltl) {
      case lrt @ LeafReferenceTarget(rt) =>
        postOrder = rt :: postOrder
        lrt
      case leaf => leaf
    }
    postOrder
  }

  def captureRefTargets(ltl: LTLFormula): LTLFormula = {
    transformLeaves(ltl) {
      case LeafChiselBool(bool) => LeafReferenceTarget(bool.toTarget)
      case leaf => leaf
    }
  }

  def rename(ltl: LTLFormula, renames: RenameMap): LTLFormula = {
    transformLeaves(ltl) {
      case lrt @ LeafReferenceTarget(rt) =>
        renames.get(rt) match {
          case None => lrt
          case Some(Seq(rtPrime: ReferenceTarget)) => LeafReferenceTarget(rtPrime)
          case Some(s: Seq[_]) if (s.size > 1) =>
            throw new Exception("A reference target to a boolean should not be split")
          case Some(s: Seq[_]) if (s.isEmpty) =>
            throw new Exception("An LTL property leaf should not be deleted")
          case _ =>
            throw new Exception("An LTL ref should remain a reference target")
        }
      case leaf => leaf
    }
  }

  def finalize(ltl: LTLFormula): LTLFormula = {
    transformLeaves(ltl) {
      case LeafReferenceTarget(rt) =>
        assert(rt.path.isEmpty)
        assert(rt.component.isEmpty)
        // For now, support only refs to bool wires/nodes/regs
        // Combo of Chisel features (passthru node creation) and lowering guarantee this
        RawUclidExpression(rt.ref)
      case leaf => leaf
    }
  }

  def serializeUCL(ltl: LTLFormula): String = {
    ltl match {
      case AtomicProposition(RawUclidExpression(ucl)) => ucl
      case AtomicProposition(leaf) =>
        throw new IllegalArgumentException(s"Cannot serialize un-finalized AP: ${leaf}")
      case UnaryLTLOp(op, operand) =>
        val arg = serializeUCL(operand)
        op match {
          case Inverse => s"!${arg}"
          case Globally => s"G(${arg})"
          case Next => s"X(${arg})"
          case Until => s"U(${arg})"
          case Finally => s"F(${arg})"
          case Release => s"R(${arg})"
          case WeakUntil => s"W(${arg})"
        }
      case BinaryLTLOp(op, lOperand, rOperand) =>
        val l = serializeUCL(lOperand)
        val r = serializeUCL(rOperand)
        op match {
          case Implies => s"${l} ==> ${r}"
          case Iff => s"${l} <==> ${r}"
          case Conjunction => s"(${l} && ${r})"
          case Disjunction => s"(${l} || ${r})"
        }
      case _ =>
        throw new IllegalArgumentException(s"Cannot serialize un-finalized LTL IR: ${ltl}")
    }
  }
}
