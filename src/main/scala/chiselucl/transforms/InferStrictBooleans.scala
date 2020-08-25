// See LICENSE for license details.

package chiselucl
package transforms

import firrtl._
import firrtl.ir._
import firrtl.annotations.{ReferenceTarget, TargetToken}

import chiselucl.annotations.ModuleLevelProperty

private[chiselucl] case object StrictBooleanType extends Type {
  def serialize: String = ??? // Does not emit!
  def mapType(f: Type => Type): Type = StrictBooleanType
  def mapWidth(f: Width => Width): Type = StrictBooleanType
  def foreachType(f: Type => Unit): Unit = ()
  def foreachWidth(f: Width => Unit): Unit = ()
}

private[chiselucl] case class ToStrictBoolean(expr: Expression) extends Expression {
  def serialize: String = expr.serialize // Does not emit!
  def tpe: Type = StrictBooleanType
  def mapExpr(f: Expression => Expression): Expression = ToStrictBoolean(f(expr))
  def mapType(f: Type => Type): Expression = this
  def mapWidth(f: Width => Width): Expression = this
  def foreachExpr(f: Expression => Unit): Unit = f(expr)
  def foreachType(f:  Type => Unit): Unit = ()
  def foreachWidth(f: Width => Unit): Unit = ()
}

object InferStrictBooleans extends Transform with DependencyAPIMigration {

  override def prerequisites = firrtl.stage.Forms.LowForm
  override def optionalPrerequisites = Nil
  override def optionalPrerequisiteOf = Nil
  override def invalidates(a: Transform): Boolean = false

  private val firrtlBool = firrtl.Utils.BoolType

  override def execute(cs: CircuitState): CircuitState = {
    val apBooleans: Seq[ReferenceTarget] = cs.annotations.flatMap {
      case mlp: ModuleLevelProperty => mlp.getTargets.collect { case rt: ReferenceTarget => rt }
      case _ => Nil
    }
    val apBooleansByModule = apBooleans.groupBy(ap => ap.module)
    val renames = RenameMap()
    val transformedModules = cs.circuit.modules.map {
      case m: Module if apBooleansByModule.contains(m.name) =>
        val ns = Namespace(m)
        val strictBoolNodes: Seq[DefNode] = apBooleansByModule(m.name).map {
          case rt: ReferenceTarget if !rt.isLocal =>
            throw new Exception("LTL properties currently do not support cross-module references.")
          case rt: ReferenceTarget => rt.component match {
            case Nil =>
              val nodeName = ns.newName(s"${rt.ref}_strict_boolean")
              renames.record(rt, rt.copy(ref = nodeName))
              DefNode(NoInfo, nodeName, ToStrictBoolean(WRef(rt.ref, firrtlBool, UnknownKind, SourceFlow)))
            case Seq(TargetToken.Field(sf)) =>
              val nodeName = ns.newName(s"${rt.ref}_${sf}_strict_boolean")
              renames.record(rt, rt.copy(ref = nodeName, component = Nil))
              val refExp = WRef(rt.ref, UnknownType, UnknownKind, SourceFlow)
              DefNode(NoInfo, nodeName, ToStrictBoolean(WSubField(refExp, sf, firrtlBool, SourceFlow)))
          }
        }
        m.copy(body = Block(m.body +: strictBoolNodes))
      case m => m
    }
    val transformedAnnos = cs.annotations.flatMap {
      case mlp: ModuleLevelProperty => mlp.update(renames)
      case anno => Seq(anno)
    }
    cs.copy(circuit = cs.circuit.copy(modules = transformedModules), annotations = transformedAnnos)
  }

}
