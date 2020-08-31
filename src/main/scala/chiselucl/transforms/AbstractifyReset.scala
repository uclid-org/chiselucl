// See LICENSE for license details.

package chiselucl
package transforms

import firrtl._
import firrtl.ir._
import firrtl.graph.DiGraph
import firrtl.options.Dependency
import firrtl.transforms.LogicNode
import firrtl.analyses.InstanceGraph
import firrtl.transforms.CheckCombLoops

object AbstractifyReset extends Transform with DependencyAPIMigration {

  override def prerequisites = firrtl.stage.Forms.MidForm ++
    Seq(Dependency(firrtl.passes.LowerTypes), Dependency(firrtl.passes.Legalize))

  override def optionalPrerequisites = Seq.empty

  override def optionalPrerequisiteOf = Seq(Dependency(firrtl.transforms.RemoveReset))

  override def invalidates(a: Transform): Boolean = a match {
    case t: firrtl.transforms.InferResets => true
    case _ => false
  }

  private def analyze(modResetInfo: Map[String, (DefModule, Set[String])])(stmt: Statement): Seq[LogicNode] = stmt match {
    case Block(stmts) => stmts.flatMap(analyze(modResetInfo))
    case Conditionally(_, _, cons, alt) => analyze(modResetInfo)(cons) ++: analyze(modResetInfo)(alt)
    case dr: DefRegister => Seq(LogicNode(dr.reset))
    case di: DefInstance => modResetInfo(di.module)._2.map(pName => LogicNode(inst = Some(di.name), name = pName)).toSeq
    case s => Nil
  }

  private def abstractifyReset(resetNetwork: Set[LogicNode])(stmt: Statement): Statement = stmt match {
    case decl: IsDeclaration if (resetNetwork(LogicNode(decl.name))) => decl.mapType(t => ResetType)
    case s => s.mapStmt(abstractifyReset(resetNetwork))
  }

  private def onModule(conn: DiGraph[LogicNode], modResetInfo: Map[String, (DefModule, Set[String])])(m: DefModule): DefModule = {
    val resetSinks = m match {
      case m: Module => analyze(modResetInfo)(m.body)
      case m => Nil
    }
    if (resetSinks.nonEmpty) {
      val undirLogic = conn + conn.reverse
      val resetNetwork = resetSinks.foldLeft(Set.empty[LogicNode]) {
        case (rNet, rSink) => if (rNet(rSink)) rNet else rNet ++ undirLogic.reachableFrom(rSink)
      }
      val mPrime = m.mapStmt(abstractifyReset(resetNetwork)).mapPort {
        p => if (resetNetwork(LogicNode(p.name))) p.mapType(t => ResetType) else p
      }
      mPrime
    } else {
      m
    }
  }

  def execute(state: CircuitState): CircuitState = {
    val c = state.circuit
    val connectivity = new CheckCombLoops().analyzeFull(state)
    val modMap = c.modules.map(m => m.name -> m).toMap
    val bottomUpMods = (new InstanceGraph(c)).graph.transformNodes(_.module).linearize.reverse
    val transformedModMap = bottomUpMods.foldLeft(Map.empty[String, (DefModule, Set[String])]) {
      case (partialResults, mName) =>
        val mPrime = onModule(connectivity(mName), partialResults)(modMap(mName))
        val resetPorts = mPrime.ports.collect { case Port(_, name, _, ResetType) => name }
        val result = (mName, (mPrime, resetPorts.toSet))
        partialResults + result
    }
    state.copy(circuit = c.copy(modules = c.modules.map(m => transformedModMap(m.name)._1)))
  }
}
