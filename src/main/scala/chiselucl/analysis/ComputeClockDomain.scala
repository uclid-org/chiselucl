// See LICENSE for license details.

package chiselucl
package analysis

import chiselucl.annotations._

import firrtl._
import firrtl.ir._
import firrtl.Utils._

import scala.collection.mutable.{LinkedHashMap, HashSet}

object ComputeClockDomain {

  
  // TODO: We have two possible approaches to computing equal clocks
  //    1. Change this implementation to use disjoint sets with a peek
  //    2. Analyze the actual collected clocks (this seems promising)
  //
  def addClockInfo(modInfo: ModuleInfoAnnotation): ModuleInfoAnnotation = {
    val eqClocks = LinkedHashMap[Expression, HashSet[Expression]]()

    for (node<-modInfo.nodes) {
      if (node.value.tpe == ClockType) {
        node.value match {
          case wr: WRef =>
            val clockSet = eqClocks.getOrElseUpdate(wr, new HashSet[Expression]())
            clockSet.add(WRef(node))
          case _ => throwInternalError(s"Cannot handle complex clock NodeDef")
        }
      }
    }

    // Join equal clock sets
    eqClocks.foreach({
      case (k1, set1) =>
        eqClocks.foreach({
          case (k2, set2) =>
            if (set1.contains(k2)) {
              eqClocks.put(k1, set1.union(set2))
              eqClocks.remove(k2)
            }
        })
    })

    val clockSets = new HashSet[Set[Expression]]()
    eqClocks.foreach({
      case (k, set) =>
        set += k
        clockSets += set.toSet
    })

    ModuleInfoAnnotation(
      modInfo.name,
      modInfo.nodes,
      modInfo.wireDecls,
      modInfo.instDecls,
      modInfo.regResets,
      modInfo.regDecls,
      modInfo.memDecls,
      modInfo.regAssigns,
      modInfo.combAssigns,
      modInfo.wireAssigns,
      modInfo.properties,
      modInfo.clocks,
      Some(clockSets.toSet)
    )
  }
}


class ComputeClockDomain extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val annos = state.annotations.map {
      case modInfo: ModuleInfoAnnotation => ComputeClockDomain.addClockInfo(modInfo)
      case other => other
    }
    state.copy(annotations = annos)
  }
}
