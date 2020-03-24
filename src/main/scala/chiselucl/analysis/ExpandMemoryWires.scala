// See LICENSE for license details.

package chiselucl
package analysis

import chiselucl.annotations._

import firrtl._
import firrtl.ir._
import firrtl.passes._
import MemPortUtils.{memPortField}


import scala.collection.mutable.ListBuffer

object ExpandMemoryWires {
  
  def expandWires(modInfo : ModuleInfoAnnotation) : ModuleInfoAnnotation = {
    val newWires = ListBuffer[DefWire]()

    for (decl <- modInfo.memDecls) {
      decl match {
        case sx @ DefMemory(_, n, dt, _, wlat, rlat, rs, ws, rws, _) =>
          newWires += DefWire(NoInfo, s"havoc_$n", dt)
          for (r <- rs) {
            val data = memPortField(sx, r, "data")
            val addr = memPortField(sx, r, "addr")
            val en = memPortField(sx, r, "en")
            newWires += DefWire(NoInfo, LowerTypes.loweredName(data), data.tpe)
            newWires += DefWire(NoInfo, LowerTypes.loweredName(addr), addr.tpe)
            newWires += DefWire(NoInfo, LowerTypes.loweredName(en), en.tpe)
          }
          for (w <- ws) {
            val data = memPortField(sx, w, "data")
            val addr = memPortField(sx, w, "addr")
            val en = memPortField(sx, w, "en")
            val mask = memPortField(sx, w, "mask")
            newWires += DefWire(NoInfo, LowerTypes.loweredName(data), data.tpe)
            newWires += DefWire(NoInfo, LowerTypes.loweredName(addr), addr.tpe)
            newWires += DefWire(NoInfo, LowerTypes.loweredName(en), en.tpe)
            newWires += DefWire(NoInfo, LowerTypes.loweredName(mask), mask.tpe)
          }
        case _ => // Do nothing
      }
    }
    
    ModuleInfoAnnotation(
      modInfo.name,
      modInfo.nodes,
      newWires.toSeq ++ modInfo.wireDecls,
      modInfo.instDecls,
      modInfo.clocks,
      modInfo.regResets,
      modInfo.regDecls,
      modInfo.memDecls,
      modInfo.regAssigns,
      modInfo.combAssigns,
      modInfo.wireAssigns,
      modInfo.properties
    )
  }

}


class ExpandMemoryWires extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm 

  def execute(state: CircuitState): CircuitState = {
    val annos = state.annotations.map {
      case modInfo: ModuleInfoAnnotation => ExpandMemoryWires.expandWires(modInfo)
      case other => other 
    }
    state.copy(annotations = annos)
  }
}
