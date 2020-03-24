// See LICESNSE for license details.

package chiselucl
package analysis

import chiselucl.annotations._
import chiselucl.verification.lang._

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

import scala.collection.mutable.{ListBuffer, HashSet}

object CollectModuleInfo {

  def createModuleInfoAnnotation(m: Module): ModuleInfoAnnotation = {
    val nodes = ListBuffer[DefNode]()
    val wireDecls = ListBuffer[DefWire]()
    val instDecls = ListBuffer[WDefInstance]()
    val clocks = HashSet[Expression]()
    val regResets = HashSet[String]()
    val regDecls = HashSet[DefRegister]()
    val memDecls = HashSet[DefMemory]()
    val regAssigns = ListBuffer[Connect]()
    val combAssigns = ListBuffer[Connect]()
    val wireAssigns = ListBuffer[Connect]()
    val properties = ListBuffer[VerificationFormula]()

    //TODO: Need to revisit this correctness of the collection
    def processStatements(s: Statement): Statement = s map processStatements match {
      case sx: DefNode => 
        nodes += sx
        sx
      case sx: DefRegister => 
        clocks += sx.clock
        sx.reset match {
          case wr: WRef =>
            regResets += wr.name
          case UIntLiteral(v: BigInt, _) if (v == 0) =>
          case _ => 
            throwInternalError(s"Illegal reset signal ${sx.reset}")
          }
        regDecls += sx
        sx
      case sx @ Connect(_, lhs, rhs) => kind(lhs) match {
        case RegKind => 
          regAssigns += sx
        case PortKind => 
          combAssigns += sx
        case MemKind => rhs.tpe match {
          case ClockType => 
            clocks += rhs
          case _ => 
            combAssigns += sx
          }
        case InstanceKind => lhs match {
          case WSubField(WRef(instName,_,_,_), field, tpe, flow) =>
            combAssigns += sx
          case _ => 
            throwInternalError(s"Only subfields of an instance may be on the lhs of a Connect involving an instance")
          }
        case _ =>
          throwInternalError(s"Only outputs, registers, mem fields, and inst subfields may be on the lhs of a Connect")
        }
        sx
      case sx @ DefMemory(_, n, dt, _, wlat, rlat, rs, ws, rws, _) => 
        require(wlat == 1 && rlat == 0 && rws.size == 0, "This pass must run after VerilogMemDelays!")
        require(dt.isInstanceOf[GroundType], "This pass must run after LowerTypes!")
        memDecls += sx
        sx
      case sx @ WDefInstance(_,name,module,_) => 
        instDecls += sx
        sx
      case DefWire(_,_,_) =>
        // These are illegal for now
        throw EmitterException("Using illegal statement!")
      case sx =>
        sx
    }

    processStatements(m.body)

    ModuleInfoAnnotation(
      m.name,
      nodes.toSeq,
      wireDecls.toSeq,
      instDecls.toSeq,
      clocks.toSet,
      regResets.toSet,
      regDecls.toSet,
      memDecls.toSet,
      regAssigns.toSeq,
      combAssigns.toSeq,
      wireAssigns.toSeq,
      properties.toSeq
    )
  }
}


class CollectModuleInfo extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  def execute(state: CircuitState): CircuitState = {
    val moduleInfoAnnos = state.circuit.modules.flatMap { 
      case mod: Module => Some(CollectModuleInfo.createModuleInfoAnnotation(mod))
      case extMod: ExtModule => None
    }
    state.copy(annotations = moduleInfoAnnos ++ state.annotations)
  }
}
