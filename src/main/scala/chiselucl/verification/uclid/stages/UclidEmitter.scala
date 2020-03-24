// See LICENSE for license details.

package chiselucl
package verification.uclid

import chiselucl.annotations._

import firrtl._
import firrtl.analyses._
import firrtl.ir._
import firrtl.passes._
import firrtl.transforms._


import java.io.Writer

class IndentLevel {
  var value: Int = 0
  def increase() = value += 2
  def decrease() = value -= 2
}

object UclidSerializer {
    
    def serializeModule(modInfo: ModuleInfoAnnotation)(implicit w : Writer): Unit = {

    }

    def serializeAllModules(circInfo: Seq[CircuitInfoAnnotation], modInfos: Seq[ModuleInfoAnnotation])(implicit w : Writer): Unit = {

    }
}

class UclidEmitter(val debugOutput: Boolean = false) extends Transform with Emitter {
  def inputForm = LowForm
  def outputForm = LowForm

  val outputSuffix = ".ucl"

  //TODO: Need to figure out the emission interface
  override def execute(state: CircuitState) = {
    val modInfoAnnos = state.annotations.filter(_.isInstanceOf[ModuleInfoAnnotation]).asInstanceOf[Seq[ModuleInfoAnnotation]]
    val circInfoAnnos = state.annotations.filter(_.isInstanceOf[CircuitInfoAnnotation]).asInstanceOf[Seq[CircuitInfoAnnotation]]
    val newAnnos = state.annotations.flatMap {
      case EmitCircuitAnnotation(_) =>
        val writer = new java.io.StringWriter
        UclidSerializer.serializeAllModules(circInfoAnnos, modInfoAnnos)(writer)
        Seq()

      case EmitAllModulesAnnotation(_) =>
        modInfoAnnos.map{ i =>
          val writer = new java.io.StringWriter
          UclidSerializer.serializeModule(i)(writer)
          Seq()
        }

      case _ => Seq()
    }
    state.copy(annotations = newAnnos ++ state.annotations)
  }
}
