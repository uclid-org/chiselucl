// See LICENSE 


package chiselucl
package analysis

import chiselucl.annotations.{CircuitInfoAnnotation, ModuleInfoAnnotation}

import firrtl._
import firrtl.ir._


class CollectCircuitInfo extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  override def execute(cs: CircuitState): CircuitState = {
    val circuitInfoAnno = Seq(CircuitInfoAnnotation(cs.circuit.main, new InstanceGraph(cs.circuit)))
    cs.copy(annotations = circuitInfoAnno ++ cs.annotations)
  }
}

    
