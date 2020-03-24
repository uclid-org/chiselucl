// See LICENSE for license details.


package chiselucl
package analysis

import chiselucl.annotations.CircuitInfoAnnotation

import firrtl._
import firrtl.analyses.InstanceGraph


class CollectCircuitInfo extends Transform {
  def inputForm = LowForm
  def outputForm = LowForm

  override def execute(cs: CircuitState): CircuitState = {
    val circuitInfoAnno = Seq(CircuitInfoAnnotation(cs.circuit.main, new InstanceGraph(cs.circuit)))
    cs.copy(annotations = circuitInfoAnno ++ cs.annotations)
  }
}

    
