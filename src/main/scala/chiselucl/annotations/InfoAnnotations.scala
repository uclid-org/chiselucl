// See LICESNSE for license details.

package chiselucl
package annotations

// Local imports
import chiselucl.verification.lang.VerificationFormula

// Other imports 

import firrtl._
import firrtl.ir._
import firrtl.analyses.InstanceGraph
import firrtl.annotations.NoTargetAnnotation

/**
  * Base trait for annotations that contain information on 
  * components of the circuit. Extend this to pass information 
  * to the verification stages.
  */
trait VerificationInfoAnnotation extends NoTargetAnnotation 

/**
  * Contains information on circuit structure to be used when 
  * constructing a solver query.
  *
  */
case class CircuitInfoAnnotation(
  name: String, 
  instGraph: InstanceGraph
) extends VerificationInfoAnnotation {

  /**
    * Returns module names from highest module to leaf module.
    */
  def getModuleNames: Seq[String] = instGraph.moduleOrder.map(_.name)

}

/**
  * Collect information on relevant module state and assertions
  * to be used when constructing a solver query.
  *
  */
case class ModuleInfoAnnotation(
  name: String,
  nodes: Seq[DefNode],
  wireDecls: Seq[DefWire],
  instDecls: Seq[WDefInstance],
  clocks: Set[Expression],
  regResets: Set[String],
  regDecls: Set[DefRegister],
  memDecls: Set[DefMemory],
  regAssigns: Seq[Connect],
  combAssigns: Seq[Connect],
  wireAssigns: Seq[Connect],
  properties: Seq[VerificationFormula]
) extends VerificationInfoAnnotation 
