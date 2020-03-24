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

//TODO: Should we use named 
trait Name {
  def name: String
}

trait StateInfo {
  def nodes: Seq[DefNode]
  def wireDecls: Seq[DefWire]
  def instDecls: Seq[WDefInstance]
  def regResets: Set[String]
  def regDecls: Set[DefRegister]
  def memDecls: Set[DefMemory]
  def regAssigns: Seq[Connect]
  def combAssigns: Seq[Connect]
  def wireAssigns: Seq[Connect]
}

trait PropertyInfo {
  def properties: Seq[VerificationFormula]
}
  
trait ClockInfo {
  def clocks: Set[Expression]
  def clockSets: Option[Set[Set[Expression]]]
}




/**
  * Collect information on relevant module state and assertions
  * to be used when constructing a solver query.
  *
  */
case class ModuleInfoAnnotation(
  override val name: String,
  override val nodes: Seq[DefNode],
  override val wireDecls: Seq[DefWire],
  override val instDecls: Seq[WDefInstance],
  override val regResets: Set[String],
  override val regDecls: Set[DefRegister],
  override val memDecls: Set[DefMemory],
  override val regAssigns: Seq[Connect],
  override val combAssigns: Seq[Connect],
  override val wireAssigns: Seq[Connect],
  override val properties: Seq[VerificationFormula],
  override val clocks: Set[Expression],
  override val clockSets: Option[Set[Set[Expression]]]
) extends VerificationInfoAnnotation with Name 
                                     with StateInfo 
                                     with ClockInfo 
                                     with PropertyInfo


