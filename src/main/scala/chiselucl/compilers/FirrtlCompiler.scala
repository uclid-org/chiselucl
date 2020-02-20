// See LICENSE for license details.

package chiselucl

import firrtl._
import firrtl.stage.{FirrtlStage, FirrtlCircuitAnnotation, FirrtlSourceAnnotation}

import backend._
import limeutil._


object FirrtlCompiler {

  def generateModel(firrtlString : String, channelTargetDir : String) : Seq[Option[EmittedCircuit]] = {
  
    //TODO: Figure out which is the correct one to do
    //val repr = toLowFirrtl(firrtlString) 
    //val repr = toLowFirrtl(firrtlString) 
    val repr = toMVFirrtl(firrtlString)

    /** Add annotations for UclidEmitter and EmitChannelInfo */
    val annotations = AnnotationSeq.apply(Seq(
                        new EmitCircuitAnnotation(classOf[LowFirrtlEmitter]),
                        TargetDirAnnotation(channelTargetDir)
                      ))
    val annotCircuitState = CircuitState(repr.circuit,
                                         repr.form,
                                         annotations,
                                         repr.renames)
    val uclidEmitter = new UclidEmitter
    val transforms = uclidEmitter.transforms ++ 
                      Seq(uclidEmitter, new EmitChannelInfo)

  
    
    val uclidModel = transforms.foldLeft(annotCircuitState) {
      (c : CircuitState, t : Transform) => t.runTransform(c)
    }

    uclidModel.annotations.map {
      case EmittedVerilogCircuitAnnotation(c) => Some(c)
      case EmittedFirrtlCircuitAnnotation(c) => Some(c)
      case _ => None
    }
  }

  /**
   * Converts a FIRRTL string to a LoFIRRTL circuit
   *
   * @param string String containing the contents of a FIRRTL file
   * @return A CircuitState object containing equivalent LoFIRRTL of `string`
   */
  private def toLowFirrtl(string: String): CircuitState = {
    (new FirrtlStage)
      .execute(Array("-X", "low"), Seq(FirrtlSourceAnnotation(string)))
      .collectFirst{ case FirrtlCircuitAnnotation(a) => a }
      .map(a => firrtl.CircuitState(a, firrtl.UnknownForm))
      .get
  }


  /**
   * Converts a FIRRTL string to a minimum Verilog LoFIRRTL circuit
   *
   * @param string String containing the contents of a FIRRTL file
   * @return A CircuitState object containing equivalent LoFIRRTL of `string`
   */
  private def toMVFirrtl(string: String): CircuitState = {
    (new FirrtlStage)
      .execute(Array("-X", "mverilog"), Seq(FirrtlSourceAnnotation(string)))
      .collectFirst{ case FirrtlCircuitAnnotation(a) => a }
      .map(a => firrtl.CircuitState(a, firrtl.UnknownForm))
      .get
  }

}
