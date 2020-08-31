// See LICENSE for license details.

package chiselucl

import firrtl._
import firrtl.stage._

import backend._
import limeutil._


object FirrtlCompiler {

  def generateModel(firrtlString : String, channelTargetDir : String) : Seq[EmittedCircuit] = {

    val circuit = toLowFirrtl(firrtlString).circuit

    /** Add annotations for UclidEmitter and EmitChannelInfo */
    val annotations = Seq(
      FirrtlCircuitAnnotation(circuit),
      EmitCircuitAnnotation(classOf[LowFirrtlEmitter]),
      TargetDirAnnotation(channelTargetDir),
      RunFirrtlTransformAnnotation(new UclidEmitter)
    )

    val compiled = (new FirrtlStage).run(annotations)

    compiled.collect {
      case EmittedVerilogCircuitAnnotation(c) => c
      case EmittedFirrtlCircuitAnnotation(c) => c
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
