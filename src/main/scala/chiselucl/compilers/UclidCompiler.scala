// See LICENSE for license details.

package chiselucl

import chisel3._
import firrtl._
import backend._

object UclidCompiler {
  def generateModel(gen: () => RawModule): Unit = {
    val optionsManager = new ExecutionOptionsManager("chisel3")
        with HasFirrtlOptions
        with HasChiselExecutionOptions {
      commonOptions = CommonOptions(targetDirName = "generated_models")
      firrtlOptions = FirrtlExecutionOptions(customTransforms = Seq(new UclidEmitter))
    }
    chisel3.Driver.execute(optionsManager, gen)
  }
}
