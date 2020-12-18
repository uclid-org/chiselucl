// See LICENSE for license details.

package chiselucl

import scala.io.Source
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

/*
 * Executable that runs FirrtlCompiler on multiple files
 *
 * @input args Single FIRRTL file.
 */
@deprecated("LIME-specific code", "0.3")
object FirrtlToUclid extends App {
  if (args.length == 0) {
    println("Need to supply FIRRTL files as input.")
  } else {
    println("Generating UCLID5 models.")
    //TODO: May need to fix the channel_info argument
    //TODO: Parse a directory of FIRRTL files
    args.map(f => {
      val firrtl = FirrtlFileUtils.read(f)
      val emittedCircuits = chiselucl.FirrtlCompiler.generateModel(firrtl, "./channel_info/")
      emittedCircuits.foreach {
        circ => FirrtlFileUtils.write(s"./generated_models/${circ.name}${circ.outputSuffix}", circ.value)
      }
    })
  }
}

object FirrtlFileUtils {

  /**
   * Reads in a single FIRRTL file as a string
   *
   * @param filename Name of the FIRRTL file to be read in
   * @return A string with the contents of `filename`
   */
  def read(fileName: String) : String = {
    val bufferedSource = Source.fromFile(fileName)
    val fileContents = bufferedSource.mkString
    bufferedSource.close
    fileContents
  }

  def write(fileName: String, contents: String) : Unit = {
    val file = new File(fileName)
    file.delete()
    file.createNewFile()
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(contents)
    bw.close()
  }

}
