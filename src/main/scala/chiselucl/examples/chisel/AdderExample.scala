package chiselucl
package examples

import chisel3._
import uclid._

class Adder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(4.W))
    val b = Input(UInt(4.W))
    val sum = Output(UInt(4.W))
  })

  io.sum := io.a + io.b
  Assume(io.a > 1.U, "a_bigger_than_one")
  Assert(io.sum > io.b, "output_bigger")
}

object AdderModel extends App {
  UclidCompiler.generateModel(() => new Adder)
}
