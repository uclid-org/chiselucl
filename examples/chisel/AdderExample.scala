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

  val revert = io.sum - io.b

  io.sum := io.a + io.b
  Assume(io.a > 1.U, "a_bigger_than_one")
  Assert(io.sum > io.b, "output_bigger")
  Assert(revert  =/= 0.U, "nonsense")
}

object AdderModel extends App {
  UclidCompiler.generateModel(() => new Adder)
}
