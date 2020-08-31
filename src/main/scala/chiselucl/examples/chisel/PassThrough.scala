// See LICENSE for license details.

package chiselucl
package examples

import chisel3._
import chisel3.util.{Irrevocable, Queue}

import chiselucl.control._
import chiselucl.properties._
import chiselucl.properties.ltlsyntax._

class PassThroughInner extends MultiIOModule {
  val i = IO(Input(UInt(4.W)))
  val o = IO(Output(UInt(4.W)))
  val x = ~i
  o := ~x
}

class PassThroughExample extends MultiIOModule {
  val i = IO(Input(UInt(4.W)))
  val o = IO(Output(UInt(4.W)))

  val inner = Module(new PassThroughInner)

  inner.i := i
  o := inner.o

  // Should not need reset gating, but hierarchical init strategy needs work...
  LTL(G(reset.toBool || o === i), "passthrough")
  BMC(5)
}

object PassThroughExampleModel extends App {
  UclidCompiler.generateModel(() => new PassThroughExample)
}
