// See LICENSE for license details.

package chiselucl
package examples

import chisel3._
import chisel3.util.{DecoupledIO, Queue}

import chiselucl.properties.ltlsyntax._

class BadMerge extends MultiIOModule {
  val a =IO(Flipped(DecoupledIO(UInt(4.W))))
  val b = IO(Flipped(DecoupledIO(UInt(4.W))))
  val x = IO(DecoupledIO(UInt(4.W)))

  val q = Module(new Queue(UInt(4.W), 4))

  q.io.enq.valid := a.valid && b.valid
  q.io.enq.bits := a.bits + b.bits
  a.ready := q.io.enq.ready
  b.ready := q.io.enq.ready

  x <> q.io.deq
  LTL(AP(a.ready && !a.valid) ==> X(a.ready), "a_irrevocable_ready")
  LTL(AP(b.ready && !b.valid) ==> X(b.ready), "b_irrevocable_ready")
}

object BadMergeModel extends App {
  UclidCompiler.generateModel(() => new BadMerge)
}
