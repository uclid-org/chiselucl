package chiselucl
package examples

import chisel3._
import chisel3.util.{Irrevocable, QueueIO, Queue, Counter}

import chiselucl.control._
import chiselucl.properties.ltlsyntax._

class BadQueue[T <: Data](gen: T, val entries: Int) extends Module {
  val io = IO(new QueueIO(gen, entries))
  require(entries > 0)

  val ram = Mem(entries, gen)

  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)

  when (io.enq.fire) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }

  when (io.deq.fire) {
    deq_ptr.inc()
  }

  io.count := deq_ptr.value - enq_ptr.value

  io.deq.valid := enq_ptr.value =/= deq_ptr.value
  io.enq.ready := true.B
  io.deq.bits := ram(deq_ptr.value)
}

class IrrevocableExample(correct: Boolean) extends MultiIOModule {
  val i = IO(Flipped(Irrevocable(UInt(4.W))))
  val o = IO(Irrevocable(UInt(4.W)))

  val q = if (correct) Module(new Queue(UInt(4.W), 4)) else Module(new BadQueue(UInt(4.W), 4))

  q.io.enq.valid := i.valid
  q.io.enq.bits := i.bits
  i.ready := q.io.enq.ready

  o.valid := q.io.deq.valid
  o.bits := q.io.deq.bits
  q.io.deq.ready := o.ready

  LTL(G(AP(o.valid && !o.ready && !reset.toBool) ==> X(o.valid)), "output_irrevocable")
  BMC(5)
}

class CorrectIrrevocableExample extends IrrevocableExample(true)
class BrokenIrrevocableExample extends IrrevocableExample(false)


object CorrectIrrevocableExampleModel extends App {
  UclidCompiler.generateModel(() => new CorrectIrrevocableExample)
}

object BrokenIrrevocableExampleModel extends App {
  UclidCompiler.generateModel(() => new BrokenIrrevocableExample)
}
