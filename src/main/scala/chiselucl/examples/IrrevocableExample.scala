// See LICENSE for license details.

package chiselucl
package examples

import chisel3._
import chisel3.util.{Irrevocable, QueueIO, Counter}

import chiselucl.control._
import chiselucl.properties._
import chiselucl.properties.ltlsyntax._

class BaseQueue[T <: UInt](gen: T, val entries: Int) extends Module {
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

  val ptr_match = enq_ptr.value === deq_ptr.value
  val maybe_full = RegInit(false.B)
  when (io.enq.fire =/= io.deq.fire) {
    maybe_full := io.enq.fire
  }

  val ptr_diff = enq_ptr.value - deq_ptr.value
  if (BigInt(entries).bitCount == 1) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(ptr_match,
      Mux(maybe_full,
        entries.asUInt, 0.U),
      Mux(deq_ptr.value > enq_ptr.value,
        entries.asUInt + ptr_diff, ptr_diff))
  }

  io.deq.valid := !ptr_match || maybe_full
  io.enq.ready := !ptr_match || !maybe_full
  io.deq.bits := ram(deq_ptr.value)

  Assume.initialReset(reset)

  // Basic handshaking properties
  val deq_has_k = io.deq.valid && io.deq.bits === FreeConstant(gen)
  LTL(G(AP(deq_has_k && !io.deq.ready && !reset.toBool) ==> X(deq_has_k)), "output_irrevocable")
  LTL(G(AP(io.enq.ready && !io.enq.valid && !reset.toBool) ==> X(io.enq.ready)), "input_stays_ready")
  LTL(G(AP(io.deq.ready) ==> F(io.enq.ready)), "no_spurious_backpressure")

  // Enqueued data is eventually output
  val kprime = FreeConstant(gen)
  val kprime_enqueued = AP(io.enq.fire && io.enq.bits === kprime)
  val kprime_output_unless_reset = AP(reset.toBool || (io.deq.valid && io.deq.bits === kprime))
  LTL(G(F(io.deq.ready)) ==> G(kprime_enqueued ==> F(kprime_output_unless_reset)), "input_eventually_output")

  BMC(10)
}

class CorrectQueue extends BaseQueue(UInt(4.W), 4)

class MagicOutput extends BaseQueue(UInt(4.W), 4) {
  val magic_token_at_output = ram(deq_ptr.value) === 6.U
}

class DropsOutput extends MagicOutput {
  // Drop magic token after one cycle at output
  when (io.deq.fire || magic_token_at_output) {
     deq_ptr.inc()
  }
}

class SwapsOutput extends MagicOutput {
  when (io.deq.valid && !io.deq.ready && magic_token_at_output) {
    io.deq.bits := ram(deq_ptr.value) + 1.U
  }
}

class SwapsOutputAfterOne extends MagicOutput {
  val magic_token_presented = RegInit(false.B)
  when (io.deq.valid && !io.deq.ready && magic_token_at_output) {
    magic_token_presented := true.B
  } .elsewhen(io.deq.ready) {
    magic_token_presented := false.B
  }

  when (magic_token_presented) {
    io.deq.bits := ram(deq_ptr.value) + 1.U
  }
}

class TogglesReady extends BaseQueue(UInt(4.W), 4) {
  val ready_gate = RegInit(false.B)
  io.enq.ready := ready_gate && (!ptr_match || !maybe_full)
  io.enq.bits match {
    case u: UInt =>
      when (u === 7.U) { ready_gate := !ready_gate }
      .otherwise { ready_gate := false.B }
    case _ => false.B
  }
}

object CorrectQueueModel extends App {
  UclidCompiler.generateModel(() => new CorrectQueue)
}

object DropsOutputModel extends App {
  UclidCompiler.generateModel(() => new DropsOutput)
}

object SwapsOutputModel extends App {
  UclidCompiler.generateModel(() => new SwapsOutput)
}

object SwapsOutputAfterOneModel extends App {
  UclidCompiler.generateModel(() => new SwapsOutputAfterOne)
}

object TogglesReadyModel extends App {
  UclidCompiler.generateModel(() => new TogglesReady)
}
