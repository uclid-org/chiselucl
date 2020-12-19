// See LICENSE for license details.

package chiselucl
package examples

import chisel3._
import chisel3.util.{Decoupled, Counter}

import chiselucl.control._
import chiselucl.properties._
import chiselucl.properties.ltlsyntax._

class SimpleQueue(width: Int, entries: Int) extends MultiIOModule {
  val i = IO(Flipped(Decoupled(UInt(width.W))))
  val o = IO(Decoupled(UInt(width.W)))

  require(entries > 0, "Queue must have a positive number of entries")

  val ram = Mem(entries, UInt(width.W))

  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val ptr_match = enq_ptr.value === deq_ptr.value

  val maybe_full = RegInit(false.B)

  i.ready := !ptr_match || !maybe_full
  o.valid := !ptr_match || maybe_full
  o.bits := ram(deq_ptr.value)

  when (i.fire) {
    ram(enq_ptr.value) := i.bits
    enq_ptr.inc()
    when (!o.fire) {
      maybe_full := true.B
    }
  }

  when (o.fire) {
    deq_ptr.inc()
    when (!i.fire) {
      maybe_full := false.B
    }
  }
}

class UnitQueue(width: Int) extends MultiIOModule {
  val i = IO(Flipped(Decoupled(UInt(width.W))))
  val o = IO(Decoupled(UInt(width.W)))

  val data = Reg(UInt(width.W))
  val full = RegInit(false.B)

  i.ready := !full
  o.valid := full
  o.bits := data

  when (i.fire) { full := true.B }
  when (o.fire) { full := false.B }
}

