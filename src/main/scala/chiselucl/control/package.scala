// See LICENSE for license details.

package chiselucl

import chisel3._
import chisel3.experimental.{ChiselAnnotation, annotate}

import annotations._

package object control {

  object BMC {
    def apply(steps: BigInt): Unit = {
      annotate(new ChiselAnnotation { def toFirrtl = BMCAnnotation(steps) })
    }
  }

}
