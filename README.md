# ChiselUCL

[Chisel](https://www.github.com/freechipsproject/chisel3) utilities for modeling and a [FIRRTL](https://www.github.com/freechipsproject/firrtl) backend for the [UCLID5](https://www.github.com/uclid-org/uclid) modeling system.

Contributors:
- Albert Magyar
- Pranav Gaddamadugu

Note: This repo is under active development.

## Example: using the FIRRTL translator
The following commands need to be run to configure the repository
```
mkdir channel_info
mkdir generated_models
```
To run the translator (from top level directory), execute the following
```
sbt "runMain chiselucl.FirrtlToUclid $PATH_TO_FIRRTL_FILE"
```


## Example: adding an assumption to an adder

To execute a simple model of an adder with basic verification directives, run the following commands. Note that UCLID5 must be installed on your system and visible on your path.
```
sbt "runMain chiselucl.examples.AdderModel"
uclid generated_models/Adder.ucl
```

The emitted model is defined by the following Chisel source, found in `examples/AdderExample.scala`.

```
 class Adder extends Module {
   val io = IO(new Bundle {
     val a = Input(UInt(4.W))
     val b = Input(UInt(4.W))
     val sum = Output(UInt(4.W))
   })

   io.sum := io.a + io.b
   Assume(io.a > 1.U, "a_bigger_than_one")
 }
 ```

## Source directory structure

`src/main/scala/chiselucl/`

* `apps/` contains driver application for FIRRTL translator

* `compilers/` contains the main generators to produce UCLID5 from Chisel and FIRRTL

* `backend/` contains FIRRTL transforms related to the UCLID5 backend

* `examples/` contains basic templates for some use cases of ChiselUCL

* `limeutil/` contains utilities for [checking optimized FPGA simulation models](https://people.eecs.berkeley.edu/~magyar/papers/golden-gate.pdf)

* `uclid/` contains the API for inserting UCLID5 assumptions, properties, directives into a module. These are emitted as part of the UCLID5 collateral for the module.

* `util/` contains infrastructure used in both the Chisel and FIRRTL layers
