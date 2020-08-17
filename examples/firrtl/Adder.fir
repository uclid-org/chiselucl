;buildInfoPackage: chisel3, version: 3.2.0-RC1, scalaVersion: 2.12.9, sbtVersion: 1.2.7
circuit Adder : 
  module Adder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<4>, flip b : UInt<4>, sum : UInt<4>}
    
    node _T = add(io.a, io.b) @[AdderExample.scala 14:18]
    node _T_1 = tail(_T, 1) @[AdderExample.scala 14:18]
    io.sum <= _T_1 @[AdderExample.scala 14:10]
    node a_bigger_than_one = gt(io.a, UInt<1>("h01")) @[AdderExample.scala 15:15]
    node output_bigger = gt(io.sum, io.b) @[AdderExample.scala 16:17]
    
