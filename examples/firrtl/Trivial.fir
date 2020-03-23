circuit Trivial :
  module Trivial : 
    input clock : Clock
    input reset : UInt<1>
    
    input init : SInt<1>
    reg myreg : SInt<1> , clock with : ( reset => ( reset , init ) )
