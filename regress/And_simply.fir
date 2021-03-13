circuit And_simply : 
  module And_simply : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip fn : UInt<32>, flip in : UInt<32>, out : UInt<32>}
    node _T_1 = not(io.fn)
    node _T_2 = not(io.in)
    io.out <= and(_T_1,_T_2)
    
  