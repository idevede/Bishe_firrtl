circuit Sub_simply : 
  module Sub_simply : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip fn : UInt<32>, flip in : UInt<32>, out : UInt<32>}
    node _T = gt(io.fn, UInt<4>("h08")) 
    when _T :
      node _T_1 = xor(io.in, UInt<3>("h07")) 
      io.out <= _T_1 
      skip 
    else : 
      node _T_3 = xor(io.in, UInt<3>("h06")) 
      io.out <= _T_3 
      skip 
  