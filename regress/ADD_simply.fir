circuit ADD_simply : 
  module ADD_simply : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip fn : UInt<32>, flip in : UInt<32>, out : UInt<32>}
    node _T = gt(io.fn, UInt<4>("h08")) 
    when _T :
      node _T_1 = add(io.in, UInt<32>("h07")) 
      node _T_2 = tail(_T_1, 1) 
      io.out <= _T_2 
      skip 
    else : 
      node _T_3 = add(io.in, UInt<32>("h06")) 
      node _T_4 = tail(_T_3, 1) 
      io.out <= _T_4 
      skip 
  