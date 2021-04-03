circuit ADD_simply_after: 
  module ADD_simply_after : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip fn : UInt<32>, flip in : UInt<32>, out : UInt<32>}
    node _T = gt(io.fn, UInt<4>("h08")) 
    wire _T_1 : UInt<3>
    
    when _T :
      _T_1 <= UInt<3>("h07") 
      
      
      skip 
    else : 
      _T_1 <= UInt<3>("h06")
       
      
      skip
    node _T_3 = add(io.in,_T_1)
    io.out <= tail(_T_3, 1)
  