circuit AAB_reduce : 
  module AAB_reduce : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip fn : UInt<32>, flip in : UInt<32>, out : UInt<32>}
    node _T_1 = not(io.fn)
    node _T_2 = mul(_T_1, io.in)
    io.out <= add(io.fn,_T_2)
    