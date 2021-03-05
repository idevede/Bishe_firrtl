;buildInfoPackage: chisel3, version: 3.2.0, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit ALU_simplify : 
  module ALU_simplify : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip fn : UInt<2>, flip in : UInt<32>, out : UInt<32>}
    
    wire result : UInt<32> @[cmd24.sc 10:20]
    result <= UInt<1>("h01") @[cmd24.sc 11:10]
    node _T = eq(UInt<2>("h03"), io.fn) @[Conditional.scala 37:30]
    when _T : @[Conditional.scala 40:58]
      result <= UInt<2>("h03") @[cmd24.sc 14:22]
      skip @[Conditional.scala 40:58]
    else : @[Conditional.scala 39:67]
      node _T_1 = eq(UInt<2>("h02"), io.fn) @[Conditional.scala 37:30]
      when _T_1 : @[Conditional.scala 39:67]
        result <= UInt<2>("h02") @[cmd24.sc 15:22]
        skip @[Conditional.scala 39:67]
      else : @[Conditional.scala 39:67]
        node _T_2 = eq(UInt<1>("h01"), io.fn) @[Conditional.scala 37:30]
        when _T_2 : @[Conditional.scala 39:67]
          result <= UInt<1>("h01") @[cmd24.sc 16:22]
          skip @[Conditional.scala 39:67]
        else : @[Conditional.scala 39:67]
          node _T_3 = eq(UInt<1>("h00"), io.fn) @[Conditional.scala 37:30]
          when _T_3 : @[Conditional.scala 39:67]
            result <= UInt<1>("h00") @[cmd24.sc 17:22]
            skip @[Conditional.scala 39:67]
    node _T_4 = add(result, io.in) @[cmd24.sc 21:19]
    node _T_5 = tail(_T_4, 1) @[cmd24.sc 21:19]
    io.out <= _T_5 @[cmd24.sc 21:10]