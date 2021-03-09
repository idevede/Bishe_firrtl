; SPDX-License-Identifier: Apache-2.0
circuit Ops_Small:
  module Ops_Small:
    input sel: UInt<4>
    input is: UInt<8>
    output os: UInt<8>
    os <= UInt(0)
    when eq(sel, UInt(0)):
      os <= add(is,UInt(1))
    else:
      when eq(sel, UInt(1)):
        os <= sub(is, UInt(2))
      else:
          os <= add(is,UInt(3))
        