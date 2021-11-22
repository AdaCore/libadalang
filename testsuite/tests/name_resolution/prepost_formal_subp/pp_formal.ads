generic
   with function P (X, Y : in out Integer) return Integer
      with Pre => X mod 2 = 0, Post => X > Y;
   pragma Test_Block;
package PP_Formal is
   procedure Ren (X, Y : in out Integer);
end PP_Formal;
