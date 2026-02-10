pragma Assertion_Level (L1);
pragma Assertion_Level (L2);
package Pkg is
   L1 : Integer := 2;

   procedure Bar
      with Post => (L1 => L1 > 1);
   pragma Test_Block;

private
   procedure Bar is null;
end Pkg;
