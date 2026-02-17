with Pkg;

package Other is
   procedure Foo
      with Post => (L1 => True, L2 => True);
   pragma Test_Block;

   procedure Bar
      with Post => (L3 => True);
   pragma Test_Block;
private

   procedure Foo is null;
   procedure Bar is null;
end Other;
