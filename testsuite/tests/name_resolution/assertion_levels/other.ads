with Pkg;

package Other is
   procedure Foo
      with Post => (L1 => True, L2 => True);
   pragma Test_Block;
private

   procedure Foo is null;
end Other;
