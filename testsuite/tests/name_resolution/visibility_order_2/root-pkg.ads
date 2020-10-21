with Foo;
with Foo2;

package Root.Pkg is
   type X is private;
   package Nested is
      procedure Proc;
      use Foo;
   end Nested;
private
   type X is record
      Y, Z : Integer;
   end record;
end Root.Pkg;
