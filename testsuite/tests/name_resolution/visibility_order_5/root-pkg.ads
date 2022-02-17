with Foo;
with Foo2;

package Root.Pkg is
   type X is private;
   protected Nested is
      procedure Proc;
   end Nested;
   use Foo;
private
   type X is record
      Y, Z : Integer;
   end record;
end Root.Pkg;
