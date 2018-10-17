with A;

package body B is
   procedure Foo is
      Y : Integer := A.X;
   begin
      return Y + X;
   end Foo;
end B;