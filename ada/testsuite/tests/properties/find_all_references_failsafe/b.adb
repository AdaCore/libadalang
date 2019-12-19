with A;

package body B is
   procedure Foo is
      Y : Integer := A.X;
      pragma Find_All_References (Any, Imprecise_Fallback => True);
   begin
      return Y + X;
   end Foo;
end B;
