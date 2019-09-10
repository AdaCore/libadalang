separate (Pkg)
package body Inner is

   A : Integer;

   procedure Proc is
      Y : Integer := Pkg.Inner.A;
      pragma Test_Statement;
   begin
      null;
   end Proc;

end Inner;

