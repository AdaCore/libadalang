with Pkg;        use Pkg;
with Pkg.Subpkg; use Pkg.Subpkg;

procedure Test is

   procedure P (W : in out New_Window; X, Y : Natural) is
      procedure Ident (C : Color) is null;
      I : Integer;
   begin
      Ident (W (X, Y).C);
      pragma Test_Statement;

      Ident (W.CRef (X, Y).C);
      pragma Test_Statement;

      Ident (W ("ABCDEF").C);
      pragma Test_Statement;

      W (X, Y).C := R;
      pragma Test_Statement;

      W ("ABCDEF").C := G;
      pragma Test_Statement;

      I := W ("ABCDEF");
      pragma Test_Statement;
   end P;

begin
   null;
end Test;
