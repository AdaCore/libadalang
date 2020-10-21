with G;
with G_II;
with P;

procedure Test is
   package P_I is new P (G_II);

   X : Integer := P_I.Foo (X => 2);
   pragma Test_Statement;
begin
   null;
end Test;
