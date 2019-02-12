separate (Pkg)
package body Inner is

   procedure Proc is
      Y : Integer := X;
      pragma Test_Statement;

      Inst : Kikou := (Lol => 12);
      pragma Test_Statement;

      Pouet : T;

      I : Float := Z;
      pragma Test_Statement;
   begin
      Pouet.Obj := 12;
      pragma Test_Statement;
   end Proc;

end Inner;

