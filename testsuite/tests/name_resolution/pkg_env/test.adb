procedure Test is

   generic
      type Value_T is private;
   package Pkg is
      type Value_TT is array (0 .. 1) of Pkg.Value_T;
      --  When constructing equation for `Value_T`, `Pkg` env should also
      --  contain the env of the formal part of `Pkg`.
   end Pkg;

   package Inst is new Pkg (Value_T => Integer);

   procedure Proc (Values : Inst.Value_TT) is
      O : Integer := Values (0);
      pragma Test_Statement;
   begin
      null;
   end Proc;

begin
   null;
end Test;
