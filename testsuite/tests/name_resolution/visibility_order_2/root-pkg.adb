package body Root.Pkg is

   package body Nested is
      --  Here we want to test that X resolves to the proper type X
      procedure Proc is
         Inst : X;
      begin
         Inst.Y := 12;
         pragma Test_Statement;
      end Proc;
   end Nested;

end Root.Pkg;
