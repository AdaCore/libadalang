separate (Test_Pkg) package body Foo is
   procedure Bar is
      Inst : A;
      Dummy : Integer :=

      --  Check that we have visibility on subprograms defined in the body of
      --  Test_Pkg from this separate.
      Inst.
      --% list(node.p_complete)
         ;
   begin
      null;
   end Bar;
end Foo;
