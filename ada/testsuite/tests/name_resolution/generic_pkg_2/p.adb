package body P is
   procedure Foo (Self : T) is
      Inst : U;
   begin
      Inst := Convert (Self);
      pragma Test_Statement;
   end Foo;
end P;
