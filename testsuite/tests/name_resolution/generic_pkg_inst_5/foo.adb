with Opt;

procedure Foo is
   package Int1_Opt is new Opt (Integer);
   package Int2_Opt is new Opt (Integer);

   X : Int1_Opt.Opt_Type;
begin
   X := Int2_Opt.Create;
   pragma Test_Statement (Expect_Fail => True);
end Foo;
