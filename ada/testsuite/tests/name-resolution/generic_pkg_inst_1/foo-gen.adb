package body Foo.Gen is
   procedure P (Self : T) is
      New_T : T;
   begin
      New_T := Self;
   end P;
end Foo.Gen;
