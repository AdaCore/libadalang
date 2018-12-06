package body D is
   procedure Pouet (Self : in out Foo; R : C.Root_Type) is
   begin
      C.Visit (R, Self);
      pragma Test_Statement;
   end Pouet;
end D;
