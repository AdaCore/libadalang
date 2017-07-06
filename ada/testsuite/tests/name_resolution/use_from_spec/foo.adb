pragma Config (Display_Slocs => True);

function Foo (C : Content) return Integer is
   procedure Helper (C : Content) is
      C_I : Integer := C.I;
      pragma Test_Statement;
   begin
      null;
   end Helper;
begin
   Helper (C);
   return 1;
end Foo;
