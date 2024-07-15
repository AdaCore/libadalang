with System;

procedure Test (A : System.Address) is
   I : Integer := Integer'Deref(A);
   pragma Test_Statement;

   B : Boolean := Integer'Deref(A) = 2;
   pragma Test_Statement;
begin
   null;
end;
