procedure Test is
   procedure Put_Line (I : Integer) is null;

   A, B : Integer := 1;
begin
   Put_Line (A);
end;
