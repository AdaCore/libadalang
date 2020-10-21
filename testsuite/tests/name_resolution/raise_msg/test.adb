procedure Test is
   function Get (X : Integer) return String;
   function Get (X : Integer) return Integer;

   Failure : exception;
begin
   raise Failure with Get (2) & Get (3);
   pragma Test_Statement;

   declare
      Dummy : Integer := (raise Failure with Get (2) & Get (3));
      pragma Test_Statement;
   begin
      null;
   end;
end Test;
