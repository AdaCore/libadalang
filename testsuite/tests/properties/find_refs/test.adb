procedure Test is
   procedure P (X : Integer);
   procedure P (X : Integer) is
      Y : Integer := X + 2;
   begin
      null;
   end P;

   procedure Q (X : Integer) is
      Y : Integer := X + 2;
   begin
      null;
   end Q;
begin
   null;
end Test;
