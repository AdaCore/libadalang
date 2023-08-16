procedure Test is
   procedure Put (X : String) is null;
   procedure Put (X : Integer) is null;

   function Trim (Source : in String) return String is ("");
   procedure Trim (Source  : in out String) is null;

   procedure P (Y : String) is
   begin
      Put (if True then Y else Trim (Y));
      pragma Test_Statement;
      Put (if True then Trim (Y) else Y);
      pragma Test_Statement;
      Put (if True then Trim (Y) else Trim(Y));
      pragma Test_Statement;
      Put (if True then Trim (Y) elsif True then Trim (Y) else Trim(Y));
      pragma Test_Statement;
    end P;
begin
   null;
end Test;
