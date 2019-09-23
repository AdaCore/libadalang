procedure Test is
   package P is
      type T is private;
      subtype TT is T;
   private
      type T is record
         A, B : Integer;
      end record;
   end P;

   package body P is
      A : TT := (A => 12, B => 15);
      pragma Test_Statement;
   end;

begin
   null;
end Test;
