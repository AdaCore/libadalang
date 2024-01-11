procedure Test is
   type Rec (K : Boolean) is record
      X : Integer;
      case K is
         when True =>
            Y : Integer;
         when False =>
            Z : Boolean;
      end case;
   end record;

   --  Should not fail silently
   A : Rec := (K => True, X => 1, Z => 2);
   pragma Test_Statement;

   --  Should not raise an exception
   B : Rec := (X => 1, Y => 2);
   pragma Test_Statement;
begin
   null;
end Test;
