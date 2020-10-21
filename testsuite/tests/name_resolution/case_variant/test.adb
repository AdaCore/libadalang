procedure Test is
   type Color is (Red, Green, Blue);

   type My_Rec_Variant (C : Color) is record
      case C is
         when Red =>
            A : Integer;
         when Color range Green .. Blue =>
            B : Integer;
      end case;
   end record;
   pragma Test_Block;
begin
   null;
end Test;
