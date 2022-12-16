package Variant_Mismatch is

   type T1 (B : Boolean) is record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;

   type T2 is record
      I : Integer;
   end record;

   type T3_Parent (B : Boolean) is record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;
   type T3_Child (B : Boolean) is record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;

   type T4 (B : Boolean) is record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;

   type T5 (B1, B2 : Boolean) is record
      case B1 is
         when False =>
            I1 : Integer;
         when True =>
            I2 : Integer;
            case B2 is
               when False => null;
               when True  => I3 : Integer;
            end case;
      end case;
   end record;

end Variant_Mismatch;
