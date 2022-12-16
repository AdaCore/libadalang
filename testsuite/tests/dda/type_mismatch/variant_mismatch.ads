package Variant_Mismatch is

   type T1 is record
      I : Integer;
   end record;

   type T2 (B : Boolean) is record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;

   type T3_Parent (B : Boolean) is tagged record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;
   type T3_Child (B : Boolean) is new T3_Parent with record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;

   type T4 (B : Boolean) is record
      case B is
         when others => I : Integer;
      end case;
   end record;

   type T5 (B1, B2 : Boolean) is record
      case B1 is
         when False =>
            I1 : Integer;
         when True =>
            I2 : Integer;
      end case;
   end record;

end Variant_Mismatch;
