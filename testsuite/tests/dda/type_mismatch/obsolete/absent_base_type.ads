package Absent_Base_Type is

   type T1_Parent (B : Boolean) is tagged limited record
      case B is
         when False => null;
         when True  => I : Integer;
      end case;
   end record;

   type T1_Child is new T1_Parent with null record;

   type T2_Child is null record;

end Absent_Base_Type;
