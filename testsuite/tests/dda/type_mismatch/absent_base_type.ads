package Absent_Base_Type is

   type T1_Child is new T1_Parent with null record;

   type T2_Parent is tagged null record;
   type T2_Child is new T2_Parent with null record;

end Absent_Base_Type;
