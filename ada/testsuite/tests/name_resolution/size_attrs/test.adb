procedure Main is
   type R is record
      X : Integer;
   end record;

   X : R;
   I : Integer;
begin
   I := R'Size;
   pragma Test_Statement;

   I := X'Size;
   pragma Test_Statement;

   I := R'Object_Size;
   pragma Test_Statement;

   I := R'Value_Size;
   pragma Test_Statement;

   I := R'Max_Size_In_Storage_Elements;
   pragma Test_Statement;

   I := R'VADS_Size;
   pragma Test_Statement;
end Main;
