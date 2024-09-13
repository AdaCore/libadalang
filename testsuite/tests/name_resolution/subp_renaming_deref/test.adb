procedure Test is
   type String_Function is access function return String;
   G_Pointer: String_Function := null;

   function Renamed_G return String renames G_Pointer.all;
   pragma Test_Statement;
begin
   null;
end Test;
