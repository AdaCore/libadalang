if Subprogram_Data.Kind in Ada_Subp_Kind_Function_Range
  and then (for all Parameter of Subprogram_Data.Parameters_Data
            => Parameter.Type_Parent_Package
               /= Subprogram_Data.Return_Type_Parent_Package)
then
   null;
end if;