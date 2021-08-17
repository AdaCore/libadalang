procedure Test is
   type R (B : Boolean) is record
      case B is
         when False =>
            X : Integer;
         when True =>
            null;
      end case;
   end record;
   --% node.p_get_aspect("unchecked_union")
   pragma Unchecked_Union (R);

   type Arr is array (1 .. 10) of Integer;
   --% node.p_get_aspect("volatile_components")
   --% node.p_get_aspect("independent_components")
   pragma Volatile_Components (Arr);
   pragma Independent_Components (Arr);

   type Enum is (A, B, C);
   --% node.p_get_aspect("discard_names")
   pragma Discard_Names (Enum);
begin
   null;
end Test;
