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
begin
   null;
end Test;
