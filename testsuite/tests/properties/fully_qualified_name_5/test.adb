with Ada.Unchecked_Deallocation;

procedure Test is
   A, B : Integer;
   --% node.p_fully_qualified_name
   --% a = node.p_defining_names[0]
   --% b = node.p_defining_names[1]
   --% a.p_fully_qualified_name
   --% b.p_fully_qualified_name
   --% a.p_fully_qualified_name_array
   --% b.p_fully_qualified_name_array
   --% b.p_canonical_fully_qualified_name
begin
   null;
end Test;
