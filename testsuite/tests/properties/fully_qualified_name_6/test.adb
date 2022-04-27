--  Check that p_fully_qualified_name returns correct results on synthetic
--  types.

with Ada.Unchecked_Deallocation;

procedure Test is
   type T is tagged null record;

   function Foo return
      T'Class 
      with Import;
   --% node[1][3].p_designated_type_decl.p_fully_qualified_name

   function Bar return Integer'Base with Import;
   --% node[1][3].p_designated_type_decl.p_fully_qualified_name

   A : aliased Integer;
begin
   A := A'Unrestricted_Access.all;
   --% node[1][0].p_expression_type.p_fully_qualified_name
end Test;
