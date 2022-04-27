--  Check that the output of p_parent_basic_decl is correct for synthetic types

with Ada.Unchecked_Deallocation;

procedure Test is
   type T is tagged null record;

   function Foo return
      T'Class 
      with Import;
   --% node[1][3].p_designated_type_decl.p_parent_basic_decl

   function Bar return Integer'Base with Import;
   --% node[1][3].p_designated_type_decl.p_parent_basic_decl

   A : aliased Integer;
begin
   A := A'Unrestricted_Access.all;
   --% node[1][0].p_expression_type.p_parent_basic_decl
end Test;
