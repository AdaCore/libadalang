with Ada.Unchecked_Deallocation;

procedure Test is
   type Int is range 1 .. 10;
   type Int_Access is access all Int;

   procedure Free is new Ada.Unchecked_Deallocation (Int, Int_Access);

   X : Int_Access;
begin
   Free (X);
   --% decl = node[0].f_name.p_referenced_decl()
   --% decl.p_designated_generic_decl.p_fully_qualified_name
   --% gen = decl.p_designated_generic_decl
   --% gen.p_fully_qualified_name
   --% gen.p_get_uninstantiated_node.p_fully_qualified_name
end Test;
