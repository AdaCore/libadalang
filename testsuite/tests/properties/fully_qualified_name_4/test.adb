with Ada.Unchecked_Deallocation;

procedure Test is
   type Int is range 1 .. 10;
   type Int_Access is access all Int;

   procedure Free is new Ada.Unchecked_Deallocation (Int, Int_Access);

   X : Int_Access;
begin
   Free (X);
   --% node[0].f_name.p_referenced_decl().p_designated_subp.p_fully_qualified_name
end Test;
