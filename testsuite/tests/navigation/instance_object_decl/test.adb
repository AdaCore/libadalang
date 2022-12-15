--  Make sure we don't lose rebindings when doing navigation on an object
--  decl from inside a generic instance.

procedure Test is
   generic
   package Pkg is
      Obj : constant Integer;
   private
      Obj : constant Integer := 0;
   end Pkg;

   package My_Pkg is new Pkg;
   --% obj=node.p_designated_generic_decl.find(lal.ObjectDecl)
   --% completion=obj.p_next_part_for_decl()
   --% completion.p_previous_part_for_decl()
begin
   null;
end Test;
