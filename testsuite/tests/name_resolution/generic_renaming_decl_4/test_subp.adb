procedure Test_Subp is
   generic
      type T is private;
   procedure O (X : T);
   --% subp_name=node.p_defining_names[0]
   --% subp_name.p_find_all_references([node.unit], follow_renamings=True)
   --% subp_name.p_find_all_references([node.unit], follow_renamings=False)

   procedure O (X : T) is
   begin
      null;
   end;

   generic procedure R renames O;
   --% node.f_name.p_find_all_references([node.unit])

   procedure K is new R (Integer);
   --% node.f_generic_subp_name.p_gnat_xref()
   --% node.f_generic_subp_name.p_referenced_decl()
begin
   null;
end Test_Subp;
