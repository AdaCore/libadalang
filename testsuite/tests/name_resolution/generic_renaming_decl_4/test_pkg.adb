procedure Test_Pkg is
   generic
      type T;
   package O is
   end O;
   --% pkg_name=node.f_package_name
   --% pkg_name.p_find_all_references([node.unit], follow_renamings=True)
   --% pkg_name.p_find_all_references([node.unit], follow_renamings=False)

   generic package R renames O;
   --% node.f_name.p_find_all_references([node.unit])

   package K is new R (Integer);
   --% node.f_generic_pkg_name.p_gnat_xref()
   --% node.f_generic_pkg_name.p_referenced_decl()
begin
   null;
end Test_Pkg;
