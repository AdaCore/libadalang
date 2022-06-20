with Main_Generic_Package;

package Main_Package is

   function Boolean_And (X, Y : Boolean) return Boolean is (X and Y);

   package Operations is new Main_Generic_Package (Boolean);

   procedure Operation_Not is new Operations.Operation (Boolean_And);
   --% decl=node.p_designated_generic_decl
   --% param=decl.p_subp_spec_or_null(True).p_params[0].p_defining_name
   --% parent_decl=param.p_parent_basic_decl
   --% parent_decl.p_generic_instantiations

end Main_Package;
