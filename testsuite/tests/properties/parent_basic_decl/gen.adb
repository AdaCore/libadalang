procedure Gen is
   generic
   package Pkg_G is
      X : Integer;
   end Pkg_G;
   --% node.parent.p_parent_basic_decl
   --% node.p_parent_basic_decl
   --% node.find(lal.ObjectDecl).p_parent_basic_decl

   generic
   procedure Subp_G (X : Integer);
   --% node.p_parent_basic_decl
   --% node.f_subp_decl.p_parent_basic_decl
   --% node.find(lal.ParamSpec).p_parent_basic_decl

   package My_Pkg is new Pkg_G;
   --% node.p_parent_basic_decl
   --% decl = node.p_designated_generic_decl
   --% decl.p_parent_basic_decl
   --% internal = decl.f_package_decl
   --% internal.p_parent_basic_decl
   --% obj = internal.find(lal.ObjectDecl)
   --% obj.p_parent_basic_decl

   procedure My_Subp is new Subp_G;
   --% node.p_parent_basic_decl
   --% decl = node.p_designated_generic_decl
   --% decl.p_parent_basic_decl
   --% internal = decl.f_subp_decl
   --% internal.p_parent_basic_decl
   --% param = decl.find(lal.ParamSpec)
   --% param.p_parent_basic_decl
begin
   null;
end Gen;
