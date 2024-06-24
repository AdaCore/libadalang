procedure Test is

   generic
   package PP is
      generic
         type T is range <> or use Integer;
      package Pkg is
         X : T;
      end Pkg;

      package My_Pkg_1 is new Pkg;

      package My_Pkg_2 is new Pkg (Long_Integer);
   end PP;

   package M is new PP;
   --% obj = node.p_designated_generic_decl.find(lal.ObjectDecl)
   --% obj.f_type_expr.p_designated_type_decl

   --% insts = node.p_designated_generic_decl.findall(lal.GenericInstantiation)
   --% gens = [inst.p_designated_generic_decl for inst in insts]
   --% objs = [gen.find(lal.ObjectDecl) for gen in gens]
   --% [obj.f_type_expr.p_designated_type_decl for obj in objs]

begin
   null;
end Test;
