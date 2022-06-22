procedure Test is
   package Pkg is
      type T is tagged null record;

      procedure Foo (X : T) is null;
   end Pkg;

   package Der is
      type U is new Pkg.T with null record;
   end Der;

   X : Der.U;
begin
   Der.Foo (X);
   --% called_subp=node.f_call.p_referenced_decl()
   --% called_subp.p_subp_spec_or_null().p_primitive_subp_tagged_type()
end Test;
