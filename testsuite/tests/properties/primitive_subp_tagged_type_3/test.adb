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
   X.Foo;
   --% node.f_call.p_called_subp_spec.p_primitive_subp_tagged_type()
end Test;
