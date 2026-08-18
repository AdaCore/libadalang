procedure Test is
   generic
      type S is private;
   package Gen is
      type P is tagged null record;

      procedure Set (X: in out P; Sc : S) is null;
   end Gen;

   package Inst2 is new Gen (Integer);

   type Foo is new Inst2.P with null record;
   --% spec = node.p_get_primitives(True, False)[0].p_subp_spec_or_null()
   --% spec.p_param_types()
   --% [x.p_formal_type() for x in spec.p_abstract_formal_params]
begin
   null;
end Test;


