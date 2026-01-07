procedure Test is
   package Pkg is
      type A is null record;

      type B is tagged null record
        with First_Controlling_Parameter;

      procedure Foo (X : A; Y : B) is null;
      --  Is a primitive of A but not of B, since the B parameter does not
      --  appear in first position and B is marked First_Controlling_Parameter.
      --% node.p_subp_spec_or_null().p_primitive_subp_types()

      function Bar (X : B) return B is (X);
      --  Is a primitive of B since it appears in first position
      --% node.p_subp_spec_or_null().p_primitive_subp_types()

      type C is tagged null record;

      procedure Baz (X : Integer; Y : C) is null;
      --  Is a normal primitive of C (no First_Controlling_Parameter aspect)
      --% node.p_subp_spec_or_null().p_primitive_subp_types()
   end Pkg;

   package Der is
      type AA is new Pkg.A;

      type BB is new Pkg.B with null record;

      procedure Foo_Bar (X : AA; Y : BB) is null;
      --  Is a primitive of AA but not of BB, since the latter does not appear
      --  in first position but inherits and First_Controlling_Parameter aspect
      --  of its parent type.
      --% node.p_subp_spec_or_null().p_primitive_subp_types()

      type CC is new Pkg.C with null record
         with First_Controlling_Parameter;

      overriding procedure Baz (X : Integer; Y : CC) is null;
      --  Is a primitive of CC although it has the First_Controlling_Parameter
      --  aspect, because it overrides a primitive of its parent type Pkg.C.
      --% node.p_subp_spec_or_null().p_primitive_subp_types()
   end Der;

   use Der;

   X : BB;
   Y : AA;
begin
   X := Bar (X);
   Foo (Y, Pkg.B (X));
end Test;

