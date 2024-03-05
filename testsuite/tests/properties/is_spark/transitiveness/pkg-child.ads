package Pkg.Child with SPARK_Mode is
   pragma Elaborate_Body;

   procedure Foo;

   procedure Bar
      with Annotate => (GNATProve, Skip_Proof);

   procedure Baz;

   procedure Bazz
      with Annotate => (GNATProve, Skip_Proof);

   K : Integer;
end Pkg.Child;
