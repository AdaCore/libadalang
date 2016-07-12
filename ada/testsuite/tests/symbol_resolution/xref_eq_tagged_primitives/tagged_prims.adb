procedure Tagged_Prims is

   type Int is range 0 .. 102;

   type T is tagged record
      Baz : Int;
   end record;

   procedure Foo (Self : T) is null;
   procedure Bar (Self : access T) is null;

   procedure Baz (Self : T'Class) is null;
   procedure Qux (Self : access T'Class) is null;

   procedure Baz (Self : T'Class; Pouet : Int) is null;

   T_Inst : T;

   A : Int;

begin
   pragma Test(T_Inst.Baz);

   pragma Test (T_Inst.Baz (12));

   T_Inst.Baz;
   pragma Test_Statement;

   Baz (T_Inst, A);
   pragma Test_Statement;

   T_Inst.Baz (A);
   pragma Test_Statement;
end Tagged_Prims;
