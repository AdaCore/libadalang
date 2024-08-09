with Pkg;

package Models is
   procedure Foo_Model (X : Integer)
      with Model_Of => Pkg.Foo;
   pragma Test_Block;

   procedure Foo_Model (X : Pkg.T)
      with Model_Of => Pkg.Foo;
   pragma Test_Block;

   function Bar_Model (X : Boolean) return Boolean
      with Model_Of => Pkg.Inner.Bar;
   pragma Test_Block;

   procedure Baz_Model (X : Integer)
      with Model_of => Pkg.Gen.Baz;
   pragma Test_Block;

   procedure Baz_Model (X : Pkg.Gen.F)
      with Model_of => Pkg.Gen.Baz;
   pragma Test_Block;

   procedure Gen_Proc_1_Model (X : Integer)
      with Model_Of => Pkg.Gen_Proc_1;
   pragma Test_Block;

   type T_Model is private
      with Model_Of => Pkg.T;
   pragma Test_Block;

   -----------------------------------------------------------
   --  TODO: all the following cases are not supported yet  --
   -----------------------------------------------------------

   procedure Gen_Proc_2_Model (X : Pkg.Gen_Proc_2.F)
      with Model_Of => Pkg.Gen_Proc_2;

   procedure Private_Proc_Model (X : Integer)
      with Model_Of => Pkg.Private_Proc;

   procedure Private_Proc_Model (X : Pkg.Private_Type)
      with Model_Of => Pkg.Private_Proc;

   type Private_Type_Model is private
      with Model_Of => Pkg.Private_Type;
end Models;
