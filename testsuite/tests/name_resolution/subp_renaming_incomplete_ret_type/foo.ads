package Foo is
   type List is tagged null record;

   type A is abstract tagged;

   type A_Access is access all A'Class;

   type A_List is new List with null record;


   function Element_At (L : A_List; P : Positive) return A'Class;

   function As_A (X : A_List; C : Positive) return A'Class renames Element_At;

   type A is tagged null record;
end Foo;
pragma Test_Block;
