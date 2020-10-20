package Base is
   type A;  --  This incomplete type decl will be ignored

   type A is record
      B : Integer;
      --  B field
   end record;


   procedure Barize (Self : A);
   --  Barize @Self
   --  We can also use the :ref:ada:``Self`` syntax.

   procedure Nodoc (Self : A);
   --% no-document: True
   --% unknown-annotation: True

   procedure Init_A;
   --% belongs-to: A

   procedure Init;
   --  This is the global initialization procedure

   No_A : constant A := (B => 12);
   --  This is a null constant for @A

   Random_Integer_Constant : constant Integer := 12;
   --  This is a random integer constant

   Renaming_Object : Integer renames Random_Integer_Constant;
   --  Renaming object

   No_Default_Val : access Integer;

   task T;
   --  Not handled yet, will cause "not handled" warnings

   --  Nested package declaration with doc before
   package Nested is
   end Nested;

   generic
      type T is private;
      --  Documentation for generic formal
   package P is
   end P;
   --  Generic package with doc inside


   procedure Proc_With_Documented_Params
      (A : Integer;
       --  Doc for param ``A``
       B : Float
       --  Doc for param ``B``
       );

   type Disc_Private
     (K : Boolean
      -- Doc for ``K``
     ) is private;
   --  Private type with discriminants

   My_Exception : exception;
   --  My exception type


   package P_Inst is new P (Integer);
   --  Instantiation of @P

   package P is
      type T is tagged record
         F, G : Integer;
      end record;

      type U is new T with private;
      --  Private type that will make `p_shapes` crash, see TA20-019
   private
      type U is new T with null record;
   end P;

private
   type Disc_Private (K : Boolean) is null record;
end Base;
