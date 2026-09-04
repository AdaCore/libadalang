with Ext;

package Test_Aspect
  with
    Abstract_State => (S1, S2, S3, S4),
    Initializes    =>
      (S1,
       X,
       S2 => Ext.State,
       S3 => (Ext.V, Ext.W),
       S4 => null,
       Y  => Ext.V,
       Z  => (Ext.V, Ext.State))
is
   X : Integer;
   Y : Integer;
   Z : Integer;

   package Single
     with Initializes => Var
   is
      Var : Integer := 0;
   end Single;

   package Parenthesized
     with Initializes => (Par_Var)
   is
      Par_Var : Integer := 0;
   end Parenthesized;

   --  Here the initialization_item denotes a state abstraction, which has no
   --  type.
   package Parenthesized_State
     with Abstract_State => Par_State, Initializes => (Par_State)
   is
   end Parenthesized_State;

   package Nested
     with Initializes => (Nested_Var => X)
   is
      Nested_Var : Integer := X;
   end Nested;

   package Nothing
     with Abstract_State => A, Initializes => null
   is
   end Nothing;
end Test_Aspect;
pragma Test_Block;
