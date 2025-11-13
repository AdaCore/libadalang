procedure Test is
   type Non_Standard_Type is record
      X : Integer;
      Y : Boolean;
   end record;

   A : constant Boolean := 2 ** (Integer'Size - 1) - 1 = Integer'Last;
   --% node.f_default_expr.p_eval_as_int
   --  Don't print the actual value as it is target-dependent. See
   --  ada_api/target_info for a complete test case.

   B : constant Positive := Non_Standard_Type'Size;
   --% node.f_default_expr.p_eval_as_int
   --  This is not supported as Libadalang can only answer this query for
   --  predefined types in standard.

   C : constant Positive := String'Size;
   --% node.f_default_expr.p_eval_as_int
   --  It doesn't make sense for some of them though -- so they should raise
   --  an error.

   D : constant Positive := A'Size;
   --% node.f_default_expr.p_eval_as_int
   --  Calling 'Size on an object is not supported for now, even if the type
   --  of the object is one of the predefined types from Standard.

   type Specific_Size_1 is new Boolean
      with Size => 4;

   type Specific_Size_2 is new Integer;
   for Specific_Size_2'Size use 32;

   E : constant Positive := Specific_Size_1'Size + Specific_Size_2'Size;
   --% node.f_default_expr.p_eval_as_int
   --  We support the evaluation of this attribute on types for which it is
   --  explicitly defined, however.

   type New_Specific_Size_1 is new Specific_Size_1;

   F : constant Positive := New_Specific_Size_1'Size;
   --% node.f_default_expr.p_eval_as_int
   --  Even if the aspect was defined not on the designated type but one of
   --  its parent types, as this aspect is inherited.
begin
   null;
end Test;
