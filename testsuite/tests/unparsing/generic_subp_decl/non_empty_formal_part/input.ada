generic
   type Foo_Bar_Baz is private;
   with function ">" (Left_Left_Left_Left_Left : Foo_Bar_Baz; Right_Right_Right_Right_Right : Foo_Bar_Baz) return Boolean is <>;
function Perform_Comparison_Check
  (Comparing_Value : Foo_Bar_Baz;
   Rule_Value : Foo_Bar_Baz;
   Logic_Operator : Unbounded_String)
   return Boolean;
