generic
   type Foo_Bar_Baz is private;
   with function ">" (Left : Foo; Right : Bar) return Boolean is <>;
function Perform_Comparison_Check
  (Comparing_Value : Foo_Bar_Baz;
   Rule_Value : Foo_Bar_Baz;
   Logic_Operator : Unbounded_String)
   return Boolean;
