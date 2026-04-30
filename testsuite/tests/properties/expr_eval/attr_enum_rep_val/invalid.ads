package Invalid is

   --  'Enum_Val with a value not present in the representation clause

   type Status is (Off, Idle, Active);
   for Status use (Off => 0, Idle => 5, Active => 10);

   Bad_Rep_Val : constant Status := Status'Enum_Val (3);
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: 'Enum_Val: no enum value with given representation

   --  'Enum_Val out of range (no representation clause)

   type Color is (Red, Green, Blue);

   Out_Of_Range : constant Color := Color'Enum_Val (5);
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: out of bounds 'Enum_Val on enum

   --  'Enum_Rep applied to a non-enum type

   Bad_Type : constant := Integer'Enum_Rep (42);
   --% node.f_expr.p_eval_as_int
   --  => Exception: 'Enum_Rep only applicable to enum types

   --  'Enum_Val out of range on a character type (> Character'Last)

   Char_Val_OOR : constant Character := Character'Enum_Val (300);
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: out of bounds 'Enum_Val on enum

   --  'Enum_Val with a value not in the representation clause of a
   --  character-literal enum type.

   type Char_Code is ('a', 'b', 'c');
   for Char_Code use ('a' => 10, 'b' => 20, 'c' => 30);

   CC_Bad_Val : constant Char_Code := Char_Code'Enum_Val (15);
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: 'Enum_Val: no enum value with given representation

   --  'Enum_Rep with more than one argument

   Enum_Rep_Too_Many : constant := Color'Enum_Rep (Red, Green);
   --% node.f_expr.p_eval_as_int
   --  => Exception: 'Enum_Rep requires exactly one argument

   --  'Enum_Rep argument is not an enum literal (real number given)

   Enum_Rep_Real_Arg : constant := Color'Enum_Rep (1.0);
   --% node.f_expr.p_eval_as_int
   --  => Exception: 'Enum_Rep expects an enum argument

   --  'Enum_Val with no argument

   Enum_Val_No_Args : constant Color := Color'Enum_Val;
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: 'Enum_Val requires exactly one argument

   --  'Enum_Val with more than one argument

   Enum_Val_Too_Many : constant Color := Color'Enum_Val (0, 1);
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: 'Enum_Val requires exactly one argument

   --  'Enum_Val argument is not an integer (real number given)

   Enum_Val_Real_Arg : constant Color := Color'Enum_Val (1.0);
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: 'Enum_Val expects an integer argument

   --  'Enum_Val applied to a non-enum type

   Enum_Val_Bad_Type : constant Integer := Integer'Enum_Val (0);
   --% node.f_default_expr.p_eval_as_int
   --  => Exception: 'Enum_Val only applicable to enum types

end Invalid;
