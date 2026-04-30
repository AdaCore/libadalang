package Valid is

   --  Without a representation clause: 'Enum_Rep is equivalent to 'Pos

   type Color is (Red, Green, Blue);

   Color_Rep_Red   : constant := Color'Enum_Rep (Red);
   --% node.f_expr.p_eval_as_int
   --  => 0

   Color_Rep_Green : constant := Color'Enum_Rep (Green);
   --% node.f_expr.p_eval_as_int
   --  => 1

   Color_Rep_Blue  : constant := Color'Enum_Rep (Blue);
   --% node.f_expr.p_eval_as_int
   --  => 2

   Color_Val_0 : constant Color := Color'Enum_Val (0);
   --% node.f_default_expr.p_eval_as_int
   --  => 0  (Red, position 0)

   Color_Val_2 : constant Color := Color'Enum_Val (2);
   --% node.f_default_expr.p_eval_as_int
   --  => 2  (Blue, position 2)

   --  With a representation clause: 'Enum_Rep returns the assigned codes

   type Status is (Off, Idle, Active);
   for Status use (Off => 0, Idle => 5, Active => 10);

   Status_Rep_Off    : constant := Status'Enum_Rep (Off);
   --% node.f_expr.p_eval_as_int
   --  => 0

   Status_Rep_Idle   : constant := Status'Enum_Rep (Idle);
   --% node.f_expr.p_eval_as_int
   --  => 5

   Status_Rep_Active : constant := Status'Enum_Rep (Active);
   --% node.f_expr.p_eval_as_int
   --  => 10

   --  'Enum_Val maps representation codes back to enum literals;
   --  p_eval_as_int returns the position of the resulting literal.

   Status_Val_0  : constant Status := Status'Enum_Val (0);
   --% node.f_default_expr.p_eval_as_int
   --  => 0  (Off, position 0)

   Status_Val_5  : constant Status := Status'Enum_Val (5);
   --% node.f_default_expr.p_eval_as_int
   --  => 1  (Idle, position 1)

   Status_Val_10 : constant Status := Status'Enum_Val (10);
   --% node.f_default_expr.p_eval_as_int
   --  => 2  (Active, position 2)

   --  With negative and non-contiguous representation values

   type T is (A, B, C);
   for T use (A => -4, B => 0, C => 999);

   T_Rep_A : constant := T'Enum_Rep (A);
   --% node.f_expr.p_eval_as_int
   --  => -4

   T_Rep_B : constant := T'Enum_Rep (B);
   --% node.f_expr.p_eval_as_int
   --  => 0

   T_Rep_C : constant := T'Enum_Rep (C);
   --% node.f_expr.p_eval_as_int
   --  => 999

   T_Val_Neg4 : constant T := T'Enum_Val (-4);
   --% node.f_default_expr.p_eval_as_int
   --  => 0  (A, position 0)

   T_Val_0   : constant T := T'Enum_Val (0);
   --% node.f_default_expr.p_eval_as_int
   --  => 1  (B, position 1)

   T_Val_999 : constant T := T'Enum_Val (999);
   --% node.f_default_expr.p_eval_as_int
   --  => 2  (C, position 2)

   --  'Enum_Rep with object variable references as argument

   O : T := A;
   P : T := B;
   Q : T := C;

   T_Rep_O : constant Integer := T'Enum_Rep (O);
   --% node.f_default_expr.p_eval_as_int
   --  => -4  (O = A)

   T_Rep_P : constant Integer := T'Enum_Rep (P);
   --% node.f_default_expr.p_eval_as_int
   --  => 0  (P = B)

   T_Rep_Q : constant Integer := T'Enum_Rep (Q);
   --% node.f_default_expr.p_eval_as_int
   --  => 999  (Q = C)

   --  'Enum_Rep on a subtype

   subtype U is T range A .. B;

   X : U := A;

   U_Rep_X : constant Integer := U'Enum_Rep (X);
   --% node.f_default_expr.p_eval_as_int
   --  => -4  (X = A)

   --  No-argument form: X'Enum_Rep (object or literal as prefix)

   T_Rep_A_2 : constant := A'Enum_Rep;
   --% node.f_expr.p_eval_as_int
   --  => -4

   T_Rep_B_2 : constant := B'Enum_Rep;
   --% node.f_expr.p_eval_as_int
   --  => 0

   T_Rep_O_2 : constant Integer := O'Enum_Rep;
   --% node.f_default_expr.p_eval_as_int
   --  => -4  (O = A)

   --  Character types: no representation clause, so 'Enum_Rep equals 'Pos
   --  and 'Enum_Val returns the character code as an integer.

   Char_Rep_A  : constant := Character'Enum_Rep ('A');
   --% node.f_expr.p_eval_as_int
   --  => 65  (ASCII code of 'A')

   Char_Val_65 : constant Character := Character'Enum_Val (65);
   --% node.f_default_expr.p_eval_as_int
   --  => 65  ('A', position 65)

   --  Enumeration type with character-literal enumerators and a
   --  representation clause; also tests 'Enum_Rep/'Enum_Val on a subtype.

   type Char_Code is ('a', 'b', 'c');
   for Char_Code use ('a' => 10, 'b' => 20, 'c' => 30);

   subtype Sub_Char_Code is Char_Code range 'a' .. 'b';

   CC_Rep_A : constant := Char_Code'Enum_Rep ('a');
   --% node.f_expr.p_eval_as_int
   --  => 10

   CC_Rep_C : constant := Char_Code'Enum_Rep ('c');
   --% node.f_expr.p_eval_as_int
   --  => 30

   CC_Val_20 : constant Char_Code := Char_Code'Enum_Val (20);
   --% node.f_default_expr.p_eval_as_int
   --  => 1  ('b', position 1)

   Sub_CC_Rep_A : constant := Sub_Char_Code'Enum_Rep ('a');
   --% node.f_expr.p_eval_as_int
   --  => 10

   Sub_CC_Val_10 : constant Sub_Char_Code := Sub_Char_Code'Enum_Val (10);
   --% node.f_default_expr.p_eval_as_int
   --  => 0  ('a', position 0)

end Valid;
