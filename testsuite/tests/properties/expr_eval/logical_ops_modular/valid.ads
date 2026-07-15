package Valid is

   type Byte is mod 256;

   --  Binary logical operators on a power-of-2 modular type

   Mod_And : constant Byte := 170 and 204;
   --% node.f_default_expr.p_eval_as_int
   --  0xAA and 0xCC = 0x88 = 136

   Mod_Or : constant Byte := 170 or 204;
   --% node.f_default_expr.p_eval_as_int
   --  0xAA or 0xCC = 0xEE = 238

   Mod_Xor : constant Byte := 170 xor 204;
   --% node.f_default_expr.p_eval_as_int
   --  0xAA xor 0xCC = 0x66 = 102

   --  Unary "not" on a power-of-2 modular type: not X = 255 - X

   Mod_Not : constant Byte := not 170;
   --% node.f_default_expr.p_eval_as_int
   --  not 0xAA = 0x55 = 85

   Mod_Not_Zero : constant Byte := not 0;
   --% node.f_default_expr.p_eval_as_int
   --  not 0 = 255

   Mod_Not_Max : constant Byte := not 255;
   --% node.f_default_expr.p_eval_as_int
   --  not 255 = 0

   --  Non-power-of-2 modular type: modulus reduction applies to or/xor

   type Mod5 is mod 5;

   Mod5_And : constant Mod5 := 3 and 4;
   --% node.f_default_expr.p_eval_as_int
   --  011 and 100 = 000 = 0

   Mod5_Or : constant Mod5 := 3 or 4;
   --% node.f_default_expr.p_eval_as_int
   --  011 or 100 = 111 = 7 mod 5 = 2

   Mod5_Xor : constant Mod5 := 3 xor 4;
   --% node.f_default_expr.p_eval_as_int
   --  011 xor 100 = 111 = 7 mod 5 = 2

   Mod5_Not : constant Mod5 := not 3;
   --% node.f_default_expr.p_eval_as_int
   --  5 - 1 - 3 = 1

   --  Chained operations

   Mod_Chain : constant Byte := (170 and 204) or 1;
   --% node.f_default_expr.p_eval_as_int
   --  (0x88 or 0x01) = 0x89 = 137

end Valid;
