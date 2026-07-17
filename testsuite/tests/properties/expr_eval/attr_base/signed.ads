package Signed is

   --  'Base of a signed integer type widens to the smallest predefined
   --  integer type covering the declared range (per target info, a power of
   --  two here).

   type Int is range -100 .. 100;

   Int_Base_Last : constant Int'Base := Int'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   --  A subtype constraint is ignored: 'Base spans Int's base range
   subtype Sub is Int range -32 .. 31;

   Sub_Base_First : constant Sub'Base := Sub'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Sub_Base_Last : constant Sub'Base := Sub'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   --  Same through a derived type
   type Der is new Sub;

   Der_Base_First : constant Der'Base := Der'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Der_Base_Last : constant Der'Base := Der'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   --  Ranges crossing the size boundaries: 'Base widens to the smallest
   --  covering predefined integer type (16-, 32- and 64-bit here).

   type R16 is range 0 .. 1_000;
   R16_Base_First : constant R16'Base := R16'Base'First;
   --% node.f_default_expr.p_eval_as_int
   R16_Base_Last : constant R16'Base := R16'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   type R32 is range -40_000 .. 40_000;
   R32_Base_First : constant R32'Base := R32'Base'First;
   --% node.f_default_expr.p_eval_as_int
   R32_Base_Last : constant R32'Base := R32'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   type R64 is range 0 .. 3_000_000_000;
   R64_Base_First : constant R64'Base := R64'Base'First;
   --% node.f_default_expr.p_eval_as_int
   R64_Base_Last : constant R64'Base := R64'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   --  Biased small representation: the 'Size clause is ignored for 'Base,
   --  which still widens to the smallest covering integer (16-bit).
   type Biased is range 1000 .. 1004;
   for Biased'Size use 3;

   Biased_Base_First : constant Biased'Base := Biased'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Biased_Base_Last : constant Biased'Base := Biased'Base'Last;
   --% node.f_default_expr.p_eval_as_int

   --  Predefined types: a predefined integer type is its own base, and
   --  subtypes such as Natural / Positive share their type's base range.
   Integer_Base_First : constant Integer'Base := Integer'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Integer_Base_Last : constant Integer'Base := Integer'Base'Last;
   --% node.f_default_expr.p_eval_as_int
   Natural_Base_First : constant Natural'Base := Natural'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Positive_Base_First : constant Positive'Base := Positive'Base'First;
   --% node.f_default_expr.p_eval_as_int
   Short_Integer_Base_Last : constant Short_Integer'Base :=
     Short_Integer'Base'Last;
   --% node.f_default_expr.p_eval_as_int
   Long_Integer_Base_Last : constant Long_Integer'Base :=
     Long_Integer'Base'Last;
   --% node.f_default_expr.p_eval_as_int

end Signed;
