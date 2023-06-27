with Ada.Text_IO; use Ada.Text_IO;

procedure Slice is
  S : constant String := "abcdefgh";

  Slice_1 : constant String := S (S'First + 1 .. S'Last - 1);
  --% node.f_default_expr.p_eval_as_string
  Slice_2 : constant String := Slice_1 (Slice_1'First + 1 .. Slice_1'Last - 1);
  --% node.f_default_expr.p_eval_as_string
  Slice_3 : constant String := Slice_2 (Slice_2'First + 1 .. Slice_2'Last - 1);
  --% node.f_default_expr.p_eval_as_string
  Slice_4 : constant String := Slice_3 (Slice_3'First + 1 .. Slice_3'Last - 1);
  --% node.f_default_expr.p_eval_as_string

  Slice_5 : String renames Slice_4;
  Slice_6 : String renames S (6 .. 7);
  Slice_7 : String := Slice_2 & Slice_3;

  S_First_1 : constant Natural := S'First;
  --% node.f_default_expr.p_eval_as_int
  S_First_2 : constant := S'First;
  --% node.f_expr.p_eval_as_int
  Slice_1_First : constant Natural := Slice_1'First;
  --% node.f_default_expr.p_eval_as_int
  Slice_2_First : constant Natural := Slice_2'First;
  --% node.f_default_expr.p_eval_as_int
  Slice_3_First : constant Natural := Slice_3'First;
  --% node.f_default_expr.p_eval_as_int
  Slice_4_First : constant Natural := Slice_4'First;
  --% node.f_default_expr.p_eval_as_int

  Slice_5_First : constant Natural := Slice_5'First;
  --% node.f_default_expr.p_eval_as_int
  Slice_6_First : constant Natural := Slice_6'First;
  --% node.f_default_expr.p_eval_as_int
  Slice_7_First : constant Natural := Slice_7'First;
  --% node.f_default_expr.p_eval_as_int

  S_Last_1 : constant Natural := S'Last;
  --% node.f_default_expr.p_eval_as_int
  S_Last_2 : constant := S'Last;
  --% node.f_expr.p_eval_as_int
  Slice_1_Last : constant Natural := Slice_1'Last;
  --% node.f_default_expr.p_eval_as_int
  Slice_2_Last : constant Natural := Slice_2'Last;
  --% node.f_default_expr.p_eval_as_int
  Slice_3_Last : constant Natural := Slice_3'Last;
  --% node.f_default_expr.p_eval_as_int
  Slice_4_Last : constant Natural := Slice_4'Last;
  --% node.f_default_expr.p_eval_as_int

  Slice_5_Last : constant Natural := Slice_5'Last;
  --% node.f_default_expr.p_eval_as_int
  Slice_6_Last : constant Natural := Slice_6'Last;
  --% node.f_default_expr.p_eval_as_int
  Slice_7_Last : constant Natural := Slice_7'Last;
  --% node.f_default_expr.p_eval_as_int
begin
   Put_Line (Slice_4);
end Slice;
