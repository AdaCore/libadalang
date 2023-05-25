package Valid is

   S1 : constant String := "Hello World!";
   --% node.f_default_expr.p_eval_as_string

   S2 : constant String := "Hello";
   --% node.f_default_expr.p_eval_as_string

   S3 : constant String := "World!";
   --% node.f_default_expr.p_eval_as_string

   S4 : constant String := S2 & " " & S3;
   --% node.f_default_expr.p_eval_as_string

   S5 : constant String := "A" & "B" & "C" & "D" & "E";
   --% node.f_default_expr.p_eval_as_string

   S6 : constant String := "A" & ("B" & ("C" & ("D" & "E")));
   --% node.f_default_expr.p_eval_as_string

   S7 : constant String := ("A" & "B") & ("C" & ("D" & "E"));
   --% node.f_default_expr.p_eval_as_string

   S_First : constant := S1'First;
   --% node.f_expr.p_eval_as_int

   S_Last : constant := S1'Last;
   --% node.f_expr.p_eval_as_int

   S_First_2 : constant Natural := S1'First;
   --% node.f_default_expr.p_eval_as_int

   S_Last_2 : constant Natural := S1'Last;
   --% node.f_default_expr.p_eval_as_int

   S_Length : constant := S1'Length;
   --% node.f_expr.p_eval_as_int

   Item_P : constant := Character'Pos ('H');
   --% node.f_expr.p_eval_as_int

   S_Item : constant Natural := Character'Pos (S1 (S_Last));
   --% node.f_default_expr.p_eval_as_int

   S_Item_2 : constant Natural := Character'Pos (S1 (S_First));
   --% node.f_default_expr.p_eval_as_int

   Slice : String renames S1 (S_First + 1 .. S_Last - 1);
   --% node.f_renaming_clause.f_renamed_object.p_eval_as_string

   Slice_First : constant Natural := Slice'First;
   --% node.f_default_expr.p_eval_as_int

   Slice_Last : constant Natural := Slice'Last;
   --% node.f_default_expr.p_eval_as_int

   Slice_Item : constant Natural := Character'Pos (Slice (Slice_First));
   --% node.f_default_expr.p_eval_as_int
end Valid;
