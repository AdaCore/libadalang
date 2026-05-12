package Valid is

   type My_Int is range -1000 .. 1000;
   type Small_Pos is range 0 .. 99;
   type Small_Neg is range -99 .. -1;
   type Null_Int is range 1 .. 0;

   subtype My_Sub is My_Int range 0 .. 9;
   subtype My_Neg_Sub is My_Int range -100 .. -1;
   subtype My_Null_Sub is My_Int range 10 .. 0;

   type Color is (Red, Green, Blue, Yellow);
   type Day_Of_Week is
     (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

   subtype Weekday is Day_Of_Week range Monday .. Friday;
   subtype Weekend is Day_Of_Week range Saturday .. Sunday;
   subtype Single_Day is Day_Of_Week range Wednesday .. Wednesday;
   subtype Null_Days is Day_Of_Week range Saturday .. Monday;

   subtype Working_Days is Day_Of_Week
     with Static_Predicate => not (Working_Days in Saturday | Sunday);
   subtype Non_Mid_Week is Day_Of_Week
      with Static_Predicate => Non_Mid_Week not in Tuesday .. Thursday;

   W1 : constant := My_Int'Width;
   --  => 5: max("-1000", " 1000")
   --% node.f_expr.p_eval_as_int

   W2 : constant := Small_Pos'Width;
   --  => 3: max(" 0", " 99"), 'Image includes a leading space for non-negative
   -- values.
   --% node.f_expr.p_eval_as_int

   W3 : constant := Small_Neg'Width;
   --  => 3: max("-99", "-1")
   --% node.f_expr.p_eval_as_int

   W4 : constant := Null_Int'Width;
   --  => 0: null range
   --% node.f_expr.p_eval_as_int

   W5 : constant := My_Sub'Width;
   --  => 2: max(" 0", " 9")
   --% node.f_expr.p_eval_as_int

   W5b : constant := My_Neg_Sub'Width;
   --  => 4: max("-100", "-1")
   --% node.f_expr.p_eval_as_int

   W5c : constant := My_Null_Sub'Width;
   --  => 0: null range
   --% node.f_expr.p_eval_as_int

   W6 : constant := Boolean'Width;
   --  => 5: max("FALSE", "TRUE")
   --% node.f_expr.p_eval_as_int

   W7 : constant := Color'Width;
   --  => 6: longest literal is Yellow
   --% node.f_expr.p_eval_as_int

   W8 : constant := Day_Of_Week'Width;
   --  => 9: longest literal is Wednesday
   --% node.f_expr.p_eval_as_int

   W9 : constant := Weekday'Width;
   --  => 9: Wednesday is within Monday .. Friday
   --% node.f_expr.p_eval_as_int

   W10 : constant := Weekend'Width;
   --  => 8: longest literal in Saturday .. Sunday is Saturday
   --% node.f_expr.p_eval_as_int

   W10b : constant := Single_Day'Width;
   --  => 9: single-element subtype, Wednesday
   --% node.f_expr.p_eval_as_int

   W10c : constant := Null_Days'Width;
   --  => 0: null range (Saturday > Monday in enum order)
   --% node.f_expr.p_eval_as_int

   W14 : constant := Working_Days'Width;
   --  => 9: predicate excludes Saturday and Sunday, longest remaining is
   --  Wednesday.
   --% node.f_expr.p_eval_as_int

   W14b : constant := Non_Mid_Week'Width;
   --  => 8: predicate excludes Tuesday .. Thursday; remaining values are
   --  Monday, Friday, Saturday, Sunday; longest is Saturday.
   --% node.f_expr.p_eval_as_int

   --  W11/W12/W13 all return 12, surprising at first but correct: GNAT's
   --  Character'Width is 12 because some control characters have longer image
   --  representations.
   W11 : constant := Character'Width;
   --% node.f_expr.p_eval_as_int

   W12 : constant := Wide_Character'Width;
   --% node.f_expr.p_eval_as_int

   W13 : constant := Wide_Wide_Character'Width;
   --% node.f_expr.p_eval_as_int

end Valid;
