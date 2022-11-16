procedure Test is
   subtype My_Range is Integer range 1 .. 3;

   task Tsk is
      entry Foo;
      --% node.p_next_part_for_decl()
      --% node.p_body_part_for_decl()

      entry Foo (X : Integer);
      --% node.p_next_part_for_decl()
      --% node.p_body_part_for_decl()

      entry Bar (My_Range);
      --% node.p_next_part_for_decl()
      --% node.p_body_part_for_decl()

   end Tsk;

   task body Tsk is
      X : My_Range := 2;
   begin
      accept Foo;
      --% node.f_body_decl.p_previous_part()

      accept Foo (X : Integer) do
         null;
      end Foo;
      --% node.f_body_decl.p_previous_part()

      accept Bar (X);
      --% node.f_body_decl.p_previous_part()

   end Tsk;

   task type Tsk_T is
      entry Foo;
      --% node.p_next_part_for_decl()

      entry Foo (X : Integer);
      --% node.p_next_part_for_decl()

      entry Bar (My_Range);
      --% node.p_next_part_for_decl()

   end Tsk_T;

   task body Tsk_T is
      X : My_Range := 2;
   begin
      accept Foo;
      --% node.f_body_decl.p_previous_part()

      accept Foo (X : Integer) do
         null;
      end Foo;
      --% node.f_body_decl.p_previous_part()

      accept Bar (X);
      --% node.f_body_decl.p_previous_part()
 
   end Tsk_T;

   task Tsk_2 is
      entry Foo;
   end Tsk_2;

   task body Tsk_2 is
      task Nested is
         entry Foo;
         --% node.p_next_part_for_decl()
      end Nested;

      task body Nested is
      begin
         accept Foo;
         --% node.f_body_decl.p_previous_part()
 
      end Nested;
   begin
      accept Foo;
      --% node.f_body_decl.p_previous_part()
 
   end Tsk_2;
begin
   null;
end Test;
