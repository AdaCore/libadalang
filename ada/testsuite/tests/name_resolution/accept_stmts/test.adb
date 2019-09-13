procedure Test is
   subtype My_Range is Integer range 1 .. 3;

   task Tsk is
      entry Foo;
      entry Foo (X : Integer);
      entry Bar (My_Range);
   end Tsk;

   task body Tsk is
      X : My_Range := 2;
   begin
      accept Foo;
      pragma Test_Statement;
      accept Foo (X : Integer) do
         null;
      end Foo;
      pragma Test_Statement;
      accept Bar (X);
      pragma Test_Statement;
   end Tsk;

   task type Tsk_T is
      entry Foo;
      entry Foo (X : Integer);
      entry Bar (My_Range);
   end Tsk_T;

   task body Tsk_T is
      X : My_Range := 2;
   begin
      accept Foo;
      pragma Test_Statement;
      accept Foo (X : Integer) do
         null;
      end Foo;
      pragma Test_Statement;
      accept Bar (X);
      pragma Test_Statement;
   end Tsk_T;

   task Tsk_2 is
      entry Foo;
   end Tsk_2;

   task body Tsk_2 is
      task Nested is
         entry Foo;
      end Nested;

      task body Nested is
      begin
         accept Foo;
         pragma Test_Statement;
      end Nested;
   begin
      accept Foo;
      pragma Test_Statement;
   end Tsk_2;
begin
   null;
end Test;
