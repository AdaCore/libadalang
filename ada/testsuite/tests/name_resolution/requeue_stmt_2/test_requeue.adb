with Ada.Text_IO        ; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Test_Requeue is
   task T is
      entry A (C : Integer);
      entry B;
   end T;

   task body T is
   begin
      accept A (C : Integer) do
         --  Call to an entry with a different but empty profile
         requeue B;
         pragma Test_Statement;
      end A;

      accept B;

   end T;


   type B_Range is range 1 .. 10;
   protected P is
      entry A (Val : Integer);
      entry B (B_Range);
      entry C (1 .. 9);
   end P;

   protected body P is
      entry A (Val : Integer) when True is
         function Foo return Integer is (1);
         function Foo return B_Range is (2);
      begin
         if Val > 12 then
            --  Call to an entry from an entry family
            requeue B (Foo);
            pragma Test_Statement;
         elsif Val = 13 then
            --  Ambiguous call to an entry from an entry family. NOTE: triggers
            --  a bug in GNAT.
            requeue C (Foo);
            pragma Test_Statement;
         elsif Val = 14 then
            -- Fully qualified name requeue to a single entry with same profile
            requeue Test_Requeue.P.A;
            pragma Test_Statement;
         elsif Val = 14 then
            -- Fully qualified name requeue to an entry from an entry family.
            -- NOTE: triggers a bug in GNAT.
            requeue Test_Requeue.P.C (2);
            pragma Test_Statement;
         end if;

      end A;

      entry B (for I in B_Range) when True is
      begin
         null;
      end B;

      entry C (for I in 1 .. 9) when True is
      begin
         null;
      end C;
   end P;
begin
   null;
end Test_Requeue;
