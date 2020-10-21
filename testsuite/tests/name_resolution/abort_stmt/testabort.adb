with Ada.Text_IO; use Ada.Text_IO;

procedure Testabort is
   package B is
      task T is
      end T;
   end B;

   package body B is
      task body T is
      begin
         loop
            Put_Line ("HAI");
            delay 0.5;
         end loop;
      end T;
   end B;


   task type My_Task_Type is
   end My_Task_Type;

   task body My_Task_Type is
   begin
      loop
         Put_Line ("HAI");
         delay 0.5;
      end loop;
   end My_Task_Type;

   T2 : My_Task_Type;

   use B;
begin
   abort T;
   pragma Test_Statement;

   abort T2;
   pragma Test_Statement;
end Testabort;
