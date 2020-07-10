with Ada.Real_Time;

procedure Delay_Until is
   use type Ada.Real_Time.Time;
   use type Ada.Real_Time.Time_Span;

   Poll_Time :          Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Period    : constant Ada.Real_Time.Time_Span
     := Ada.Real_Time.Milliseconds (10);
begin
   loop
      delay until Poll_Time;
      pragma Test_Statement_UID;
      Poll_Time := Poll_Time + Period;
   end loop;
end Delay_Until;
