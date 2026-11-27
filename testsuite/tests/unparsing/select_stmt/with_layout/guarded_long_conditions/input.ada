task body Server is
begin
   select
      -- Wait until the work can be accepted.
      when Work_Is_Available and then The_Server_Is_Ready_To_Process_Another_Work_Item => -- Ready
         accept Work do
            Process_Work;
         end Work;
   or
      when Timeout_Is_Enabled
         -- Check whether work is still pending.
         and then There_Is_No_Pending_Work =>
         -- Wait for the next deadline.
         delay until Deadline;
         Timed_Out;
   end select;
end Server;
