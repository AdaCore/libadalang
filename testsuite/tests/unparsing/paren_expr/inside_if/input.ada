if (Key = Last_Hang
    or Key = Last_Crash
    or Key = Last_Find
    or Key = Slowest_Execution_MS
    or Key = Execs_Done)
  and Val = "0"
then
   Data_Valid := False;
   return Val;
end if;
