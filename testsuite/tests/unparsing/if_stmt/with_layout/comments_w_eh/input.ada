procedure Main is
begin
   --  Comment A for the if

   if Something then
      -- Comment B for the next statement
      null;

   -- Comment C for the elsif

   elsif Something_Else then
      -- Comment D for the next statement
      null;

   -- Comment E for the else

   else
      -- Comment F for the next statement
      null;
   end if;

--  Comment G for the exception

exception
   when others =>
      null;
end Main;
