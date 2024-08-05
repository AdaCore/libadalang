task body Server is
   Current_Work_Item : Work_Item;
begin
   loop
      select
         accept Next_Work_Item (WI : in Work_Item) do
            Current_Work_Item := WI;
         end;
         Process_Work_Item (Current_Work_Item);
      or
         accept Shut_Down;
         exit;       -- Premature shut down requested
      or
         terminate;  -- Normal shutdown at end of scope
      end select;
   end loop;
end Server;
