procedure Spin (R : in out Resource) is
begin
   loop
      select
         Controller.Request (Medium) (Some_Item);
         R.Seize;
         return;
      else
         null;  --  busy waiting
      end select;
   end loop;
end;
