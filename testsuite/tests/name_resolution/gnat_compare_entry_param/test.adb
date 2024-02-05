procedure Test is
   task T is
      entry E (C1 : Integer; C2 : Boolean);
   end T;

   task body T is
      X : Integer;
   begin
      accept E (C1 : Integer; C2 : Boolean) do
         if C2 then
            X := C1;
         end if;
      end E;
   end T;
begin
   null;
end Test;
