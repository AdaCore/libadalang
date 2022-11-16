procedure Test is
   task type T is
      entry E (I : Integer);
   end T;
 
   task body T is
      B : Boolean;
   begin
      select
         accept E (I : Integer) do
            B := I = 1;
            pragma Test_Statement;
            B := E.I = 1;
            pragma Test_Statement;
         end E;
      end select;
   end T;
begin
   null;
end Test;
