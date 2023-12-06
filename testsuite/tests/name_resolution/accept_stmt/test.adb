procedure Test is
   task t is
      entry ent1 (character);
   end t;

   task body t is
   begin
      accept ent1 ('c') do
         null;
      end;
      pragma Test_Statement;
   end t;
begin
   null;
end Test;
