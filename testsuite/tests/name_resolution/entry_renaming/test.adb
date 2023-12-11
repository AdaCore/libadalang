-- An entry can be renamed within its own body

procedure Test is
   task Tas is
      entry Ent;
      entry Ent (I : Integer);
   end Tas;

   task body Tas is
      procedure P1 renames Ent;
      pragma Test_Statement;
      procedure P1 (I : Integer) renames Ent;
      pragma Test_Statement;
   begin
      accept Ent do
         declare
            procedure P2 renames Ent;
            pragma Test_Statement;
            procedure P2 (I : Integer) renames Ent;
            pragma Test_Statement;
         begin
            null;
         end;
      end Ent;
   end Tas;
begin
   Tas.Ent;
   pragma Test_Statement;
end Test;
