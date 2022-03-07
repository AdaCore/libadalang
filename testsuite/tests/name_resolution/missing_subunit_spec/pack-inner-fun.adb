separate (Pack.Inner)
function Fun return Integer is
   procedure Proc is separate;
begin
   Proc;
   return 0;
end Fun;
