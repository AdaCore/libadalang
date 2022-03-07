separate (Pack)
package body Inner is
   function Fun return Integer is separate;
begin
   I := Fun;
end Inner;
