separate (Pkg)
package body Inner is
   procedure Not_Used is null;
begin
   I := 1;
   pragma Test_Statement;
end Inner;
