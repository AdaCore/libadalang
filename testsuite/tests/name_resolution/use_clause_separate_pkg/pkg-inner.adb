separate(Pkg)
package body Inner is
   procedure Bar is
      X : T;
      pragma Test_Statement;
   begin
      null;
   end Bar;
end Inner;
