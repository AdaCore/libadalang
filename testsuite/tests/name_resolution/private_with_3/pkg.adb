with Pkg.Priv;
package body Pkg is
   function Get return Integer is
   begin
      return Priv.Y;
   end Get;
end Pkg;
