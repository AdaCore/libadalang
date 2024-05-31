package body Pkg is
   package Types is
      type T is null record;
   end Types;

   package Inner is
      use Types;
      procedure Bar;
   end Inner;

   procedure Test is
   begin
      Inner.Bar;
   end Test;

   package body Inner is separate;
end Pkg;
