procedure Test is
   package Pkg is
      type Kind is ('1', '2');

      type R (K : Kind) is record
         case K is
            when '1' =>
               X : Integer;
            when '2' =>
               Y : Boolean;
         end case;
      end record;
   end Pkg;

   function Foo return Pkg.R is
   begin
      return (K => '1', X => 2);
      pragma Test_Statement;
   end Foo;
begin
   null;
end Test;
