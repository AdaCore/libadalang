procedure Main is
   package P is
   private
      type T;

      procedure F (X : access T);

      type T is null record;
   end P;

   package body P is
      function F (X : access T) is
      begin
         null;
      end F;
   end P;
begin
   null;
end Main;
