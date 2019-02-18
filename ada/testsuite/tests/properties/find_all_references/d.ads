package D is
   type T;

   function Get_T return T;

   type T is private;
private
   type T is null record;
end D;
