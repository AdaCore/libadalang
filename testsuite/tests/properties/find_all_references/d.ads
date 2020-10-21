package D is
   type T;
   pragma Find_All_References (Any);

   function Get_T return T;

   type T is private;
private
   type T is null record;
end D;
