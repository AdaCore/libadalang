package D is

   type Int is new Integer;

   type T1 is tagged private;
   function  F2 return T1;
   function "+" (X, Y : in T1) return Int;

   procedure Main;
private
   type T1 is tagged null record;
   procedure P2 (XC : in T1'Class; Y : in Int);
end D;
