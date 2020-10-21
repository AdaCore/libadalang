procedure Test is
   type Index_T is range 1 .. 5;
   subtype Sub_Index_T is Index_T range 2 .. 4;

   type A1 is array (Index_T) of Integer;
   type A2 is array (Index_T) of A1;
   type A3 is array (Index_T) of A2;

   V : A1;
   X : A3;
   Y : Integer;
begin
   Y := X (Sub_Index_T) (2) (1 .. 4) (3) (Sub_Index_T range 2 .. 3) (2);
   Y := V (Sub_Index_T) (Sub_Index_T)'Size;
end Test;
