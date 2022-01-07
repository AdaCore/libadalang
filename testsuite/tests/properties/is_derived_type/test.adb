procedure Test is
   type Non_Tagged_T is null record;
   type Deriving_Non_Tagged_T is new Non_Tagged_T;

   type Tagged_T is tagged null record;
   type Deriving_Tagged_T is new Tagged_T with null record;

   type Interface_T is interface;
   type Deriving_Interface_T is new Interface_T with null record;
begin
   null;
end Test;
