procedure Test is
   type Non_Tagged_T is null record;
   type Deriving_Non_Tagged_T is new Non_Tagged_T;

   type Tagged_T is tagged null record;
   type Deriving_Tagged_T is new Tagged_T with null record;

   type Interface_T is interface;
   type Deriving_Interface_T is new Interface_T with null record;
   type Interface_Deriving_Interface_T is interface and Interface_T;

   subtype Tagged_T_CW is Tagged_T'Class;
   subtype Deriving_Tagged_T_CW is Deriving_Tagged_T'Class;
   subtype Interface_Subtype_T is Interface_Deriving_Interface_T;
   subtype Deriving_Interface_T_CW is Interface_Subtype_T'Class;
begin
   null;
end Test;
