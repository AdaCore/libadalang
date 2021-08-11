package Test is
   type Type_T is tagged null record;

   type Private_T is new Type_T with private;

   type Public_From_Private_T is new Private_T with null record;

private

   type Private_T is new Type_T with record
      null;
   end record;

end Test;
