package Test is
   type Type_T is tagged null record;
   procedure Bar (T: Type_T) with Post'Class => True;
end Test;
