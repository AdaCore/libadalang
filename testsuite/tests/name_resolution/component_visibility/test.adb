procedure Test is
   package Set is
      type T is record
         X : Integer;
      end record;
   end Set;

   type Rec is record
      Set : access Set.T;
   end record;

   X : Rec;
begin
   X.Set.X := 2;
   pragma Test_Statement;
end Test;
