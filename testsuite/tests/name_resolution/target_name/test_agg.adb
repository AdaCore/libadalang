procedure Test_Agg is
   type R is record
      Count : Integer;
   end record;

   Param : R;
begin
   Param.Count := @ + 1;
   pragma Test_Statement;
   Param := (Count => @.Count + 1);
   pragma Test_Statement;
   Param := (@ with delta Count => @.Count + 1);
   pragma Test_Statement;
end;
