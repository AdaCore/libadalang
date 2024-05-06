package body Formal.Child is
   X : Integer;

   procedure Bar is
   begin
      Child.X := 42;
   end Bar;
end Formal.Child;

