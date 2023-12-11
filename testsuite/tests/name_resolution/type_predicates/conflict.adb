-- This test ensure that `Name` in the CallExpr `Name (Name'First)` is correctly
-- resolved to the `To_String`'s Name parameter while `Name (1 .. 4)` in the
-- `Predicate` pragma is correctly bind to the subtype `Name`.

procedure Conflict is
   subtype Name is String (1 .. 10);

   function To_String (Name : String) return String is
     (Name (Name'First) & "!");
   pragma Test_Statement;

   pragma Predicate (Name, Name (1 .. 4) = "FAIL");
   pragma Test_Statement;

   function P (N : String) return Boolean is (True);

   pragma Predicate (Name, P (Name (1 .. 4)));
   pragma Test_Statement;
begin
   null;
end Conflict;
