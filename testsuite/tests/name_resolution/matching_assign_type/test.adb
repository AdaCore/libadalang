procedure Test is

   type MDI is tagged null record;
   type MDI_Child is access all MDI'Class;

   type Local_T is new MDI with null record;

   function Set return access Local_T'Class;

   function Set return access Local_T'Class is
   begin
      return null;
   end Set;

   Child : MDI_Child;

begin
    Child := Set;
    pragma Test_Statement;
end Test;
