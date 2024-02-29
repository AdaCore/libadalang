-- Test for the Preelaborable aspect/attribute name resolution

procedure Test is
   type T is new Integer;

   type U is new T with
      Preelaborable_Initializaton => T'Preelaborable_Initialization;
   pragma Test_Block;
begin
   null;
end Test;
