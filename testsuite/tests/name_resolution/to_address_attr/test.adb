with System.Storage_Elements;

procedure Test is
   X : System.Address := System'To_Address (0);
   pragma Test_Statement_UID;

   I : Integer := 12;
   Y : System.Address := System'To_Address (I);
   pragma Test_Statement_UID;

   J : System.Storage_Elements.Integer_Address := 42;
   Z : System.Address := System'To_Address (J);
   pragma Test_Statement_UID;
begin
   null;
end Test;
