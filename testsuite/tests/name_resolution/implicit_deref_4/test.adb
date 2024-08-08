procedure Test is
   type Database is tagged null record;

   type Connection (C, D : access Database) is null record
     with Implicit_Dereference => D;
   pragma Test_Block;

   subtype Sub_Connection is Connection;

   function Get (DB : Database'Class) return Boolean is (True);

   procedure P (DB : Sub_Connection) is
      B : Boolean := Get (DB);
      pragma Test_Statement;
   begin
      null;
   end P;

   D : access Database;

   type Sub_Connection_3 is new Connection
     with Implicit_Dereference => D;
   pragma Test_Block;
begin
   null;
end;
