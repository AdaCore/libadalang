procedure Test is
   type Database is tagged null record;

   type Connection (D : access Database) is tagged null record
     with Implicit_Dereference => D;

   subtype Sub_Connection is Connection;

   function Get (DB : Database'Class) return Boolean is (True);

   procedure P (DB : Sub_Connection) is
      B : Boolean := Get (DB);
      pragma Test_Statement;
   begin
      null;
   end P;
begin
   null;
end;
