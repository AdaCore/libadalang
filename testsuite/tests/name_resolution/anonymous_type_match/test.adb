procedure Test is
   type Obj is tagged null record;

   type T is record
      Class : access Obj'Class;
   end record;

   type Obj2 is new Obj with record
      I : Integer;
   end record;

   procedure P (Prm : access Obj2);

   procedure P (Prm : access Obj2) is
       E : T := T'(Class => Prm);
       pragma Test_Statement;
   begin
      null;
   end P;

begin
   null;
end Test;
