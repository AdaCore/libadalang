with Ada.Text_IO; use Ada.Text_IO;

procedure Test is

   package P is
      type My_Container is private
         with Aggregate => (New_Indexed => New_Indexed,
                            Assign_Indexed => Assign_Indexed,
                            Empty => New_Empty);

      function New_Empty return My_Container with Import;
      function New_Indexed (Lb, Hb : Integer) return My_Container with Import;
      procedure Assign_Indexed
        (Obj : in out My_Container; Index : Integer; Value : Integer)
         with Import;
   private
      type My_Container is null record;
   end;

   use P;

   type A is array (Integer range <>) of Integer;

   X : My_Container := [for I in 1 .. 10 => I];
   pragma Test_Statement;
begin
   null;
end Test;
