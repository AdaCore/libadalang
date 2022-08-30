with Ada.Finalization;

procedure Test1 is
   subtype Narrow is integer range 10 ..20;
   generic
      type Item_Type is private;
      type Item_Count is range <> or use Narrow; --  New syntax.
      with function "=" (L, R : in Item_Type) return Boolean is <>;
      I : Item_Count;
   package Lists is

      type List is private;

      procedure Append
        (L    : in out List;
         Item : in     Item_Type);

      function Length
        (L : in List)
        return Natural;
      procedure Report;

   private

      type Node;
      type Link is access all Node;
      type Node is
         record
            Data       : Item_Type;
            Next, Prev : Link;
         end record;

      type List is new Ada.Finalization.Controlled with
         record
            N          : Item_Count'Base := 0;
            Head, Tail : Link;
         end record;

   end Lists;

   package body Lists is
      procedure Append
        (L    : in out List;
         Item : in     Item_Type)
      is
      begin
         null;
      end;

      function Length
        (L : in List)
        return Natural
      is
      begin
         return 5;
      end;

      procedure Report is
      begin
         Put_Line (Item_Count'Image (Item_Count'First));
      end;
   end Lists;

   package Int_Lists is new Lists (Integer, Positive, "=", 33);
   pragma Test_Statement;
   package Int_Lists2 is new Lists (Integer, "=" => "=", I => 11);
   pragma Test_Statement;
begin
   null;
end Test1;
