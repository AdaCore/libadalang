-- Tests derived from the AI12-0212 description

pragma Ada_2022;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Sets;

procedure AI12_0212 is

   package P is
      --  Set_Type is a set-like container type
      type Set_Type is private
         with Aggregate => (Empty       => Empty_Set,
                            Add_Unnamed => Include);

      function Empty_Set return Set_Type;

      subtype Small_Integer is Integer range -1000..1000;

      procedure Include (S : in out Set_Type; N : Small_Integer) is null;

      --  Map_Type is a map-like container type
      type Map_Type is private
         with Aggregate =>  (Empty     => Empty_Map,
                             Add_Named => Add_To_Map);

      procedure Add_To_Map (M : in out Map_Type; Key : Integer; Value : String);

      Empty_Map : constant Map_Type;

      --  Vector_Type is an extensible array-like container type
      type Vector_Type is private
         with Aggregate => (Empty          => Empty_Vector,
                            Add_Unnamed    => Append_One,
                            New_Indexed    => New_Vector,
                            Assign_Indexed => Assign_Element);

      type Count_Type is new Natural;

      function Empty_Vector (Capacity : Count_Type := 0) return Vector_Type;

      procedure Append_One (V : in out Vector_Type; New_Item : String) is null;

      procedure Assign_Element (V : in out Vector_Type; Index : Positive;
                                Item : String) is null;

      function New_Vector (First, Last : Positive) return Vector_Type
        with Pre => First = Positive'First;
   private

      package Int_Sets is
        new Ada.Containers.Ordered_Sets
           (Element_Type => Small_Integer);
      type Set_Type is new Int_Sets.Set with null record;

      Empty_S : constant Set_Type :=
        (Int_Sets.Empty_Set with null record);

      package Int_String_Maps is
        new Ada.Containers.Indefinite_Ordered_Maps
          (Key_Type => Integer, Element_Type => String);

      type Map_Type is new Int_String_Maps.Map with null record;

      procedure Add_To_Map (M : in out Map_Type; Key : Integer; Value : String)
        renames Insert;

      Empty_Map : constant Map_Type :=
        (Int_String_Maps.Empty_Map with null record);

      package String_Vectors is
        new Ada.Containers.Indefinite_Vectors
          (Index_Type => Positive, Element_Type => String);

      type Vector_Type is new String_Vectors.Vector with null record;

      Empty_V : constant Vector_Type :=
        (String_Vectors.Empty_Vector with null record);
   end P;

   package body P is
      function Empty_Vector (Capacity : Count_Type := 0) return Vector_Type is
      begin
         return Empty_V;
      end Empty_Vector;

      function New_Vector (First, Last : Positive) return Vector_Type is
      begin
         return Empty_V;
      end New_Vector;

      function Empty_Set return Set_Type is
      begin
         return Empty_S;
      end Empty_Set;
   end P;

   use P;

   S : Set_Type;
   M : Map_Type;
   V : Vector_Type;

   --  Define a table of pairs
   type Pair is record
      Key : Integer;
      Value : access constant String;
   end record;

   Table : constant array(Positive range <>) of Pair :=
      [(Key => 33, Value => new String'("a nice string")),
       (Key => 44, Value => new String'("an even better string"))];

   --  Create an image table for an array of integers
   Keys : constant array (Positive range <>) of Integer := [2, 3, 5, 7, 11];
begin
   --  Example aggregates using Set_Type

   --  Assign the empty set to S:
   S := [];
   pragma Test_Statement;

   --  A positional set aggregate
   S := [1, 2];
   pragma Test_Statement;

   --  A set aggregate with an iterated_element_association
   S := [for Item in 1 .. 5 => Item * 2];
   pragma Test_Statement;

   --  A set aggregate consisting of two iterated_element_associations
   S := [for Item in 1 .. 5 => Item,
         for Item in 1 .. 5 => -Item];
   pragma Test_Statement;

   --  Example aggregates using Map_Type

   --  Create an empty map
   M := [];
   pragma Test_Statement;

   --  A simple named map aggregate
   M := [12 => "house", 14 => "beige"];
   pragma Test_Statement;

   --  A map aggregate using an iterated_element_association
   --  and a key expression, built from from a table of key/value pairs:
   --  M:= [for P of M use P.Key => P.Value];
   --  !!! Can't compile this example with GNAT.

   --  A map aggregate where the values produced by the
   --  iterated_element_association are of the same type as the key
   --  (eliminating the need for a separate key_expression):
   --  M:= [for Key of Keys => Integer'Image (Key)];
   --  !!! Make GNAT crash.

   --  The above could have been written using an explicit key_expression:
   M:= [for Key of Keys use Key => Integer'Image (Key)];
   pragma Test_Statement;

   --  Example aggregates using Vector_Type

   --  Create an empty vector aggregate
   V := [];
   pragma Test_Statement;

   --  A positional vector aggregate
   V := ["abc", "def"];
   pragma Test_Statement;

   --  An indexed vector aggregate
   V := [1 => "this", 2 => "is", 3 => "a", 4 => "test"];
   pragma Test_Statement;

   --  A vector of images of dynamic length
   V := [for I in 1 .. 5 => Integer'Image (I)];
   pragma Test_Statement;

   --  A vector made from the elements of a map
   --  V:= [for Elem of M => Elem];
   --  !!! Can't compile this example with GNAT.
end AI12_0212;
