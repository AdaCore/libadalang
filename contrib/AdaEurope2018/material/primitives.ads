package Primitives is

   type T is abstract tagged null record;

   function Create_T return T is abstract;
   function Create_T (I : Integer) return T is abstract;
   --  Primitives that has no controlling parameter

   procedure Print (Value : T) is abstract;
   --  Simple case

   procedure Print (Prefix : String; Value : T) is abstract;
   --  Controlling parameter is not first

   procedure Print (Previous : T; Value : T) is abstract;
   --  Two controlling arguments

   function "<" (Left, Right : T) return Boolean is abstract;
   --  Two controlling arguments in the same param spec

end Primitives;
