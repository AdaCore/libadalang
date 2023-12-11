procedure Test is
   type Non_Limited is range 1 .. 10;
   --% node.p_is_limited_type

   type Limited_Null_Rec is limited null record;
   --% node.p_is_limited_type

   type Limited_Array is array (Positive range <>) of Limited_Null_Rec;
   --% node.p_is_limited_type

   type Limited_Derived_Type is new Limited_Null_Rec;
   --% node.p_is_limited_type

   type Limited_Tagged_Rec is tagged limited null record;
   --% node.p_is_limited_type

   type Limited_Derived_Tagged_Type is new Limited_Tagged_Rec with null record;
   --% node.p_is_limited_type

   type Limited_Component_Type is record
      X : Limited_Null_Rec;
   end record;
   --% node.p_is_limited_type

   type Limited_Variant_Type (K : Boolean) is record
      case K is
         when True =>
            X : Limited_Null_Rec;
         when False =>
            Y : Integer;
      end case;
   end record;
   --% node.p_is_limited_type

   type Non_Limited_Interface is interface;
   --% node.p_is_limited_type

   type Limited_Synchronized_Interface is synchronized interface;
   --% node.p_is_limited_type

   type Limited_Interface is limited interface;
   --% node.p_is_limited_type

   type Non_Limited_Derived_Interface is interface and Limited_Interface;
   --% node.p_is_limited_type

   type Non_Limited_From_Limited_Interface is new Limited_Interface
      with null record;
   --% node.p_is_limited_type
begin
   null;
end Test;
