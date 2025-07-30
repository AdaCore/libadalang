procedure Test is
   type Non_Abstract is range 1 .. 10;
   --% node.p_is_abstract_type

   type Incomplete_Abstract_Type is abstract tagged;
   --% node.p_is_abstract_type

   type Abstract_Tagged_Rec is abstract tagged null record;
   --% node.p_is_abstract_type

   type Abstract_Derived_Tagged_Type is abstract new Abstract_Tagged_Rec
   with null record;
   --% node.p_is_abstract_type

   type Derived_Tagged_Type is new Abstract_Tagged_Rec with null record;
   --% node.p_is_abstract_type

   type Interface_Type is interface;
   --% node.p_is_abstract_type

   type Derived_Interface is interface and Interface_Type;
   --% node.p_is_abstract_type

   type Non_Abstract_From_Interface is new Interface_Type with null record;
   --% node.p_is_abstract_type

   subtype Classwide_Type is Abstract_Tagged_Rec'Class;
   --% node.p_is_abstract_type
begin
   null;
end Test;
