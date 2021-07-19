procedure Test is
   type R is new Integer;
   --% node.p_is_interface_type()

   type I is interface;
   --% node.p_is_interface_type()

   subtype J is I;
   --% node.p_is_interface_type()

   subtype K is R;
   --% node.p_is_interface_type()

   type JJ is interface and J;
   --% node.p_is_interface_type()

   type T is tagged null record;
   --% node.p_is_interface_type()

   type TT is new T and I with null record;
   --% node.p_is_interface_type()
begin
   null;
end Test;
