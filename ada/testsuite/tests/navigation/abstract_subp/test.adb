procedure Test is
   package Pkg is
      type T is tagged null record;

      procedure Foo (Self : T) is abstract;
      --% node.p_defining_name.p_next_part()
   end Pkg;
begin

end Test;
