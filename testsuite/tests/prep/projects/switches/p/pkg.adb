package body Pkg is

   procedure Dummy is null;

begin

#if BAR'Defined then
   null;
#else
   invalid code
#end if;

end Pkg;
