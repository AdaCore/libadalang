package body Pkg is

   procedure Dummy is null;

begin

#if BAR'Defined then
   null;
#else
   invalid code
#end if;

#if Debug then
   null;
#end if;

end Pkg;
