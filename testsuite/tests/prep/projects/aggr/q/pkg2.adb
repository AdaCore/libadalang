package body Pkg2 is

   procedure Dummy is null;

begin

#if FOO'Defined then
   null;
#else
   invalid code
#end if;

end Pkg2;
