package body Pkg is

   procedure Dummy is null;

begin

#if FOO'Defined then
   null;
#else
   invalid code
#end if;

end Pkg;
