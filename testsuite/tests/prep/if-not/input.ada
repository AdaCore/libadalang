procedure Foo is
begin

------------
-- Simple --
------------

#if not True_Sym then
   null;
#end if;

#if not False_Sym then
   null;
#end if;

-------------
-- Complex --
-------------

# if not X'Defined then
   null;
#end if;

# if not True_Sym'Defined then
   null;
#end if;

# if not Foo_Sym = "foo" then
   null;
# end if;

# if not (Foo_Sym = "foo") then
   null;
# end if;

# if not (True_Sym = "True" and False_Sym = "True") then
   null;
# end if;

-------------
-- Invalid --
-------------

# if not "foo" then
   null;
# end if;

# if not Foo_Sym then
   null;
# end if;

end Foo;
