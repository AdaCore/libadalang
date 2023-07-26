#if X then
pragma Foo;
package body Pkg is
#else
package body Pkg is
#end if;
   procedure P is null;
end Pkg;
