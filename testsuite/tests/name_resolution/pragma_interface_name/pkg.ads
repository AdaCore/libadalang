pragma Ada_83;

package Pkg is
   procedure Foo;
   pragma Interface (C, Foo);
   pragma Interface_Name (Foo, "foo", "link");
   pragma Test_Statement;

   procedure Bar;
   pragma Interface (C, Bar);
   pragma
     Interface_Name
       (Entity => Bar, External_Name => "bar", Link_Name => "link");
   pragma Test_Statement;
end;
