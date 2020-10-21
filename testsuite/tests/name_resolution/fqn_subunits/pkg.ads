with Foo;

package Pkg is

   type Kikou is private;

   package Inner is
      type T is private;

      use Foo;

      procedure Proc;

      Pouet : Integer;
   private
      type T is record
         Obj : Integer;
      end record;
   end Inner;

private
   type Kikou is record
      Lol : Integer;
   end record;

   Pouet : Integer;

   Z : Float;

   type T is null record;
end Pkg;
