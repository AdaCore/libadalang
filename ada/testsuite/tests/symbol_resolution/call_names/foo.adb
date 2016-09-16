pragma Config (Display_Slocs => True);

package body Foo is

   type Integer is range 1 .. 100;
   type String is array (Positive range <>) of Character;

   procedure Proc (I : Integer);
   procedure Proc (I, J : Integer);

   package Pack is
      procedure Proc;
      procedure Proc (I : Integer);
      procedure Proc (S : String);

      procedure Local is
      begin
         pragma Section ("Local");
         pragma Test (Proc);
         pragma Test (Proc (1));
         pragma Test (Proc (S => "foo"));
         pragma Test (Foo.Pack.Proc (I => 1));
      end Local;

      procedure Global is
      begin
         pragma Section ("Global");
         pragma Test (Proc (1, 2));
         pragma Test (Foo.Proc (1));
      end Global;

      procedure None is
      begin
         pragma Section ("None");
         pragma Test (Proc (1, 2, 3));
         pragma Test (Foo.Pack.Proc (1));
      end Bar;
   end Pack;

   procedure No_Local is
   begin
      pragma Section ("No_Local");
      pragma Test (Proc);
      pragma Test (Proc (S => "foo"));
   end No_Local;

end Foo;
