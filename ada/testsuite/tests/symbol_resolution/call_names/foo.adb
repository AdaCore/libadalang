package body Foo is

   procedure Proc (I : Integer);
   procedure Proc (I, J : Integer);

   package Pack is
      procedure Proc;
      procedure Proc (I : Integer);
      procedure Proc (S : String);

      procedure Local is
      begin
         Proc;
         Proc (1);
         Proc (S => "foo");
         Foo.Pack.Proc (I => 1);
      end Local;

      procedure Global is
      begin
         Proc (1, 2);
         Foo.Proc (1);
      end Global;

      procedure None is
      begin
         Proc (1, 2, 3);
         Foo.Pack.Proc (1);
      end Bar;
   end Pack;

   procedure No_Local is
   begin
      Proc;
      Proc (S => "foo");
   end No_Local;

end Foo;
