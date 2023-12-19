procedure Test_Task is
   type My_Bool is new Boolean;

   task T is
      entry E;
   end T;

   task body T is
      X, Y : My_Bool;
   begin
      --  This one is allowed
      select when X and not Y =>
         accept E do
            null;
         end E;
      or
         terminate;
      end select;
      pragma Test_Statement;
   end T;

   protected type P is
      entry Foo;
   private
      X, Y : My_Bool;
   end P;

   protected body P is
      --  But this one is not allowed, only standard booleans are accepted
      entry Foo when X and not Y is
      begin
         null;
      end Foo;
      pragma Test_Statement;
   end P;
begin
   null;
end Test_Task;
