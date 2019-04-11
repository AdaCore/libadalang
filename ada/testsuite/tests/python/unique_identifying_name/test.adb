procedure A is
   function Bar (X, Y : Integer) return Boolean is
   begin
      return X > Y;
   end Bar;
   function Bar (X : Integer) return Boolean is
   begin
      return X > 42;
   end Bar;
   procedure Bar (X : Integer) is
   begin
      null;
   end Bar;
   B : Boolean;
begin
   B := Bar (1, 2);
   Bar (1);
end A;
