procedure Test is
   protected type A is
   end A;

   type A is synchronized interface;

   overriding function Bar is null;

begin
   null;
end Test;
