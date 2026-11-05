package body Wrapper is
   protected body Test is
      entry E (for I in Integer) when True is
      begin
         null;
      end E;
   end Test;
end Wrapper;
