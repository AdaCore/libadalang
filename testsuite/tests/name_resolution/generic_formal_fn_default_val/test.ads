generic
   type T;
   with function Copy (Self : T) return T is (Self);
   pragma Test_Block;
package Test is
end Test;
