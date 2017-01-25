with Types;
use type Types.Int;

package Foo is
   procedure Bar (I : Types.Int);
private
   procedure Bar (I : Types.Byte);
end Foo;
