procedure Test is
   package Strings is
      type Vector is tagged null record;

      function Append (Self : Vector; Str : String) return Vector is (Self);
      function New_Line (Self : Vector) return Vector is (Self);

      Empty_Vector : constant Vector := (null record);
   end Strings;

   function Descr return Strings.Vector
   is (Strings.Empty_Vector
       .Append ("a")
       .New_Line
       .Append ("b")
       .Append ("c"
                & "d"
                & "e")
       .New_Line
       .Append ("f")
       .Append ("g"
                & "h"
                & "i")
       .New_Line
       .Append ("j")
       .Append ("k"
                & "l"
                & "m"
                & "n"
                & "o"
                & "p"
                & "q"
                & "r")
       .New_Line
       .Append ("r")
       .Append ("s"
                & "t"
                & "u"
                & "v"
                & "w"
                & "x")
       .New_Line
       .Append ("y")
       .New_Line
       .Append ("z")
       .New_Line
       .Append ("0"));
   pragma Test_Statement;
begin
   null;
end Test;

