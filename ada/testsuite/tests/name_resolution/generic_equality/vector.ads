generic
   type Element_T is private;
package Vector is
   type Vector is private;

   function Element (Self : Vector; Index : Integer) return Element_T;
   procedure Append  (Self : Vector; Element: Integer) return Element_T;
end Vector;
