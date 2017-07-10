generic
   type Element_T is private;
package Vector is
   type Vector is tagged null record;

   function Create return Vector;
   function Element (Self : Vector; Index : Integer) return Element_T;
   procedure Append  (Self : Vector; Element: Element_T);
   function Append (Self : Vector; Element: Element_T) return Integer;

   generic
      with function "<" (L, R : Element_T) return Boolean;
   procedure Sort (Self : in out Vector);

end Vector;
