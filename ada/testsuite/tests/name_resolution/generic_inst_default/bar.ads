generic
   type Element_Type is private;
package Bar is
   procedure Proc (E : Element_Type);

   pragma Test (Element_Type);
end Bar;
