with Ada.Containers.Vectors;

package Test is

   package T_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Integer);

   type T_Vector is new T_Vectors.Vector with null record;
end Test;
