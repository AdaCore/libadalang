with Ada.Containers.Vectors;

with Libadalang.Analysis;

package Helpers is
   package LAL renames Libadalang.Analysis;

   package Unit_Vectors is new
     Ada.Containers.Vectors (Positive, LAL.Analysis_Unit, LAL."=");

   function Initialize
     (Project_Path : String;
      Sources      : out Unit_Vectors.Vector) return LAL.Analysis_Context;

end Helpers;
