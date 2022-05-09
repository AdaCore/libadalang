procedure Test is
   generic
   package Gen_Pkg with Ghost is
   end Gen_Pkg;

   package Instantiated_Pkg is new Gen_Pkg;
   --% node.p_is_ghost_code

   generic
   procedure Gen_Proc with Ghost;

   procedure Gen_Proc is null;

   procedure Instantiated_Proc is new Gen_Proc;
   --% node.p_is_ghost_code

   package Pkg with Ghost is
   end Pkg;

   package Renamed_Pkg renames Pkg;
   --% node.p_is_ghost_code

   procedure Proc is null with Ghost;

   procedure Renamed_Proc renames Proc;
   --% node.p_is_ghost_code

begin
   null;
end Test;
