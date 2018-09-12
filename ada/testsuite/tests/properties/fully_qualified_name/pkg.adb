package body Pkg is

   package body Nested is
      procedure Proc is null;
   end Nested;

   procedure Stub is separate;

end Pkg;
