package body Pkg is
   --  package body Inner is separate;
   --  We explicitly make the code illegal by commenting out the stub.
   --  Libadalang should be able to handle this gracefully without property
   --  errors.
end Pkg;
