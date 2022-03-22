--  We want to check that Libadalang does not complain about a missing body for
--  Pkg. To check that the event handler works correctly, leave a reference to
--  a missing package too.

with Gen_Pkg;
with Missing_Pkg;

package Pkg is new Gen_Pkg (Missing_Pkg.Initial_Value);
