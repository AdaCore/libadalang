private with GNATCOLL.Locks;

package Libadalang.Unit_Files is
private

   use GNATCOLL.Locks;

   GPR_Lock : aliased Mutual_Exclusion;
   --  Lock to serialize all calls to GNATCOLL.Projects, which is not
   --  thread-safe. Hopefully we will be able to get rid of this lock when
   --  moving to Libgpr2.

end Libadalang.Unit_Files;
