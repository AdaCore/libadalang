with Subp_G;

--  The result should be 'None' but should not raise an error saying
--  that file 'Subp.adb' is not found, since we don't expect to have a body
--  for this unit.

procedure Subp is new Subp_G;
--% node.p_next_part_for_decl()
