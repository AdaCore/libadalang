from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()

p_body = ctx.get_from_file("pack.adb")

some_task = p_body.root.find(lal.TaskBody)

print("some_task:", some_task)
print("previous part:", some_task.p_previous_part_for_decl)
print("canonical part:", some_task.p_canonical_part)
print(
    "canonical_part.next_part:",
    some_task.p_canonical_part.p_next_part_for_decl
)
print(
    "canonical_part.next_part.next_part:",
    some_task.p_canonical_part.p_next_part_for_decl.p_next_part_for_decl
)
print(
    "canonical_part.next_part.next_part.next_part:",
    some_task.p_canonical_part.p_next_part_for_decl
    .p_next_part_for_decl.p_next_part_for_decl
)
