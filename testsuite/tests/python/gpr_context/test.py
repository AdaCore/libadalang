"""
Check that ``GPRProject.create_context`` works as expected.
"""

from __future__ import annotations

import os.path

import libadalang as lal


def text_repr(t: str) -> str:
    result = []
    for char in t:
        code = ord(char)
        result.append(
            f"\\x{code:02x}" if code < 0x20 or code >= 0x7f else char
        )
    return f'"{"".join(result)}"'


def check(
    label: str,
    root_project: str,
    project: str = "",
    event_handler: lal.EventHandler | None = None,
    with_trivia: bool = True,
    tab_stop: int = 8
) -> None:
    """
    Load the requested root_project/project, create an analysis context from it
    and the given additional arguments and perform various requests on that
    context.
    """

    print(f"== {label} ==")
    print()

    # Load the requested project and create the analysis context from it
    gpr = lal.GPRProject(root_project)
    try:
        ctx = gpr.create_context(project, event_handler, with_trivia, tab_stop)
    except lal.UnsupportedViewError as exc:
        print("Unsupported_View_Error:", str(exc))
        print()
        return

    u = ctx.get_from_provider("pkg", lal.AnalysisUnitKind.unit_body)
    if u.diagnostics:
        for d in u.diagnostics:
            print(f"{os.path.basename(u.filename)}:{d.as_text}")
        raise RuntimeError

    # To show that With_Trivia / Tab_Stop are properly forwarded to the
    # analysis context constructor and that the default charset is correctly
    # determined, show the first token (or trivia).
    t = u.first_token
    print(
       "pkg%b first token/trivia:"
       f" <Token Kind={t.kind} Text={text_repr(t.text)}>"
    )
    print("pkg%b root node:", u.root)

    # To show that the unit provider works as expected, resolve the Pkg package
    # spec from its body.
    n = u.root.f_body.f_item.p_previous_part()
    print("pkg%b previous part:", n)

    # To show that configuration pragmas are properly detected from the
    # project, print their list.
    for p in u.root.p_all_config_pragmas:
        print("Config pragma:", p)

    print()


check(label="Simple: defaults", root_project="simple/p.gpr")
check(label="Simple: without trivia",
      root_project="simple/p.gpr",
      with_trivia=False)
check(label="Simple: tab stop = 4",
      root_project="simple/p.gpr",
      tab_stop=4)

check(label="UTF-8", root_project="utf-8/p.gpr")

check(label="Aggregate project (no specific view)",
      root_project="aggregate/agg.gpr")
check(label="Aggregate project (specific view: p2)",
      root_project="aggregate/agg.gpr",
      project="p2")


class DummyEH(lal.EventHandler):
    """
    Event handler that prints a message only the first time a unit is parsed.
    """
    def __init__(self) -> None:
        self.triggered = False

    def unit_parsed_callback(
        self,
        context: lal.AnalysisContext,
        unit: lal.AnalysisUnit,
        reparsed: bool,
    ):
        if not self.triggered:
            self.triggered = True
            print("Unit_Parsed_Callback invoked")


check(label="Simple: event handler",
      root_project="simple/p.gpr",
      event_handler=DummyEH())

check(label="Preprocessing (p1)", root_project="preprocessing/p1.gpr")
check(label="Preprocessing (p2)", root_project="preprocessing/p2.gpr")

check(label="Config pragmas (p1)", root_project="config_pragmas/p1.gpr")
check(label="Config pragmas (p2)", root_project="config_pragmas/p2.gpr")

print("Done.")
