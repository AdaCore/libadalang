function Error_Message (What, Expected, Actual : String) return String
is ("Current " & What & " is:"
    & ASCII.LF & "  "
    & (if Actual = "" then "<none>" else Actual)
    & ASCII.LF & "which is inconsistent with the " & What
    & " for " & GNATcov_RTS_Project & ": "
    & ASCII.LF & "  "
    & (if Expected = "" then "<none>" else Expected));
