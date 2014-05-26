% if _self._booleanize:
(${self_access_string} ? "True" : "False")
% else:
${self_access_string} != ${_self.nullexpr()} ? ${_self.matcher.emit_repr(self_access_string)} : "None"
% endif
