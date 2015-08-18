## vim: filetype=makoada

% if not private_part:
   type ${cls.name()}_Type;
   type ${cls.name()}_Access is access all ${cls.name()}_Type;
   type ${cls.name()} is access all ${cls.name()}_Type'Class;
% endif
