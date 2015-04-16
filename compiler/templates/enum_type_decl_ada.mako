## vim: filetype=makoada

% if not private_part:

   type ${cls.name()} is
     (Uninitialized,
      ${", ".join(str(alt) for alt in cls.alternatives_for(ada_api))});

   ## Assign explicit constants members to ease binding to other languages.
   ## Start regular enumerators at 1 so that uninitialized is always 0, which
   ## is kind of consistent with other "absent" values such as the NULL
   ## pointer, etc.
   for ${cls.name()} use
     (Uninitialized => 0,
      ${", ".join("{} => {}".format(alt, i)
                  for i, alt in enumerate(cls.alternatives_for(ada_api), 1))});

   function Image (Value : ${cls.name()}) return String is
     (case Value is
      % for alt in cls.alternatives:
      when ${cls.get_enumerator(alt)} => "${alt}",
      % endfor
      when Uninitialized => "uninitialized");

% endif
