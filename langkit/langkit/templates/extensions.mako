## Helper def to include an extension iff it exists on disk. It doesn't
## directly call compilectx.ext, because it is useful to be able to call it
## from the outside, to change the shape of the surrounding code depending on
## the existence of the extension.
<%def name="include_extension(ext)">
   % if ext:
      <%include file='${ext}'/>
   % endif
</%def>

