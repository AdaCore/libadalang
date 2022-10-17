import docutils.parsers.rst.roles

from langkit.documentation import PassthroughNode


# Passthrough the ada:rmlink role, that we want to be handled by
# sphinxcontrib-adadomain in generated doc.
docutils.parsers.rst.roles.register_local_role(
    "rmlink", PassthroughNode.role_fn
)

libadalang_docs = {
    'libadalang.gpr_project': """
        Loaded GPR project file.
    """,
    'libadalang.gpr_scenario_variable': """
        Couple name/value to define a scenario variable for a project.
    """,
    'libadalang.string_array': """
        List of strings.
    """,
    'libadalang.free_string_array': """
        Free the given list of strings.
    """,
    'libadalang.gpr_project_load': """
        Load a project file with the given parameter. On success, allocate and
        return an ``ada_gpr_project`` record. Raise an ``Invalid_Project``
        exception on failure.
    """,
    'libadalang.gpr_project_free': """
        Free resources allocated for ``Self``.
    """,
    'ada_gpr_project_create_unit_provider': """
        Create a project provider using the given GPR project. If ``Project``
        is passed, it must be the name of a sub-project. If the selected
        project contains conflicting sources, raise an ``Inavlid_Project``
        exception.
    """,
    'libadalang.gpr_project_source_files': """
        Compute the list of source files in the given GPR project according to
        ``Mode`` (whose value maps to positions in the
        ``Libadalang.Project_Provider.Source_Files_Mode`` enum) and return it.
    """,
    'libadalang.create_project_unit_provider': """
        Load the project file at ``Project_File`` and return a unit provider
        that uses it.

        If ``Project`` is passed, use it to provide units, otherwise, try use
        the whole project tree.

        As unit providers must guarantee that there exists at most one source
        file for each couple (unit name, unit kind), aggregate projects that
        contains several conflicting units are not supported: trying to load
        one will yield an error (see below).

        % if lang == 'python':
        If provided, ``Scenario_Vars`` must be a dict with key strings and
        key values to describe the set of scenario variables for this
        project.

        In order to load the given project with non-default target and
        runtimes, pass these as strings to the ``target`` and ``runtime``
        arguments.

        % else:
        If not ``${null}``, ``Scenario_Vars`` must point to an array of
        ``${capi.get_name('project_scenario_variable')}`` couples to
        provide scenario variables for this project. The last element of
        this array must end with a ``{ ${null}, ${null} }`` couple.

        If not ``${null}``, ``target`` and ``runtime`` must point to valid
        NULL-terminated strings.
        % endif

        % if lang == 'c':
        When done with it, the result must be free'd with
        ``${capi.get_name('destroy_unit_provider')}``.
        % endif

        If the requested project is invalid (error while opening the file,
        error while analysing its syntax, ...), or if it is an unsupported
        aggregate project,
        % if lang == 'python':
        this raises an ``InvalidProjectError`` exception.
        % else:
        this returns ``${null}``.
        % endif
    """,
    'libadalang.create_preprocessor_from_file': """
        Load the preprocessor data file at
        %if lang == 'c':
        ``Filename`` using, directory names in the
        ``Path_Data``/``Path_Length`` array
        % else:
        ``filename``, using directory names in ``path``
        % endif
        to look for for it and the definition files it references.

        % if lang == 'c':
        If ``Line_Mode`` is not null,
        % else:
        If ``line_mode`` is passed,
        % endif
        use it to force the line mode for source files on which the
        preprocessor is enabled.  Forcing the line mode is often needed as the
        default is to remove lines that contain preprocessor directives and
        disabled code, which breaks the line number correspondance between
        original source code and preprocessed one.  Forcing to ``blank_lines``
        or ``comment_lines`` preserves this correspondance.

        Return a file reader that preprocesses sources accordingly.
    """,
    'libadalang.gpr_project_create_preprocessor': """
        Create preprocessor data from compiler arguments found in the given GPR
        project (``-gnateP`` and ``-gnateD`` arguments), or from the
        ``Project`` sub-project (if the argument is passed).

        If ``Line_Mode`` is not null, use it to force the line mode in each
        preprocessed source file.

        Note that this function collects all arguments and returns an
        approximation from them: it does not replicates exactly gprbuild's
        behavior. This may raise a ``File_Read_Error`` exception if this fails
        to read a preprocessor data file and a ``Syntax_Error`` exception if
        one such file has invalid syntax.
    """,
    'libadalang.project_provider.invalid_project': """
        Raised when an error occurs while loading a project file.
    """,
    'libadalang.project_provider.unsupported_view_error': """
        Raised when creating a project unit provider for an unsupported project
        view (for instance, a view with conflicting aggregated projects).
    """,
    'libadalang.create_auto_provider': """
        Return a unit provider that knows which compilation units are to be
        found in the given list of source files.

        This knowledge is built trying to parse all given input files as Ada
        source files and listing the compilation units found there. Files that
        cannot be parsed properly are discarded. If two compilation units are
        found for the same unit, the first that is found in the given input
        files is taken and the other ones are discarded.

        Source files are decoded using the given charset. If it is ``${null}``,
        the default charset (ISO-8859-1) is used.

        % if lang == 'c':
        ``input_files`` must point to a ``NULL``-terminated array of
        filenames.  Once this function returns, this array and the strings
        it contains can be deallocated.

        When done with it, the result must be free'd with
        ``${capi.get_name('destroy_unit_provider')}``.
        % endif

        .. TODO: Find a way to report discarded source files/compilation units.
    """,
}
