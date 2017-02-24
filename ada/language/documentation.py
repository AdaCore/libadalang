from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

libadalang_docs = {
    'libadalang.project_scenario_variable': """
        Couple name/value to define a scenario variable for a project.
    """,
    'libadalang.create_project_unit_file_provider': """
        Load the project file at Project_File and return an unit file provider
        that uses it.

        % if lang == 'python':
            If provided, Scenario_Vars must be a dict with key strings and key
            values to describe the set of scenario variables for this project.
        % else:
            If not ${null}, Scenario_Vars must point to an array of
            ${capi.get_name('project_scenario_variable')} couples to provide
            scenario variables for this project. The last element of this array
            must end with a { ${null}, ${null} } couple.
        % endif

        % if lang == 'c':
            When done with it, the result must be free'd with
            ${capi.get_name('destroy_unit_file_provider')}.
        % endif

        If the requested project is invalid (error while opening the file,
        error while analysing its syntax, ...),
        % if lang == 'python':
            this raises an InvalidProjectError.
        % else:
            this returns ${null}.
        % endif
    """,
    'libadalang.invalid_project_error': """
        Raised when an error occurs while loading a project file.
    """,
}
