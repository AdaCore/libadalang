import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_deref_null


check_deref_null.main(check_deref_null.parser.parse_args(
    ['commands-custom.adb',
     'debugger-base_gdb-gdb_cli.adb',
     'debugger-base_gdb-gdb_mi.adb',
     'dispatching_calls.adb',
     'foo.adb',
     'g-comlin.adb',
     'g-debpoo.adb',
     'g-exttre.adb',
     'g-spipat.adb',
     'gprbind.adb',
     'gps-kernel-search-sources.adb',
     'gps-location_view.adb',
     'gvd-process.adb',
     'keymanager_module.adb',
     'language-tree-database.adb',
     'language.adb',
     'project_viewers.adb',
     's-parint.adb',
     's-traceb-xi-ppc.adb',
     'traces_dump.adb',
     'vcs_view-explorer.adb']))
