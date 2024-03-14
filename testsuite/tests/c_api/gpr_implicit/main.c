#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "utils.h"
#include "langkit_text.h"

int
main (void)
{
    ada_gpr_project gpr;
    ada_string_array_ptr errors;
    ada_string_array_ptr files;
    char *file;

    printf ("== simple ==\n\n");

    ada_gpr_project_load_implicit (
        "",
        "",
        &gpr,
        &errors
    );
    abort_on_exception ();

    // Dislpay all files in the project
    printf ("Files in the implicit project:\n");
    files = ada_gpr_project_source_files (gpr, 1, NULL, 0);
    abort_on_exception ();

    if (files->length > 0) {
        for (int i = 0; i < files->length; i++) {
            file = file_basename (files->c_ptr[i]);
            printf ("  - %s\n", file);
            free (file);
        }
    } else {
        printf ("No files\n");
    }

    // Release the resources
    ada_gpr_project_free (gpr);
    abort_on_exception ();
    ada_free_string_array (errors);
    abort_on_exception ();
    ada_free_string_array (files);
    abort_on_exception ();

    return 0;
}
