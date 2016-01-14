#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"


int
main(void)
{
    const ada_exception *exc;

    libadalang_initialize();
    ada_destroy_analysis_context(NULL);

    exc = ada_get_last_exception();
    if (exc != NULL)
        printf("ada_destroy_analysis_context raised an exception\n");
    else
        error("ada_destroy_analysis_context raised no exception,"
              " unexpectedly\n");
    puts("Done");
    return 0;
}
