#include <stdio.h>
#include <stdlib.h>

#include "libadalang.h"

#include "langkit_dump.h"
#include "langkit_text.h"


/* On Windows, we need this in order to initialize the SEH mechanism.  This
   is necessary to turn exceptions (in the Windows meaning: page fault, for
   instance) into Ada exceptions (which this testcase needs). */
extern void __gnat_initialize(void*);


int
main(void)
{
    int SEH[2];
    const ada_exception *exc;

    /* Initialize the SEH context */
    __gnat_initialize (&SEH);

    ada_context_discard_errors_in_populate_lexical_env(NULL, 0);

    exc = ada_get_last_exception();
    if (exc != NULL)
        printf("discard_errors_in_populate_lexical_env raised an exception\n");
    else
        error("discard_errors_in_populate_lexical_env raised no exception, unexpectedly\n");
    puts("Done");
    return 0;
}
