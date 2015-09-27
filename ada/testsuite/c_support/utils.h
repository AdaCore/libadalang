#ifndef UTILS_H
#define UTILS_H

#include <stdio.h>
#include <stdlib.h>


static void
error(const char *msg)
{
    fputs(msg, stderr);
    exit(1);
}

#endif /* UTILS_H */
