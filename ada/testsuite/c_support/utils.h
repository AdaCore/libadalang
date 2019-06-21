#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>


const ada_internal_entity_info no_entity_info = { { false, false, false, NULL }, NULL, false };


static void
error(const char *msg)
{
    const ada_exception *exc = ada_get_last_exception();

    fprintf(stderr, "%s\n", msg);
    if (exc != NULL) {
        puts("Last Ada exception:");
        puts(exc->information);
    }

    exit(1);
}

static bool
token_eq_p(const ada_token *left, const ada_token *right)
{
  return (left->token_data == right->token_data
	  && left->token_index == right->token_index
	  && left->trivia_index == right->trivia_index);
}

#endif /* UTILS_H */
