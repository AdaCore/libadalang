#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>


static void
error(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
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
