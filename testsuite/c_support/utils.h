#ifndef UTILS_H
#define UTILS_H

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>


const ada_internal_entity_info no_entity_info = { { false, NULL, NULL }, NULL, false };

/* Print the exception that was raised by the last Libadalang API call.  When
   there was no such exception, print nothing if OR_SILENT, and print "Got no
   exception" otherwise.  Return if there was an exception.  */

static bool
print_exception (bool or_silent)
{
  const ada_exception *exc = ada_get_last_exception ();
  if (exc != NULL)
    {
      char *exc_name = ada_exception_name (exc->kind);
      printf ("Got an exception (%s):\n  %s\n",
	      exc_name,
	      exc->information);
      free (exc_name);
      return true;
    }
  else if (! or_silent)
    puts ("Got no exception\n");
  return false;
}

static void
abort_on_exception (void)
{
  if (print_exception (true))
    exit (1);
}

static void
error(const char *msg)
{
    printf("%s\n", msg);
    print_exception (true);
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
