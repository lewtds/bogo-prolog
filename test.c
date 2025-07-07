#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <SWI-Prolog.h>


int main(int argc, char **argv)
{ 
  
  char *program = argv[0];
  char *plav[2];
  int n;

  /* make the argument vector for Prolog */

  plav[0] = program;
  plav[1] = NULL;

  /* initialise Prolog */

  if ( !PL_initialise(1, plav) )
    PL_halt(1);

  /* Lookup calc/1 and make the arguments and call */

  { predicate_t pred = PL_predicate("process_atom", 2, "user");
    term_t h0 = PL_new_term_refs(2);
    term_t h1 = h0 + 1;
    int rval;

    PL_put_atom_chars(h0, "ddoongj");
    rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, h0);
    assert(rval == 1);
    char* output;

    int success = PL_get_chars(h1, &output, CVT_ALL | REP_UTF8);
    if (success) {
      printf("%s\n", output);
    } else {
      printf("failed\n");
    }

    PL_halt(rval ? 0 : 1);
  }

  return 0;
}
