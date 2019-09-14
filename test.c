#include <stdio.h>
#include <string.h>
#include <SWI-Prolog.h>

#define MAXLINE 1024

int
main(int argc, char **argv)
{ char expression[MAXLINE];
  char *e = expression;
  char *program = argv[0];
  char *plav[2];
  int n;

  /* combine all the arguments in a single string */

  for(n=1; n<argc; n++)
  { if ( n != 1 )
      *e++ = ' ';
    strcpy(e, argv[n]);
    e += strlen(e);
  }

  /* make the argument vector for Prolog */

  plav[0] = program;
  plav[1] = NULL;

  /* initialise Prolog */

  if ( !PL_initialise(1, plav) )
    PL_halt(1);

  /* Lookup calc/1 and make the arguments and call */

  { predicate_t pred = PL_predicate("process_nice", 2, "user");
    term_t h0 = PL_new_term_refs(2);
    term_t h1 = h0 + 1;
    int rval;

    PL_put_atom_chars(h0, "ddoongj");
    rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, h0);
    char* output;
    size_t len;

    int success = PL_get_chars(h1, &output, CVT_LIST | REP_UTF8);
    if (success) {
      printf("%s\n", output);
    } else {
      printf("failed\n");
    }


    // for (int i = 0; i < len; i++) {
    //   printf("%02X %d", (unsigned char) output[i], PL_is_atom(h1));
    // }

    // printf('\n');

    PL_halt(rval ? 0 : 1);
  }

  return 0;
}
