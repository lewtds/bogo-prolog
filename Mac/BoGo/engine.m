//
//  engine.m
//  BoGo
//
//  Created by Ngo Trung on 5.7.2025.
//

#import <Foundation/Foundation.h>
#import <SWI-Prolog.h>

int init_engine(void) {
    char *plav[10];
    /* make the argument vector for Prolog */
    
    int ac = 0;
    plav[ac++] = "hello";
    plav[ac++] = "-x";
    plav[ac++] = [[[NSBundle mainBundle] pathForResource:@"mystate" ofType:@""] cStringUsingEncoding:NSUTF8StringEncoding];
    plav[ac++] = "-q";  // Quiet startup
    plav[ac++] = "--nosignals";  // Don't install signal handlers
    plav[ac++] = "--no-tty";  // No terminal interaction
    plav[ac++] = "-g";
    plav[ac++] = "true";
    plav[ac] = NULL;
    
    /* initialise Prolog */
    int init_success = PL_initialise(ac, plav);
    return init_success;
}

void stop_engine(int rval) {
    PL_cleanup(rval);
}

int process_text(NSString* input, NSString **output) {
    /* Lookup calc/1 and make the arguments and call */
    predicate_t pred = PL_predicate("process_atom", 2, "user");
    term_t h0 = PL_new_term_refs(2);
    term_t h1 = h0 + 1;
    
    PL_put_atom_chars(h0, [input cStringUsingEncoding:NSUTF8StringEncoding]);
    int rval = PL_call_predicate(NULL, PL_Q_NORMAL, pred, h0);
    
    char* tmp_out;
    int success = PL_get_chars(h1, &tmp_out, CVT_ALL | REP_UTF8);
    
    *output = [NSString stringWithUTF8String:tmp_out];
    return success;
}
