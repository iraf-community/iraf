/**
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <errno.h>
#include "voApps.h"


char  optval[128];

/*  Task specific option declarations.
 */
char *opts = "abn:v:";
struct option long_opts[] = {
    { "debug",    0, 0, 'd'},
    { "nport",    1, 0, 'n'},
    { "verbose",  2, 0, 'v'},
    { NULL,       0, 0, 0  }
};



/*
 *	-v
 *	-v 0
 *	-v=0
 *	--verbose
 *	--verbose 0
 *	--verbose=0
 *	verbose=0
*/

int	ch=0, all=0, debug=0, verbose=0, nport=0;

int
main (int argc, char *argv[])
{
    int   i, pos;
    char  **pargv;


    /*  Initialize the parameters, i.e. rewrite the options so they can
     *  be processed by getopt_long().
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0)  {
	if (ch > 0) {
	    switch (ch) {
	    case 'a': printf ("'a' key\n");   			
		break;
	    case 'd': printf ("'d' key\n");   			
		break;
	    case 'n': printf ("'n' key, arg '%s'\n", optval); 	
		nport = atoi (optval);
		break;
	    case 'v':
		if (optval[0]) {
		    printf ("'v' key, arg = '%s'\n", optval); 
		    verbose = atoi (optval);
		} else {
		    printf ("'v' key\n"); verbose = 1;
		}
		break;
	    default:
		printf ("\nDEFAULT\n");
	    }

        } else if (ch == PARG_ERR) {
            return (1);

	} else
            printf ("pos=%d optind=%d argc=%d val='%s'\n", 
		pos, optind, argc, optval);
    }

    printf ("\n\n");
    printf ("final   nport = %d\n", nport);
    printf ("final verbose = %d\n", verbose);


    /*  Free the allocated arguments.
     */
    vo_paramFree (argc, pargv);
}
