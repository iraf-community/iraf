/**
 *  VOAPPS.C -- Generic task main() for the VOClient Package applications.
 *
 *  @file       voApps.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      Generic task main() for the VOClient Package applications.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*  Run as a detached child process?
*/
#define	RUN_DETACHED		0


#include "voApps.h"

Task  *app    = (Task *) NULL;


/*  Task entry-point declarations.
 */
extern int  vosamp (int argc, char **argv, size_t *len, void **result);

/***  NOT YET IMPLEMENTED  ***
extern int  votcat (int argc, char **argv, size_t *len, void **result);
extern int  votjoin (int argc, char **argv, size_t *len, void **result);
extern int  votselect (int argc, char **argv, size_t *len, void **result);
extern int  votsplit (int argc, char **argv, size_t *len, void **result);
****/
extern int  votcnv (int argc, char **argv, size_t *len, void **result);
extern int  votget (int argc, char **argv, size_t *len, void **result);
extern int  votinfo (int argc, char **argv, size_t *len, void **result);
extern int  votpos (int argc, char **argv, size_t *len, void **result);
extern int  votsort (int argc, char **argv, size_t *len, void **result);
extern int  votstat (int argc, char **argv, size_t *len, void **result);

extern int  vosesame (int argc, char **argv, size_t *len, void **result);

extern int  voatlas (int argc, char **argv, size_t *len, void **result);
extern int  vocatalog (int argc, char **argv, size_t *len, void **result);
extern int  vodata (int argc, char **argv, size_t *len, void **result);
extern int  voimage (int argc, char **argv, size_t *len, void **result);
extern int  voiminfo (int argc, char **argv, size_t *len, void **result);
extern int  vospectra (int argc, char **argv, size_t *len, void **result);
extern int  votopic (int argc, char **argv, size_t *len, void **result);

extern int  voregistry (int argc, char **argv, size_t *len, void **result);


/*  Task application table.  Applications must be declared here to be found
 *  and used with the generic main().
 */

Task voApps[] = {
   /***  NOT YET IMPLEMENTED  ****
   { "votcat",          votcat      },
   { "votjoin",         votjoin     },
   { "votselect",       votselect   },
   { "votsplit",        votsplit    },
   ****/

   { "votcnv",          votcnv      },          /* VOTable apps      	      */
   { "votget",          votget      },
   { "votinfo",         votinfo     },
   { "votpos",          votpos      },
   { "votsort",         votsort     },
   { "votstat",         votstat     },

   { "vosamp",          vosamp      },          /* SAMP messaging  	      */

   { "vosesame",        vosesame    },          /* VO Name Resolution         */

   { "voatlas",         voatlas     },          /* VO Data access apps        */
   { "vocatalog",       vocatalog   },
   { "vodata",          vodata      },
   { "voimage",         voimage     },
   { "voiminfo",        voiminfo    },
   { "vospectra",       vospectra   },
   { "votopic",         votopic     },

   { "voregistry",      voregistry  },		/* VO Registry apps	      */
   { NULL,              NULL        }
};



/**
 *  Program main().
 */
int
main (int argc, char *argv[])
{
    char  *task   = (char *) NULL;
    int    status = 0;
    size_t reslen = 0;
    void  *result = (void *) NULL;


    /*  Get the task name minus any leading path prefix.
     */
    task = argv[0] + strlen (argv[0]) - 1;
    while (task > argv[0]) {
	if ( *(task - 1) != '/')
	    task--;
	else
	    break;
    }

    /*  Loop through the application table.  If the names match, call
     *  the entry-point function.
     */
    if (getenv ("VOAPP_CONNECTED") || (! RUN_DETACHED) ) {
	for (app = voApps; app->name; app++) {
	    if (strcmp (app->name, task) == 0) {
	        status = (*app->func) (argc, argv, &reslen, &result);
		break;
	    }
	}
    } else {
	/*  Run as a detached sub-process.
	 */
        status = vo_runTask (task, voApps, argc, argv, &reslen, &result);
    }


    if (status && result)		/* print the error message */
	fprintf (stderr, "%s\n", (char *) result);
    if (reslen && result)		/* free result pointer	   */
	free (result);

    return (status);
}
