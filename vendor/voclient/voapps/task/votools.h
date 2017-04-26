/**
 *  VOTOOLS.H -- VOTools Package Declarations.
 *
 *  @file       votools.h
 *  @author     Mike Fitzpatrick
 *  @date       12/13/12
 *
 *  @brief      VOTools Package Declarations.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*  Run as a detached child process?
*/
#define	RUN_DETACHED		0


#define	PKG_NAME		"votools"

#include "voApps.h"

Task  *app    = (Task *) NULL;


/*  Task entry-point declarations.
 */
extern int  vosamp (int argc, char **argv, size_t *len, void **result);

extern int  votcat (int argc, char **argv, size_t *len, void **result);
extern int  votcnv (int argc, char **argv, size_t *len, void **result);
extern int  votget (int argc, char **argv, size_t *len, void **result);
extern int  votinfo (int argc, char **argv, size_t *len, void **result);
extern int  votjoin (int argc, char **argv, size_t *len, void **result);
extern int  votpos (int argc, char **argv, size_t *len, void **result);
extern int  votsort (int argc, char **argv, size_t *len, void **result);
extern int  votsplit (int argc, char **argv, size_t *len, void **result);
extern int  votstat (int argc, char **argv, size_t *len, void **result);

extern int  vosesame (int argc, char **argv, size_t *len, void **result);

extern int  vodata (int argc, char **argv, size_t *len, void **result);
extern int  voatlas (int argc, char **argv, size_t *len, void **result);
extern int  voiminfo (int argc, char **argv, size_t *len, void **result);
extern int  vosloanspec (int argc, char **argv, size_t *len, void **result);

extern int  voregistry (int argc, char **argv, size_t *len, void **result);


/*  Task application table.  Applications must be declared here to be found
 *  and used with the generic main().
 */

Task voApps[] = {
   { "votcat",          votcat      },          /* VOTable apps      	      */
   { "votcnv",          votcnv      },
   { "votget",          votget      },
   { "votinfo",         votinfo     },
   { "votjoin",         votjoin     },
   { "votpos",          votpos      },
   { "votsort",         votsort     },
   { "votsplit",        votsplit    },
   { "votstat",         votstat     },

   { "vosamp",          vosamp      },          /* SAMP messaging  	      */

   { "vosesame",        vosesame    },          /* VO Name Resolution         */

   { "vodata",          vodata      },          /* VO Data access apps        */
   { "voatlas",         voatlas     },
   { "voiminfo",        voiminfo    },
   { "vosloanspec",     vosloanspec },

   { "voregistry",      voregistry  },		/* VO Registry apps	      */
   { NULL,              NULL        }
};


