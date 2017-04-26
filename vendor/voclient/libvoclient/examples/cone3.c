/************************************************************************
 *   Call a cone search service and dump the raw VOTable result.
 *
 *   Usage:        cone3 ra dec sr [serviceURL]
 *
 *   Or call with no args for the built-in unit test.
 *
 *  M. Fitzpatrick, NOAO, June 2006
 */


#include <stdio.h>
#include <stdlib.h>
#include "VOClient.h"

double  ra       = 12.0;			/* default values	*/
double  dec      = 12.0;
double  sr       = 0.1;

#ifdef USNOB
char   *service  = \
      "http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=USNO-B1&";
#else
  char   *service  = "http://chart.stsci.edu/GSCVO/GSC22VO.jsp?";
#endif

char   *server   = "6200:localhost";

static void callConeService (char *url, double ra, double dec, double sr);


int main (int argc, char *argv[])
{
    /* Process command line arguments.
     */
    if (argc <= 1) {
	/* Use builtin defaults. */
    } else if (argc >= 3) {
	int arg = 1;
    
	/* Look for a server specification. */
	if (strncmp (argv[arg], "-ds",3) == 0)
	    server = argv[++arg];
	ra   = atof (argv[arg++]);
	dec  = atof (argv[arg++]);
	sr   = atof (argv[arg++]);
	if (arg < argc)
	    service = argv[arg++];

    } else {
        fprintf (stderr, "Usage: cone1 [-ds server] ra dec sr [coneURL]\n");
        exit(1);
    }

    /* Now call the Cone Service and summarize the results.
     */
    callConeService (service, ra, dec, sr);

    return (0);
}


/*  Simple test routine to call a Cone search service and summarize results.
 */
static void
callConeService (char *service_url, double ra, double dec, double sr)
{
    char *voc_opts = NULL;
    char *vot 	   = NULL;

    DAL	      cone;				/* DAL Connection	 */
    Query     query;				/* query handle		 */
	

    /*  Initialize the VOClient code.  Error messages are printed by the
     *  interface so we just quit if there is a problem.
     */
    if (voc_initVOClient (voc_opts) == ERR) 
        return;

    /*  Get a new connection to the named service.
     */
    cone = voc_openConeConnection (service_url);    /* open a connection    */

    query = voc_getConeQuery (cone, ra, dec, sr);   /* form a query  	    */

    vot = voc_executeVOTable (query);
    write (fileno(stdout), vot, strlen (vot));
    printf ("\n\n");

    free ((void *) vot);			/* free local storage	     */

    voc_closeConnection (cone);			/* close the cone connection */
    voc_closeVOClient (1);		        /* clean up and shutdown     */

    return;
}
