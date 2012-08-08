/************************************************************************
 *   Call a SSAP search service and print the results as a CSV file.
 *
 *   Usage:        ssap3 ra dec size [serviceURL]
 *
 *   Or call with no args for the built-in unit test.
 *
 *  M. Fitzpatrick, NOAO, June 2006
 */


#include <stdio.h>
#include <stdlib.h>
#include "VOClient.h"

double  ra       = 350.25;			/* default values	*/
double  dec      = -16.4;
double  size     = 0.5;

char   *service  = "http://galex.stsci.edu/gxWS/SSAP/gxSSAP.aspx?";
char   *server   = "6200:localhost";

static void callSsapService (char *url, double ra, double dec, double size);



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
	size = atof (argv[arg++]);
	if (arg < argc)
	    service = argv[arg++];

    } else {
        fprintf (stderr, "Usage: ssap3 [-ds server] ra dec size [ssapURL]\n");
        exit(1);
    }

    /* Now call the Ssap Service and summarize the results.
     */
    callSsapService (service, ra, dec, size);

    return (0);
}


/*  Simple test routine to call a Ssap search service and summarize results.
 */
static void
callSsapService (char *service_url, double ra, double dec, double size)
{
    char *voc_opts = NULL;
    char *vot 	   = NULL;

    DAL	      ssap;				/* DAL Connection	 */
    Query     query;				/* query handle		 */
	

    /*  Initialize the VOClient code.  Error messages are printed by the
     *  interface so we just quit if there is a problem.
     */
    if (voc_initVOClient (voc_opts) == ERR) 
        return;

    /*  Get a new connection to the named service.
     */
    ssap = voc_openSsapConnection (service_url);    /* open a connection    */

    /*  Form a query.  Here we'll use the one search size we're given for
     *  both the RA,DEC sizes, and specify a null format.
     */
    query = voc_getSsapQuery (ssap, ra, dec, size, NULL, NULL, NULL);   

    vot = voc_executeVOTable (query);
    write (fileno(stdout), vot, strlen (vot));

    free ((void *) vot);			/* free local storage	     */

    voc_closeConnection (ssap);			/* close the ssap connection */
    voc_closeVOClient (1);		        /* clean up and shutdown     */

    return;
}
