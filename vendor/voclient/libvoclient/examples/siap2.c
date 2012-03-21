/************************************************************************
 *   Call a SIAP search service and print the results as a CSV file.
 *
 *   Usage:        siap2 ra dec size [serviceURL]
 *
 *   Or call with no args for the built-in unit test.
 *
 *  M. Fitzpatrick, NOAO, June 2006
 */


#include <stdio.h>
#include <stdlib.h>
#include "VOClient.h"

double  ra       = 12.0;			/* default values	*/
double  dec      = 0.0;
double  size     = 0.5;

char   *service  = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?";
char   *server   = "6200:localhost";

static void callSiapService (char *url, double ra, double dec, double size);



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
        fprintf (stderr, "Usage: siap2 [-ds server] ra dec size [siapURL]\n");
        exit(1);
    }

    /* Now call the Siap Service and summarize the results.
     */
    callSiapService (service, ra, dec, size);

    return (0);
}


/*  Simple test routine to call a Siap search service and summarize results.
 */
static void
callSiapService (char *service_url, double ra, double dec, double size)
{
    char *voc_opts = NULL;
    char *csv 	   = NULL;

    DAL	      siap;				/* DAL Connection	 */
    Query     query;				/* query handle		 */
	

    /*  Initialize the VOClient code.  Error messages are printed by the
     *  interface so we just quit if there is a problem.
     */
    if (voc_initVOClient (voc_opts) == ERR) 
        return;

    /*  Get a new connection to the named service.
     */
    siap = voc_openSiapConnection (service_url);    /* open a connection    */

    /*  Form a query.  Here we'll use the one search size we're given for
     *  both the RA,DEC sizes, and specify a null format.
     */
    query = voc_getSiapQuery (siap, ra, dec, size, size, NULL);   

    csv = voc_executeCSV (query);
    write (fileno(stdout), csv, strlen (csv));
    fprintf (stdout, "\n");

    free ((void *) csv);			/* free local storage	     */

    voc_closeConnection (siap);			/* close the siap connection */
    voc_closeVOClient (1);		        /* clean up and shutdown     */

    return;
}
