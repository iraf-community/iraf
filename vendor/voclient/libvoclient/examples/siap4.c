/************************************************************************
 *   Call a SIAP search service and download resulting datasets.
 *
 *   Usage:        siap4 ra dec size [format [maximages [serviceURL]]]
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
char	*format  = "ALL";
int	maximages= 5;

char   *service1  = "http://www-gsss.stsci.edu/gscvo/DSS2.jsp";
char   *service2  = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?";

char   *server   = "6200:localhost";

static void callSiapService (char *url, double ra, double dec, double size,
    char *format, int maximages);


int main (int argc, char *argv[])
{
    char *service = service2;


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
	if (arg < argc) format    = argv[arg++];
	if (arg < argc) maximages = atoi (argv[arg++]);
	if (arg < argc) service   = argv[arg++];

    } else {
        fprintf (stderr, 
	    "Usage: siap4 [-ds server] ra dec size [fmt [maximg [siapURL]]]\n");
        exit(1);
    }

    /* Now call the Siap Service and summarize the results.
     */
    callSiapService (service, ra, dec, size, format, maximages);

    return (0);
}


/*  Simple test routine to call a Siap search service and summarize results.
 */
static void
callSiapService (char *service_url, double ra, double dec, double size,
    char *format, int maximages)
{
    char *voc_opts = NULL;
    char *acref    = NULL;
    char  fname[SZ_FNAME];
    int   i, nattr = 0, nrec = 0;

    DAL	      siap;				/* DAL Connection	 */
    Query     query;				/* query handle		 */
    QResponse qr;                               /* query response handle */
    QRecord   rec;				/* result record handle	 */
    QRAttribute v;				/* dataset attribute	 */
	

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
    query = voc_getSiapQuery (siap, ra, dec, size, size, format);

    printf ("Executing Query:\n  %s\n\n", 
        voc_getQueryString (query, SIAP_CONN, 0));
    qr = voc_executeQuery (query);                  /* execute the query    */


    /* Summarize response.
     */
    if ((nrec = voc_getRecordCount (qr)) <= 0) {    
        fprintf (stderr, "no records matched");
        exit (1);

    } else {
        rec = voc_getRecord (qr, 0);
        nattr = (rec != ERR) ? voc_getAttrCount (rec) : 0;

        printf ("# returns %d records containing %d attributes each\n#\n",
            nrec, nattr);
    }


    /*  Download the first 'maximages' images.
     */
    printf ("Downloading images:\n");

    for (i=0; i < nrec && i < maximages; i++) {
	rec = voc_getRecord (qr, i);            /* get a row in the table    */

	v = voc_getAttribute (rec, "AccessReference");

	if (v <= 0) 
	    continue;

	bzero (fname, SZ_FNAME);
	sprintf (fname, "dataset.%04d", i);
	printf ("Downloading: %s\n", (acref = voc_stringValue (v)) );

	if ( voc_getDataset (rec, acref, fname) == OK )
	    printf ("Downloaded %s\n", fname);
	else
	    printf ("Download failed\n");
    }

    voc_closeConnection (siap);			/* close the siap connection */
    voc_closeVOClient (1);		        /* clean up and shutdown     */

    return;
}
