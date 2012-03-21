/************************************************************************
 *   Call a SSAP search service and print a summary selected fields of the
 *   results.
 *
 *   Usage:        ssap1 ra dec size [serviceURL]
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
        fprintf (stderr, "Usage: ssap1 [-ds server] ra dec size [ssapURL]\n");
        exit(1);
    }

    /* Now call the SSAP Service and summarize the results.
     */
    callSsapService (service, ra, dec, size);

    return (0);
}


/* Simple test routine to call a SSA service and summarize results.
 */
static void
callSsapService (char *service_url, double ra, double dec, double size)
{
    char *voc_opts = NULL;
    int  i=0, nrec=0, nattr=0;
    char *attrList = NULL, *ip;

    DAL	      ssap;				/* DAL Connection	 */
    Query     query;				/* query handle		 */
    QResponse qr;				/* query response handle */
    QRecord   rec;				/* result record andle   */
	

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

    printf ("Executing Query:\n  %s\n\n", 
	voc_getQueryString (query, SSAP_CONN, 0));
    qr = voc_executeQuery (query); 		    /* execute the query    */


    if ((nrec = voc_getRecordCount (qr)) <= 0) {    /* summarize response   */
        fprintf (stderr, "no records matched");
        exit (1);

    } else {
        rec = voc_getRecord (qr, 0);
        nattr = (rec != ERR) ? voc_getAttrCount (rec) : 0;


	printf ("# returns %d records containing %d attributes each\n#\n",
	    nrec, nattr);
        printf ("# --- Summary output ---\n#\n");

	attrList = voc_getAttrList (rec);
        printf ("# Attribute List:\n#    ");
	for (ip = attrList; *ip; ip++)
	    if (isspace (*ip))
		printf ("\n#    ");
	    else
		putchar (*ip);
        printf ("\n#\n");

        free ((void *) attrList);
    }

    /* Summarize and print selected query results.
     */
    for (i = 0; i < nrec; i++) {
        char *s_ra, *s_dec, *s_naxis, *s_title, *s_fsize;

        rec = voc_getRecord (qr, i);		/* get a row in the table    */

	/* The getStringAttr method returns an allocated pointer to a string
	 * we'll need to free below, however we can use a NULL pointer to 
	 * know when no data were found.
	 */
	s_ra     = voc_getStringAttr (rec, "RA");
	s_dec    = voc_getStringAttr (rec, "DEC");
	s_naxis  = voc_getStringAttr (rec, "Naxis");
	s_title  = voc_getStringAttr (rec, "Title");
	s_fsize  = voc_getStringAttr (rec, "Filesize");
	if (! s_title )
	    s_title  = voc_getStringAttr (rec, "ID_MAIN");   /* fallback     */

        printf ("ra=%-.10s\tdec=%-.10s\t[%s] %9.9s\t%-32.32s\n", 
	    (s_ra ? s_ra : "<null>"), 
	    (s_dec ? s_dec : "<null>"),
	    (s_naxis ? s_naxis : "<null>"),
	    (s_fsize ? s_fsize : "<null>"),
	    (s_title ? s_title : "<null>"));

	if (s_ra)     free ((void *) s_ra);	/* clean up temp pointers    */
	if (s_dec)    free ((void *) s_dec);
	if (s_naxis)  free ((void *) s_naxis);
	if (s_fsize)  free ((void *) s_fsize);
	if (s_title)  free ((void *) s_title);
    }

    voc_closeConnection (ssap);			/* close the ssap connection */
    voc_closeVOClient (1);		        /* clean up and shutdown     */

    return;
}
