/************************************************************************
 * Call a cone search service and print a summary selected fields of the
 * results.
 *
 * Usage:        cone1 ra dec sr [serviceURL]
 *
 * Or call with no args for the built-in unit test.
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
    int  i=0, nrec=0, nattr=0;
    char *attrList = NULL, *ip;

    DAL	      cone;				/* DAL Connection	 */
    Query     query;				/* query handle		 */
    QResponse qr;				/* query response handle */
    QRecord   rec;				/* result record handle  */
	

    /*  Initialize the VOClient code.  Error messages are printed by the
     *  interface so we just quit if there is a problem.
     */
    if (voc_initVOClient (voc_opts) == ERR) 
        return;

    /*  Get a new connection to the named service.
     */
    cone = voc_openConeConnection (service_url);    /* open a connection    */

    query = voc_getConeQuery (cone, ra, dec, sr);   /* form a query  	    */

    printf ("Executing Query:\n  %s\n\n", 
	voc_getQueryString (query, CONE_CONN, 0));
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
    }

    /* Summarize and print selected query results.
     */
    for (i = 0; i < nrec; i++) {
        char *s_id, *s_ra, *s_dec, *s_class;

        rec = voc_getRecord (qr, i);		/* get a row in the table    */

	/* The getStringAttr method returns an allocated pointer to a string
	 * we'll need to free below, however we can use a NULL pointer to 
	 * know when no data were found.
	 */
	s_id =    voc_getStringAttr (rec, "ID_MAIN");
	s_ra =    voc_getStringAttr (rec, "POS_EQ_RA_MAIN");
	s_dec =   voc_getStringAttr (rec, "POS_EQ_DEC_MAIN");
	s_class = voc_getStringAttr (rec, "CLASS_OBJECT");


        printf ("id=%-16s  ra=%s\tdec=%s\tclass=%s\n", 
	    (s_id ? s_id : "<null>"), 
	    (s_ra ? s_ra : "<null>"), 
	    (s_dec ? s_dec : "<null>"),
	    (s_class ? s_class : "<null>"));

	if (s_id)    free ((void *) s_id);	/* clean up temp pointers    */
	if (s_ra)    free ((void *) s_ra);
	if (s_dec)   free ((void *) s_dec);
	if (s_class) free ((void *) s_class);
    }

    free ((void *) attrList);

    voc_closeConnection (cone);			/* close the cone connection */
    voc_closeVOClient (1);		        /* clean up and shutdown     */

    return;
}
