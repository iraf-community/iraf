/************************************************************************
 * Call the SkyBoT minor planet service with the specified parameters.
 *
 * Usage:        skybot <ra> <dec> <sr> <jd_epoch>
 *
 * Or call with no args for the built-in unit test.
 *
 *  M. Fitzpatrick, NOAO, August 2006
 */


#include <stdio.h>
#include <stdlib.h>
#include "VOClient.h"


double	ra	= 0.0,		/* RA of search (J2000)		*/
	dec	= 0.0,		/* Dec of search (J2000)	*/
	sr	= 900.0,	/* Search radius (arcsec)	*/
	epoch	= 2454545.0;	/* JD epoch			*/


int main (int argc, char *argv[])
{
    Skybot  sb = (Skybot) NULL;
    int     i, n;


    /*  Process command line arguments or else use built-in defaults.
     */
    if (argc == 5) {
	ra    = atof (argv[1]);
	dec   = atof (argv[2]);
	sr    = atof (argv[3]);
	epoch = atof (argv[4]);
    }
    printf ("#\n#  SkyBoT Service\n#\n#  Search Terms:\n");
    printf ("#\t ra=%f dec=%f sr=%.2f epoch=%f\n#\n", ra, dec, sr, epoch);

    /*  Now call the SkyBoT Service and summarize the results.   We'll 
     *  let the interface initialize the VO Client server and simply call
     *  the procedure we need.
     */
    sb = voc_skybot (ra, dec, sr, sr, epoch);

    printf ("#  Found %d objects\n#\n", (n = voc_skybotNObjs(sb)) );

    for (i = 0; i < n; i++) {
	printf ("%02d: %-12s  ra=%10.6f  dec=%10.6f  Mv=%.2f  R=%.4f\n", i+1,
	    voc_skybotStrAttr(sb, "name", i),
	    voc_skybotDblAttr(sb, "ra", i),
	    voc_skybotDblAttr(sb, "dec", i),
	    voc_skybotDblAttr(sb, "vmag", i),
	    voc_skybotDblAttr(sb, "cdist", i));
    }

    voc_closeVOClient (1);
    return (0);
}
