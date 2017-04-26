/************************************************************************
 * Call a cone/siap search service and print a summary selected fields of the
 * results.
 *
 * Usage:        simple [cone|siap] ra dec sr [type [serviceURL]]
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

char   *cone_service  = \
      		"http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=USNO-B1&";
char   *siap_service  = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?";



int main (int argc, char *argv[])
{
    char *result = NULL;
    char *service = cone_service;
    int  svc_type = CONE_SERVICE;
    int  type = VOC_CSV;
    int  status = 0;

    /* Process command line arguments.
     */
    if (argc <= 1) {
	/* Use builtin defaults. */

    } else if (argc == 2) {
	/* Use builtin defaults with requested service type. */
	if (argv[1][0] == 'c') {
	    svc_type = CONE_SERVICE, service = cone_service;
	} else {
	    svc_type = SIAP_SERVICE, service = siap_service;
	}

    } else if (argc >= 3) {
	int arg = 1;
    
	svc_type = (argv[arg++][0] == 'c' ? CONE_SERVICE : SIAP_SERVICE);
	ra   = atof (argv[arg++]); 		/* parse arguments */
	dec  = atof (argv[arg++]);
	sr   = atof (argv[arg++]);
	if (arg < argc)
	    type = (argv[arg++][0] == 'v' ? VOC_VOTABLE : VOC_CSV);
	if (arg < argc)
	    service = argv[arg++];

    } else {
        fprintf (stderr, "Usage: simpleCone ra dec sr [type [coneURL]]\n");
        exit(1);
    }

    /* Now call the Cone Service and print the results.
     */
    if (svc_type == CONE_SERVICE)
        result = voc_coneCaller (service, ra, dec, sr, type);
    else if (svc_type == SIAP_SERVICE)
        result = voc_siapCaller (service, ra, dec, sr, sr, "image/fits", type);

    if (result)
        write (fileno(stdout), result, strlen (result));  
    else {
	fprintf (stderr, "Error executing query\n");
        status  = 1;
    }

    return (status);
}
