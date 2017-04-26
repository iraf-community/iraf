/**
 *  VOTCONCAT 
 *
 *  Example program to concatenate two input tables to a new output table.
 *
 *    Usage:
 *		votconcat [-o <out>] <vot1> <vot2>
 *
 *    Where
 *	    <vot1>	First input table	
 *	    <vot2>	Second input table	
 *	    -o <out>	Optional output table, otherwise stdout	
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "votParse.h"


int	vot1	= 0;			/* First VOTable handle		*/
int	vot2	= 0;			/* Second VOTable handle	*/
int	vot3	= 0;			/* Output VOTable handle	*/
int   	res1	= 0, 			/* RESOURCE handles		*/
	res2 	= 0;		

int
main (int argc, char **argv)
{
    char  *out_fname = (char *) NULL, *in1 = NULL, *in2 = NULL;
    int   i, verbose = 0;


    if (argc < 3) {
	fprintf (stderr, "Usage:  votconcat [-o <out>] <vot1> <vot2>\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-' && strlen (argv[i]) > 1) {
		switch (argv[i][1]) {
		case 'o':    out_fname = argv[++i]; 	break;
		case 'v':    verbose++; 		break;
		default:
		    fprintf (stderr, "Unrecognized option '%c'\n", argv[i][1]);
		    return (1);
		}
	    } else {
		if (in1)	/* first table already open	*/
		    in2 = argv[i];
		else
		    in1 = argv[i];
	    }
	}
    }

    vot1 = vot_openVOTABLE (in1);		/*  Parse the files	*/
    vot2 = vot_openVOTABLE (in2);

    res1 = vot_getRESOURCE (vot1); 		/*  Get RESOURCEs 	*/
    res2 = vot_getRESOURCE (vot2);

    vot3 = vot_openVOTABLE (out_fname); 	/*  Open output table   */

    vot_attachNode (vot3, res1); 		/*  Concat tables  	*/
    vot_attachNode (vot3, res2);

    vot_writeVOTable (vot3, "stdout", 1); 	/*  Write it out  	*/

    vot_closeVOTABLE (vot1);			/* Close the tables 	*/
    vot_closeVOTABLE (vot2);		
    if (vot3) 
	vot_closeVOTABLE (vot3);

    return (0);
}
