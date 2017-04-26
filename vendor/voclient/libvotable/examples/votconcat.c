/**
 *  VOTCONCAT 
 *
 *  Example program to concatenate N input tables to a new output table.
 *
 *    Usage:
 *		votconcat [-o <out>] <vot1> <vot2> .....
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

#define	MAX_FILES	1024

char   *infile[MAX_FILES];
int	in_vot[MAX_FILES];
int	in_res[MAX_FILES];

int	nfiles  = 0;			/* Number of input files	*/
int	out	= 0;			/* Output VOTable handle	*/
int	vot	= 0;			/* First VOTable handle		*/
int   	res 	= 0;			/* <RESOURCE> handle 		*/


/**
 *  Program entry point.
 */
int
main (int argc, char **argv)
{
    char  *out_fname = (char *) "stdout";
    int   i, verbose = 0, indent = 1;;


    if (argc < 3) {
	fprintf (stderr, "Usage:  votconcat [-o <out>] <vot1> <vot2> ....\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-' && strlen (argv[i]) > 1) {
		switch (argv[i][1]) {
		case 'i':    indent = atoi (argv[++i]); 	break;
		case 'o':    out_fname = argv[++i]; 		break;
		case 'v':    verbose++; 			break;
		default:
		    fprintf (stderr, "Unrecognized option '%c'\n", argv[i][1]);
		    return (1);
		}
	    } else
		infile[nfiles++] = argv[i];
	}
    }


    /*  Open output table.
     */
    out = vot_openVOTABLE (NULL); 	

    /*  Loop over the input tables on the cmdline.
     */
    for (i=0; i < nfiles; i++) {
        vot = vot_openVOTABLE (infile[i]);	/*  Parse the table	*/

	/*  Concat tables.
 	 */
	if ((res = vot_getRESOURCE (vot)))
            vot_attachNode (out, res); 		

        vot_closeVOTABLE (vot);			/*  Close the tables 	*/
    }

    /*  Write it out.
     */
    vot_writeVOTable (out, out_fname, indent);

    /*  Close (and free) the output table.
     */
    if (out) 
	vot_closeVOTABLE (out);

    return (0);
}
