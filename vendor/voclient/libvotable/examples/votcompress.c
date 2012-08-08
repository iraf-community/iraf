/**
 *  VOTCOMP 
 *
 *  Example program to "compress" a VOTable by deleting pretty-print
 *  whitespace.
 *
 *    Usage:
 *		votcomp [-o <fname> | '-'] [-i N] <votable>
 *    Where
 *	    -i <N>	    Number of indention spaces (zero by default)
 *	    -o <fname>	    Name of output file (or '-' for stdout)
 *	    <votable>	    Name of file to compress
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "votParse.h"

int	vot	= 0;			/* VOTable handle		*/
int	indent	= 0;			/* indentation flag		*/
char   *fname   = NULL, 		/* input file name		*/
       *oname 	= NULL;			/* output filename		*/


/**
 *  Program entry point.
 */
int main (int argc, char **argv)
{
    /*  Parse the arguments.
     */
    if (argc < 2) {
	fprintf (stderr, 
	    "Usage:  votcomp [-o <fname> | '-'] [-i N] <votable>\n");
	return (ERR);

    } else if (argc >= 2) {
        register int i;

	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-' && strlen (argv[i]) > 1) {
		switch (argv[i][1]) {
		case 'i':    indent = atoi(argv[++i]);   	break;
		case 'o':    oname = argv[++i];   		break;
		default:
		    fprintf (stderr, "Invalid argument '%c'\n", argv[i][1]);
		    return (1);
		}
	    } else
		fname = argv[i];
	}
    }


    /*  Open the table (this also parses it).  In a real application we
     *  would do an access() check on the file, but the open call below will
     *  print error information.
     */
    if ((vot = vot_openVOTABLE (fname)) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", fname);
	return (ERR);
    }

    /*  Output the XML file. */
    vot_writeVOTable (vot, (oname ? oname : "stdout"), indent);

    vot_closeVOTABLE (vot);			/* close the table  	*/
    return (OK);
}
