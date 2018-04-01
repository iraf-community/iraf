/**
 *  VOTCOPY -- Copy a VOTable to a different format. 
 *
 *    Usage:
 *		votcopp [-f <fmt>] [-o <fname>] <votable>
 *    Where
 *	    -f <fmt>	    Output format (XML, CSV, TSV, HTML, etc)
 *	    -h 	    	    Print help summary
 *	    -i <N>     	    Indention at each level for VOTable output
 *	    -o <fname>	    Name of output file
 *
 *	    --noheader	    Don't write a header
 *	    --help 	    Print help summary
 *
 *	    <votable>	    Name of input file to compress
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include "votParse.h"


int 	votcopy (int argc, char **argv);

/**
 *  Program Main.  This is just a wrapper around the interface routine.
 */
int
main (int argc, char **argv)
{
    return votcopy (argc, argv);
}



/************************************************************************ 
 *									*
 *  VOTCOPY -- Copy a votable to a new format, converting format if 	*
 *  needed.								*
 *									*
 ************************************************************************/

#define	FORMATS "|vot|asv|bsv|csv|tsv|html|shtml|fits|xml"

#define	VOT	0			/* A new VOTable		*/
#define	ASV	1			/* ascii separated values	*/
#define	BSV	2			/* bar separated values		*/
#define	CSV	3			/* comma separated values	*/
#define	TSV	4			/* tab separated values		*/
#define	HTML	5			/* standalone HTML document	*/
#define	SHTML	6			/* single HTML <table>		*/
#define	FITS	7			/* FITS binary table		*/
#define	XML	8			/* VOTable alias		*/

#define	SZ_FORMAT	32


char   *fmt	= NULL;			/* format string		*/
int	vot	= 0;			/* VOTable root handle		*/
int	indent	= 0;			/* indention at each level	*/
int	ofmt	= CSV;			/* format			*/
int	hdr	= TRUE;


static int strdic (char *in_str, char *out_str, int maxchars, char *dict);


/**
 *  Application entry point.
 */
int
votcopy (int argc, char **argv)
{
    register int i;
    int  stat = OK;
    char   *ifname = NULL, *name = NULL, *ofname = NULL, format[SZ_FORMAT];


    /*  Parse the argument list.
     */
    if (argc < 2) {
	fprintf (stderr, "Usage:  votcopy [-o <fname>] [-f <fmt>] <votable>\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-' && strlen (argv[i]) > 1) {
		switch (argv[i][1]) {
		case 'f':    fmt    = argv[++i];   		break;
		case 'i':    indent = atoi (argv[++i]);		break;
		case 'o':    name  = argv[++i];   		break;
		default:
		    fprintf (stderr, "Invalid argument '%c'\n", argv[i][1]);
		    return (1);
		}
	    } else
		ifname = argv[i];
	}
    }
    ofname = (name ? name : "stdout");


    /* Open and parse the input table.
    */
    if ( (vot = vot_openVOTABLE (ifname) ) <= 0) {
	fprintf (stderr, "Error opening VOTable '%s'\n", ifname);
	return (ERR);
    }

    /*  Output the new format.
     */
    switch (strdic (fmt, format, SZ_FORMAT, FORMATS)) {
    case VOT:   vot_writeVOTable (vot, ofname, indent);     break;
    case ASV:   vot_writeASV (vot, ofname);      	    break;
    case BSV:   vot_writeBSV (vot, ofname);      	    break;
    case CSV:   vot_writeCSV (vot, ofname);      	    break;
    case TSV:   vot_writeTSV (vot, ofname);      	    break;
    case HTML:  vot_writeHTML (vot, ifname, ofname);   	    break;
    case SHTML: vot_writeSHTML (vot, ifname, ofname);  	    break;
    case FITS:  vot_writeFITS (vot, ofname);    	    break;
    case XML:   vot_writeVOTable (vot, ofname, indent);     break;
    default:
	fprintf (stderr, "Unknown output format '%s'\n", format);
	stat = ERR;
    }
    vot_closeVOTABLE (vot);		/* close the table  	*/

    return (stat);
}


/**
 *  STRDIC -- Search a dictionary string for a match with an input string.
 *  The input string may be an abbreviation of a dictionary entry, however,
 *  it is an error if the abbreviation is not unique.  The entries in the
 *  dictionary string are separated by a delimiter character which is the
 *  first character of the dictionary string.  The full name of the matched 
 *  dictionary entry found is returned in out_str; the function value is 
 *  the word index of the dictionary entry.  The output string may be the 
 *  same as the input string.
 */

#include <ctype.h>

static int strdic (
  char	*in_str,		/* Input string, always lower case	*/
  char	*out_str,		/* Output string as found in dictionary */
  int	 maxchars,		/* Maximum length of output string	*/
  char	*dict 			/* Dictionary string			*/
)
{
    char  ch, fch;
    int	  start, len, ip, i, match, entry;


    if (dict == NULL || dict[0] == '\0')
	return (0);

    for (i=0; isspace(in_str[i]); i++)
	;

    start = i;
    match = 0;
    ip    = 1;
    len   = strlen (&in_str[start]);
    fch   = in_str[start];


    /*  Search the dictionary string.  If the input string matches a
     *  dictionary entry it is either an exact match (len = dictionary
     *  entry length) or a legal abbreviation.  If an abbreviation
     *  matches two entries it is ambiguous and an error.
     */
    for (entry=0;  dict[ip] != '\0';  entry=entry+1) {
	if (dict[ip] == fch) {
	    if (strncmp (&dict[ip], &in_str[start], len) == 0) {
		for (i=0;  i < maxchars;  i++) {
		    ch = dict[ip+i-1];
		    if ((ch == dict[0]) || (ch == '\0'))
			break;
		    out_str[i] = ch;
		}
		out_str[i] = '\0';

		if ((dict[ip+len] == dict[0]) || (dict[ip+len] == '\0'))
		    return (entry);		/* exact match		*/
		else {
		    /* If we already have a match and the new match is not
		     * exact, then the abbreviation is ambiguous.
		     */
		    if (match != 0)
		        return (0);
		    else
		        match = entry;
		}
	    }
	}

	do {
	    ip = ip + 1;
	} while (dict[ip-1] != dict[0] && dict[ip] != '\0');
    }

    return (match);
}
