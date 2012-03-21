/************************************************************************
 *   Simple test program of a Registry query using the lower-level interface
 *   calls to explicitly form the query.
 *
 *   Usage:    registry2 [flags] [keyword_list]
 *
 *   Or call with no args for the built-in unit test.  The '-o' flag says
 *   to logically OR the keywords
 *
 *   Example usage:
 *
 *	registry1 -siap   		// List all SIAP services
 *	registry1 -cone cool stars	// keyword search on Cone services
 *	registry1 cool stars		// keyword search on all resources
 *
 *
 *   M. Fitzpatrick, NOAO, June 2006
 */


#include <stdio.h>
#include <stdlib.h>
#include "VOClient.h"


#define	ANY	-1
#define	CONE	0
#define SIAP	1
#define TABULAR	2


char    *keywords = "radio galaxies\0";		/* default search terms	*/
int	orValues = 0;
int	svc_type = ANY;


static void callRegistry (char *keywords, int orValues, int type);



int main (int argc, char *argv[])
{
    /* Process command line arguments.
     */
    if (argc <= 1) {
        callRegistry (keywords, orValues, svc_type); 	/* use defaults	*/
        return (0);

    } else if (argc >= 2) {
	int   arg = 1;
	char  keyw[1024];
    
	bzero (keyw, 1024);
	while (arg < argc) {
	    if (strncmp (argv[arg], "-any", 2) == 0)
		svc_type = ANY;
	    else if (strncmp (argv[arg], "-cone", 2) == 0)
		svc_type = CONE;
	    else if (strncmp (argv[arg], "-siap", 2) == 0)
		svc_type = SIAP;
	    else if (strncmp (argv[arg], "-tabular", 2) == 0)
		svc_type = TABULAR;
	    else if (strncmp (argv[arg], "-or", 2) == 0)
	        orValues = 1;
	    else {
	        strcat (keyw, argv[arg]); 
	        if (arg < (argc-1)) strcat (keyw, " ");
	    }
	    arg++;
	}
    
       /* Now call the SIAP Service and summarize the results.
        */
	callRegistry (keyw, orValues, svc_type);
    }

    return (0);
}


/*  Simple procedure to test a Registry query and print some results.
 */
static void
callRegistry (char *keywords, int orValues, int type)
{
    int  i, nresources = -1;
    char  *rtype, *title, *sname;
    char *sql = 
	(type == CONE ?  "ResourceType like '%cone%'" : 
          (type == SIAP ?  "ResourceType like '%siap%'" : 
            (type == TABULAR ?  "ResourceType like '%tabular%'" : NULL) 
	  ) 
	);
    RegQuery  query;
    RegResult res;


    /* Initialize the VO Client interface.
     */
    if (voc_initVOClient ("") == ERR) 
        return;

    printf ("#\n# Search words:  '%s'\n", keywords);
    printf ("# SQL predicate: '%s'\n#\n", sql);

    /* Form the query and do the search.
     */
    query = voc_regQuery (sql, 0);
    if (keywords && keywords[0])
        voc_regAddSearchTerm (query, keywords, orValues);

    /*
    {
	char *qstring;
    	qstring = voc_regGetQueryString(query);
    	printf ("#\n# Query String:\n\n%s\n\n", qstring);
    }
    */

    /*  Now execute the query.
     */
    res = voc_regExecute (query);
    if (res == (RegResult) NULL) {
	fprintf (stderr, "Error executing query, quitting.\n");
        voc_closeVOClient (0);		/* shutdown	*/
	return;
    }

    /* Summarize the results.
     */
    printf ("#\n#Found %d matching records\n#\n\n", 
        (nresources = voc_resGetCount(res) ));

    for (i = 0; i < nresources; i++) {
	title  = voc_resGetStr (res, "Title", i);
	rtype  = voc_resGetStr (res, "ServiceType", i);
	sname  = voc_resGetStr (res, "ShortName", i);

	printf ("----------------------------------------------------------\n");
	printf ("(%d of %d) %s\n", i+1, nresources, title);
	printf ("ShortName:  %-30.30s\tResType:  %s\n", sname, rtype);

	/* Free the pointers returned. */
	if (title) free ((char *)title);
	if (rtype) free ((char *)rtype);
	if (sname) free ((char *)sname);
    }
    printf ("\n\n");

    voc_closeVOClient (0);		/* shutdown	*/
}
