/************************************************************************
**  Registry resolution, query, and browsing
**
**
**    Usage:   vodirectory [-<flags>] [ [<keyword>] | [<term>] ]
**
**      -h,--help                print this help
**      -v,--verbose             verbose mode
**         --vverbose            very-verbose mode
**
**      -c,--count               print a count of matching records
**      -e,--exact               print exactly matching records (resolve)
**      -l,--list                list full resource record
**      -m,--meta                list table metadata
**      -r,--resolve             resolution mode
**      -s,--search              search mode (default)
**
**      -b,--bandpass <bandpass> constrain by bandpass
**      -C,--clevel <level>      constrain by content level
**      -t,--type <type>         constrain by service type
**
**      -a,--all                 print all results (default)
**      -f,--fields <fields>     output only specified fields
**      -o,--or                  logically OR the search terms
**      -n,--index <index>       set index of result (-1 => list, 0 => all)
**
**      -I,--Id                  print only the Identifier (resolve mode)
**      -L,--long                print long lines of output (no wrapping)
**      -R,--Resolve             print the ShortName, ServiceType and ID
**      -S,--SName               print only the ShortName (resolve mode)
**      -T,--Title               print only the Title string (resolve mode)
**
**
**  Resource Type Strings:
**      catalog           Cone search services
**      image             Simple Image Access services
**      spectra           Simple Spectral Access services
**      table             Vizier services
**      <literal>         ResourceType from Registry record
**
**  Allowed Bandpass Strings:
**      Radio              Millimeter           Infrared (IR)
**      Optical            Ultraviolet (UV)     X-Ray (xray)
**      Gamma-Ray (GR)
**
**  Allowed Resource Fields (for -f flag):
**      Title              ShortName            Identifier
**      ServiceURL         ReferenceURL         Description
**      Subject            ResourceType         Type
**      Creator            Publisher            CoverageSpatial
**      CoverageTemporal   Waveband             ContentLevel
**      Version
**
**
**  Original Version:    M. Fitzpatrick, NOAO, Aug 2007
*/

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <getopt.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "VOClient.h"
#include "voAppsP.h"


/* Local processing definitions.
*/
#define M_RESOLVE		001	/* resolve to record field	*/
#define M_SEARCH		002	/* search registry		*/
#define M_FULL			004	/* print full record		*/
#define M_METALIST		010	/* print table metadata		*/

#define	MAX_TERMS		128	/* max search terms		*/

#define TIME_NEW		0	/* time-search types		*/
#define TIME_UPDATE		1

#define TUNIT_HOURS		0	/* units of search		*/
#define TUNIT_DAY		1
#define TUNIT_MONTH		2



int     mode	    = M_SEARCH;		/* program mode			*/


extern int      count;			/* return count of results	*/
extern int	verbose;		/* verbose output?		*/
extern int      meta;			/* print meta results      	*/
extern int      proxy;			/* use proxy server		*/
extern int      res_all;		/* print all results      	*/
extern int      longlines;		/* don't format output		*/
extern int      group;			/* group terms			*/
extern int	exact;			/* exact match only?		*/
extern int	debug;			/* debug output?		*/
extern int      nterms;			/* number of search terms	*/
extern int      status;			/* return status		*/

int     exact       = 0;                /* exact match only?            */
int     res_index   = -1;		/* index of result      	*/
int     nresults    = 0;		/* no. of results      		*/
int     user_fields = 0;		/* uer-defined fields?		*/
int     orValues    = 0;		/* OR search terms?		*/
int     out_votable = 0;		/* output a Resource VOTable	*/
#ifdef REG10_KLUDGE
int     reg10       = 0;		/* Registry 1.0 support?        */
#endif
int     dal_only    = 0;		/* DAL services only           	*/
int     sortRes     = 0;		/* sort results?            	*/
int     terse       = 0;		/* terse results?            	*/
int     cset        = 0;            	/* constraints set?		*/
int     timeSearch  = 0;            	/* search by create/update time */


char   *fields      = (char *) NULL;	/* output fields		*/
char   *ofname      = (char *) NULL;	/* output filename		*/
char   *bandpass    = (char *) NULL;	/* bandpass constraint		*/
char   *subject     = (char *) NULL;	/* subject constraint		*/
char   *stype       = (char *) NULL;	/* service type constraint	*/
char   *clevel      = (char *) NULL;	/* content level constraint	*/
char   *terms[MAX_TERMS];		/* search words/terms		*/




/*  For getopt_long() option parsing.
*/
int     opt_index;
static  char *opt_string = "ab:C:cdef:ghn:IlLmorRs:St:TvVX";

static struct option long_options[] = {
    { "all",      0, 0, 'a'},  		/* print all results		*/
    { "bandpass", 1, 0, 'b'},		/* bandpass constraint 		*/
    { "clevel",   1, 0, 'C'},		/* content level cons.		*/
    { "count",    0, 0, 'c'},		/* count results		*/    
    { "dal",      0, 0, 'd'},		/* DAL-only results		*/    
    { "exact",    0, 0, 'e'},		/* exact results		*/    
    { "fields",   1, 0, 'f'},		/* set output fields		*/
    { "group",    0, 0, 'g'},  		/* group terms			*/
    { "help",     0, 0, 'h'},		/* print help			*/
    { "Id",       0, 0, 'I'},  		/* ID Resolve mode		*/
    { "list",     0, 0, 'l'}, 		/* list full record		*/
    { "long",     0, 0, 'L'},  		/* output long lines		*/
    { "meta",     0, 0, 'm'},		/* print metadata		*/
    { "index",    1, 0, 'n'},		/* result index			*/
    { "or",       0, 0, 'o'},		/* logically OR keyws		*/
    { "resolve",  0, 0, 'r'},  		/* resolve mode			*/
    { "Resolve",  0, 0, 'R'},  		/* Resolve mode			*/
    { "SName",    0, 0, 'S'},  		/* SName Resolve mode		*/
    { "subject",  1, 0, 's'},  		/* subject constraint		*/
    { "Title",    0, 0, 'T'},  		/* Title Resolve mode		*/
    { "type",     1, 0, 't'},		/* svctype constraint		*/
    { "verbose",  0, 0, 'v'},		/* verbose flag			*/
    { "votable",  0, 0, 'V'},		/* output VOTable		*/
    { "xml",      0, 0, 'X'},		/* output XML			*/

    { "new",      1, 0, 'N'},		/* newly registered		*/
    { "updated",  1, 0, 'U'},		/* updated entried		*/

    { "debug",    0, 0, '1'},		/* debug output 		*/
    { "dverb",    0, 0, '2'},		/* verbose debug output 	*/
    { "reg10",    0, 0, '3'},		/* Registry 1.0 kludge		*/

    { "tweet",    0, 0, '7'},		/* tweet output			*/
    { "sort",     0, 0, '8'},		/* */
    { "terse",    0, 0, '9'},		/* */

    { NULL,       0, 0, 0  }
};


/* The following is the full list of metadata available given the
** Registry query method we are using.  This list will change when 
** VOClient moves to a new Registry schema.

char *resList[] = {				// v0.4 output
    "Title", 		"ShortName", 	     "Identifier",
    "ServiceURL", 	"ReferenceURL",      "Description",
    "Subject", 		"ResourceType",      "Type",
    "Creator", 		"Facility", 	     "Instrument",
    "Contributor", 	"CoverageSpatial",   "CoverageTemporal",
    "Waveband",         "ContentLevel",      "Version",
    NULL
};
*/
char *resList[] = {			/* resource fields (full print) */
    "Title", 		    "ShortName", 	"Identifier",
    "CapabilityStandardID", "Subject", 		"Type",
    "ServiceURL", 	    "ReferenceURL",     "Description",
    "Creator", 		    "Publisher",        "CoverageSpatial",
    "CoverageTemporal",     "Waveband",         "ContentLevel",      
    "Version",		    "CapabilityName",
    NULL
};

#define  SZ_GBUF	256
char  gbuf[SZ_GBUF];
/*
char *gfmt = "(ShortName like '%%%s%%' OR Identifier like '%%%s%%')";
char *gfmt = "(Identifier like '%%%s%%')";
*/
char *gfmt = "(Identifier like '%s')";


extern int   errno;
extern int   isSexagesimal (char *s), isDecimal (char *s);
extern int   vot_regResolver (char *id, char *svctype, char *bpass, 
		char *subject, char *fields, int index, int exact, 
		int dal_only, char **result);
extern int   vot_regSearch (char **ids, int nids, char *svctype,
		char *bpass, char *subject, int orValues, int votable, 
		int dal_only, int sortRes, int terse);

static void  print_help (void);
static void  printResolveRecord (void);
static void  printResolveHdr (void);
static void  printFullRecord (char *term);
static void  printSvcMetadata (char *svc);

static char *vot_getTime (int type, char *arg);
double       vot_calJD (int mn, double dy, int yr, int hrs, int min, int sec);

extern void  vot_printRegVOTableHdr (FILE *fd);
extern void  vot_printRegVOTableRec (FILE *fd, RegResult resource, int recnum);
extern void  vot_printRegVOTableTail (FILE *fd);

extern void  pretty_print (char *result, int nresults);
extern void  pretty_print_table (char *result, int nresults, char *fields);
extern void  ppMultiLine (char *result, int poffset, int pwidth, int maxchars);
extern void  ppResSummary (char *result, int nresults);
extern void  vot_printAttrs (char *fname, Query query, char *id);




/************************************************************************
**  Program main()
*/
int
vodirectory (int argc, char *argv[])
{
    register int i=0, ch=0, arg_index=0, narg=0;
    int use_groups = 1;


    /*  Initialize the VOClient code.  Error messages are printed by the
     *  interface so we just quit if there is a problem.
     */
    if (voc_initVOClient ((char *) NULL) == ERR) 
        exit (-1);


    count = 0;
    meta  = 0;

    /*  Process command line arguments. 
     */
    while (1) {
        ch = getopt_long (argc, argv, opt_string, long_options, &opt_index);

        if (ch > 0) {
	    switch (ch) {
 	    case 'h':
		print_help();
    		voc_closeVOClient (0);		/* close VOClient	*/
		exit (0);

            case 'a':    			/* print all results	*/
		res_all = 1;
		break;
            case 'c':				/* count results	*/    
		count++;				
		break;
            case 'd':				/* DAL-only results	*/    
		dal_only++;				
		cset++;				
		break;
	

	    /* EXPERIMENTAL FLAGS */
            case '7':				/* EXPERIMENTAL		*/
		terse += 2;;
		break;
            case '8':				/* EXPERIMENTAL		*/
		sortRes++, terse++;
		break;
            case '9':				/* EXPERIMENTAL		*/
		terse++;
		break;
	    /* EXPERIMENTAL FLAGS */

	
            case 'e':				/* exact results	*/    
		exact++;				
		mode = M_RESOLVE;
		break;
	
            case 'l':    			/* list full record	*/
		mode = M_FULL;
		verbose = 2;
		break;
            case 'I':    			/* ID Resolve mode	*/
		mode = M_RESOLVE;
		fields = "Identifier";
		user_fields++;
		break;
            case 'T':    			/* Title Resolve mode	*/
		mode = M_RESOLVE;
		fields = "Title";
		user_fields++;
		break;
            case 'R':    			/* Resolve mode		*/
		mode = M_RESOLVE;
		dal_only++;				
		fields =
		     "ShortName,CapabilityStandardID,Identifier,CapabilityName";
		user_fields++;
		break;
            case 'S':    			/* SName Resolve mode	*/
		mode = M_RESOLVE;
		fields = "ShortName";
		user_fields++;
		break;
            case 'L':    			/* output long lines	*/
		longlines++;
		break;
            case 'g':    			/* group terms		*/
		mode = M_RESOLVE;
		group++;
		orValues++;
		break;
            case 'r':    			/* resolve mode		*/
		mode = M_RESOLVE;
		break;
            case 'm':
		mode = M_METALIST;
		dal_only++;				
		meta++;
	 	break;

            case 'b':				/* bandpass constraint 	*/
		if (bandpass) {
		    fprintf (stderr,"Warning: Bandpass already specified.");
		    fprintf (stderr," Ignoring '%s'\n", optarg);
		} else
		    bandpass = optarg;
		cset++;				
		break;
            case 's':    			/* subject constraint	*/
		if (subject) {
		    fprintf (stderr,"Warning: Subject already specified.");
		    fprintf (stderr," Ignoring '%s'\n", optarg);
		} else
		    subject = optarg;
		cset++;				
		break;
            case 't':				/* svctype constraint	*/
		if (stype) {
		    fprintf (stderr, "Warning: ServiceType already specified.");
		    fprintf (stderr, " Ignoring '%s'\n", optarg);
		} else
		    stype = optarg;
		cset++;				
		break;
            case 'C':				/* content level cons.	*/
		if (clevel) {
		    fprintf (stderr,"Warning: ContentLevel already specified.");
		    fprintf (stderr," Ignoring '%s'\n", optarg);
		} else
		    clevel = optarg;
		cset++;				
		break;

            case 'n':				/* result index		*/
		res_index = atoi (optarg);
		break;
            case 'o':				/* logically OR keyws	*/
		orValues++;
		break;
            case 'f':				/* set output fields	*/
		fields = optarg;
		user_fields++;
		break;

            case 'V':				/* output VOTable	*/
            case 'X':				/* output XML		*/
		out_votable++;
		verbose = 2;
		break;

            case 'v':				/* verbose flag		*/
		verbose++;
		break;

            case 'N':				/* new resources	*/
		timeSearch++;
		terms[nterms++] = vot_getTime (TIME_NEW, optarg);
		break;
            case 'U':				/* updated entried	*/
		timeSearch++;
		terms[nterms++] = vot_getTime (TIME_UPDATE, optarg);
		break;

						/* "Engineering" flags. */
            case '1':    			/* debug		*/
		debug++;				
		break;
            case '2':    			/* verbose debug	*/
		verbose++;				
		break;
#ifdef REG10_KLUDGE
            case '3':    			/* Registry 1.0 kludge	*/
		reg10++;				
		break;
#endif
            }

        } else {
	    /* Parse the arguments.
	    **
	    **	    vodirectory <keyw> [ <keyw> .... ]
	    **
	    */
	    char  *arg, sql[SZ_SQL_TERM];
	    int   len, start, end;

	    if ( (arg_index = optind + narg) >= argc)
		break;

	    arg = argv[arg_index];
	    len = strlen (arg);
	    if (arg[0] == '\"') {
		/* Capture all args forming a quoted string.
		*/
	 	bzero (sql, SZ_SQL_TERM);
		do {
		    start = ((arg[0] == '\"') ? 1 : 0);
		    end = ((arg[len-1] == '\"') ? len-2 : len-1);
		    strncat (sql, &arg[start], end-start+1);
		    if (arg[len-1] == '\"') 
			break;
		    strcat (sql, " ");
		    arg = argv[++arg_index], narg++;
		    len = strlen (arg);
		} while (arg);
		terms[nterms++] = strdup (sql);

	    } else if (access(arg, R_OK) == 0) {
		FILE *fd;
		char *name = calloc (1, SZ_FNAME);

        	/* Process the file contents. 
        	*/
        	if ((fd = fopen (arg, "r")) == (FILE *) NULL) {
            	    fprintf (stderr, "ERROR: Cannot open file '%s'\n", arg);
            	    exit (1);
        	}
        	while (fgets (name, SZ_FNAME, fd)) {
		    name[strlen(name)-1] = '\0';	/* kill newline */
		    if (group) {
			bzero (gbuf, SZ_GBUF);
			sprintf (gbuf, gfmt, name, name);
		        terms[nterms++] = strdup (gbuf);
		    } else {
		        terms[nterms++] = strdup (name);
		    }
		    bzero (name, SZ_FNAME);
		}

        	fclose (fd);
		free (name);

	    } else {
		if (strncasecmp("ivo://CDS/VizieR", arg, 16) == 0) {
		    char ivorn[SZ_LINE];

		    /* If we're using an 'old-style' Vizier ivorn, try
		    ** converting it to a a Registry 1.0 flavor.  We'll
		    ** worry about further modifying the term if this fails
		    ** later on.
		    */
		    bzero (ivorn, SZ_LINE);
		    strcpy (ivorn, strdup (arg));
		    ivorn[9] = '.';
		    terms[nterms++] = strdup (ivorn);

		} else
		    terms[nterms++] = strdup (arg);
	    }

	    narg++;

	    /* When we're done processing the arguments, break....
	    */
	    if (arg_index == (argc - 1))
		break;
	}
    }

    if (cset && nterms == 0) {
	/* terms[nterms++] = "%"; */
        orValues = 0;
    }


    /* See whether any flags negate other options, or imply values for 
    ** other fields.
    */
    if (res_all)
	res_index = -1;
    if (nterms == 0)
	mode = M_RESOLVE;
    if (fields && mode != M_RESOLVE) {
	fprintf (stderr, "ERROR: 'fields' can only be used in Resolve Mode\n");
        voc_closeVOClient (0);			/* close VOClient connection */
	exit (0);
    }
    if (nterms == 0 && !stype && !bandpass && !clevel && !subject) {
	char *name = calloc (1, SZ_FNAME);

       	/* Process the arguments from the standard input. 
       	*/
        while (fgets (name, SZ_FNAME, stdin)) {
	    name[strlen(name)-1] = '\0';		/* kill newline */
	    terms[nterms++] = strdup (name);
	    bzero (name, SZ_FNAME);
	}
	free (name);
    }
    if (mode == M_RESOLVE)
	sortRes = 0;


    /* If all of the terms appears to be Identfiers, try to optimize the
    ** query, or reset the search mode.
    */
    if (!stype && !clevel && !bandpass && !subject) {

        for (i=0; i < nterms; i++) {
            if (strncmp("ivo:", terms[i], 4) || strncmp("htt:", terms[i], 4)) {
	        use_groups = 0;
	        break;
            }
        }
        if (use_groups && mode == M_SEARCH) 
	    mode = M_RESOLVE;
        if (use_groups && nterms <= 20) {
	    group++; 
	    orValues++;
	    for (i=0; i < nterms; i++) {
	        bzero (gbuf, SZ_GBUF);
	        sprintf (gbuf, gfmt, terms[i], terms[i]);
	        free (terms[i]);
	        terms[i] = strdup (gbuf);
	    }
        }
    }


    /* If we're printing a VOTable, output the prolog.
    */
    if (!meta && out_votable) 
	vot_printRegVOTableHdr (stdout);

    switch (mode) {
    case M_RESOLVE:
	printResolveRecord ();
        voc_closeVOClient (0);			/* close VOClient connection */
        if (!meta && out_votable) 
	    vot_printRegVOTableTail (stdout);
	return ( (nresults == 0) );

    case M_SEARCH:
	if (!count) {
	    nresults = vot_regSearch (terms, nterms, stype, bandpass,
		subject, orValues, out_votable, dal_only, sortRes, terse);
	} else {
	    if (orValues) {
	        for (i=0; i < nterms; i++) {
	            nresults = vot_regSearch (&terms[i], 1, stype, bandpass,
		        subject, 1, out_votable, dal_only, sortRes, terse);
		}
	    } else {
	        nresults = vot_regSearch (terms, nterms, stype, bandpass,
		    subject, 0, out_votable, dal_only, sortRes, terse);
	    }
	}
	break;

    case M_FULL: 
	for (i=0; i < nterms; i++)
	    printFullRecord (terms[i]);
	break;

    case M_METALIST:
	if (nterms == 0) {
	    terms[nterms++] = "%";
	}
	for (i=0; i < nterms; i++)
	    printSvcMetadata (terms[i]);
	break;

    default:
	fprintf (stderr, "Invalid mode.\n");
	exit (1);
    }


    /* Close the VOTable.
    */
    if (!meta && out_votable) 
	vot_printRegVOTableTail (stdout);

    return ( status );
}


/************************************************************************
**  PRINTRESOLVERECORD -- Print a "complete" record for the resources 
**  matching the query string.
*/
static void
printResolveRecord ()
{
    char     *result = (char *)NULL;
    register int i;


    /* Determine the fields we'll need.
    */
    if (verbose == 1 && !fields)
	fields = "ShortName,CapabilityStandardID,Title";
    if (verbose == 2 && !fields)
	fields = "ShortName,CapabilityStandardID,Description";

    if (out_votable)
	fields = "";

    if (out_votable)
	fields = "Title,ShortName,Identifier,ServiceURL,ReferenceURL,CapabilityStandardID,ContentLevel,Type,Waveband,Creator,Subject,Version";

    if (! out_votable)
        printResolveHdr ();
    
    if (nterms == 0) {
	terms[nterms++] = "%";
	exact = 1;
    }

    for (i=0; i < nterms; i++) {
#ifdef REG10_KLUDGE
        if (reg10 || strncasecmp("ivo://CDS.VizieR",terms[i],16) == 0) {
	    char ivorn[SZ_LINE];

            bzero (ivorn, SZ_LINE);
            strcpy (ivorn, terms[i]);
            strcat (ivorn, "%");
            ivorn[9] = '/';

	    nresults = vot_regResolver (ivorn, stype, bandpass, subject, 
		fields, res_index, 0, dal_only, &result);
	} else {
#endif
	    nresults = vot_regResolver (terms[i], stype, bandpass, subject, 
		fields, res_index, exact, dal_only, &result);
#ifdef REG10_KLUDGE
	}
#endif

	if (! out_votable) {
	    if (count) {
	        printf ("%-20s      %3d\t", terms[i], nresults);
	        if (verbose)
	            ppResSummary (result, nresults);
	        printf ("\n");
	    } else if (user_fields)
	        pretty_print_table (result, nresults, fields);
	    else
	        pretty_print (result, nresults);
	}

	if (group)
	    break;
    }
    if (result)
	free ((char *) result);
}


/************************************************************************
**  PRINTRESOLVEHDR -- Print the header information for Resolve mode.
*/
static void
printResolveHdr ()
{
    if (count) {
	if (verbose) {
	    printf ("\nShortName             \t\tMatching Resources\n");
	    printf ("-------------------------------------------");
	    printf ("------------------------------------");
	} else {
	    printf ("\nShortName             Matching Resources\n");
	    printf ("-------------------------------------------");
	}
	printf ("\n");

    } else if (fields && !user_fields) {
	printf("\nShortName           ResourceType\t%s\n",
	    (verbose > 1 ? "Description" : "Title"));
	printf("----------------------------------------");
	printf("----------------------------------------\n");
    }
}


/************************************************************************
**  PRINTFULLRECORD -- Print a "complete" recordfor the resources matching
**  the query string.
*/
static void
printFullRecord (char *term)
{
    RegResult  resource = 0;
    int        i, j, nresults;
    char      *attr_val, sql[SZ_LINE];


    if (debug)
	fprintf (stderr, "printFullRec:  term='%s'\n", term);

    if (res_all)
        sprintf (sql, "(ShortName like '%%%s%%') OR (Identifier like '%%%s%%')",
	    term, term);
    else
        sprintf (sql, "(ShortName like '%s') OR (Identifier like '%s')",
	    term, term);

    resource = voc_regExecute (voc_regQuery (sql, 0));
    nresults = voc_resGetCount (resource);

    if (nresults == 0 && !res_all) {

         if (strncasecmp("ivo://CDS.VizieR",term,16) == 0) {
            char *ip, ivorn[SZ_LINE];

            bzero (ivorn, SZ_LINE);
            strcpy (ivorn, term);
            for (ip = &ivorn[strlen(term)-1]; *ip != '/'; ip--)
                *ip = '\0';
            *ip = '\0';

            sprintf (sql, 
		"(ShortName like '%%%s%%') OR (Identifier like '%%%s%%')",
	    	ivorn, ivorn);
        } else
            sprintf (sql, 
		"(ShortName like '%%%s%%') OR (Identifier like '%%%s%%')",
	    	term, term);

        resource = voc_regExecute (voc_regQuery (sql, 0));
        nresults = voc_resGetCount (resource);

	if (nresults == 0) {
	    fprintf (stderr, "No results found for '%s'\n", term);
	    return;
	}
    }

    for (i=0; i < nresults; i++) {
	if (out_votable) {
	    vot_printRegVOTableRec (stdout, resource, i);

	} else {
            for (j=0; resList[j]; j++) {
	        if ((attr_val = voc_resGetStr (resource, resList[j], i))) {
		    if (strstr (resList[j], "StandardID"))
	                printf ("%16.16s: ", "ResourceType");
		    else
	                printf ("%16.16s: ", resList[j]);
	            ppMultiLine((attr_val ? attr_val : "(none)"), 18, 61, 8192);
	            printf ("\n");
		}
	    }
    	    if (nresults > 1 && i < (nresults - 1))
	        printf ("------------------------------------------\n");
        }
    }
}

	    
/************************************************************************
**  PRINTSVCMETADATA -- Print the column metadata for the named service.
*/
static void
printSvcMetadata (char *svc)
{
    RegResult resource = 0;
    RegQuery  rquery = 0;
    int       i, nresults;
    char      *type, *url, sql[SZ_LINE];
    DAL	      dal;
    Query     query;
 

    if (res_all)
        sprintf (sql, "(ShortName like '%%%s%%') OR (Identifier like '%%%s%%')",
	    svc, svc);
    else
        sprintf (sql, "(ShortName like '%s') OR (Identifier like '%s')",
	    svc, svc);

    if (debug) fprintf (stderr, "metalist sql = '%s'\n", sql);

    rquery = voc_regQuery (sql, 0);

    if (bandpass && bandpass[0])
        voc_regConstWaveband (rquery, bandpass);
    if (stype && stype[0])
        voc_regConstSvcType (rquery, stype);
    if (dal_only) 
        voc_regDALOnly (rquery, dal_only);
    voc_regSortRes (rquery, sortRes);

    resource = voc_regExecute (rquery);
    nresults = voc_resGetCount (resource);

    if (debug) fprintf (stderr, "query nres = %d\n", nresults);

    for (i=0; i < nresults; i++) {
	if ((type = voc_resGetStr (resource, "CapabilityStandardID", i))) {
	    int len;

	    url = voc_resGetStr (resource, "ServiceURL", i);

	    /* Clean up any dangling '&amp;' in the URL.
	    */
	    len = strlen (url);
	    if (strcmp(&url[len-5], "&amp;") == 0)
		url[len-5] = '\0';
	    
	    /* Form a dummy query for the metadata.
	    */
	    if (strcasecmp ("CONE", type) == 0 ||
	        strncasecmp ("TABULAR", type, 7) == 0) {
		    dal = voc_openConeConnection (url);

		    /*  As of 1/28/09 the Vizier SCS doesn't respond properly
		    **  to SR=0 for a metadata query, however if we leave off
		    **  the query params entirely we do get a valid response.
		    **  This is just a kludge.
		    **
		    query = voc_getConeQuery (dal, 0.0, 0.0, 
	        	(strstr (url, "vizier"))  ? -1.0 : 0.0);
		    **		FIXME					    */
		    /*		FIXME					    */

		    /*  This is the standard SCS way to get metadata.  For now
		    **  we'll do as the spec says and broken services will
		    **  have to be fixed.
		    */
		    query = voc_getConeQuery (dal, 0.0, 0.0, 0.001);

        	    (void) voc_addIntParam (query, "VERB",
			(meta ? 3 : min(3,verbose+1)) );

        	    if (debug) {
            	        fprintf (stderr, "Executing Metadata Query:\n  %s\n\n", 
                	    voc_getQueryString (query, CONE_CONN, 0));
		    }

		    /*
	            printf (
			"\n# ---- Svc: %s (%s) ---------------------------\n\n",
		     	svc, type);
		    */
		    vot_printAttrs (NULL, query, svc);

	    } else if (strncasecmp ("SIAP", type, 4) == 0) {
		dal = voc_openSiapConnection (url);
		query = voc_getSiapQuery (dal, 0.0, 0.0, 0.0, 0.0, "METADATA");
        	(void) voc_addIntParam (query, "VERB", verbose);

		/*
	        printf ("\n# ---- Svc: %s (%s) ---------------------------\n\n",
		    svc, type);
		*/
		vot_printAttrs (NULL, query, svc);
	    }
	}
    }
}


/************************************************************************
**  GETLONGLINES -- Utility procedure to return the value of the 'longlines'
**  flag.
*/
int
vot_getLonglines ()
{
    return (longlines);
}


/************************************************************************
**  GETTME -- Cteate a time-constrainst string for the search.
*/
#define	SZ_TSTR		32
#define	SZ_TQUERY	256

static char *
vot_getTime (int type, char *arg)
{
    char  unit = arg[strlen (arg) - 1];
    long  diff, val;
    time_t  now = time((time_t)NULL), then;
    struct tm *start, *end;
    static char  t1[SZ_TSTR], t2[SZ_TSTR], tquery[SZ_TQUERY];


    val = atoi (arg);
    if (!isdigit(unit)) {			/* get units of search 	*/
	if (strchr ("hH", (int) unit))
	    diff = val * 3600;
	else if (strchr ("dD", (int) unit))
    	    diff = val * (3600 * 24);
	else if (strchr ("wW", (int) unit))
    	    diff = val * (3600 * 24 * 7);
	else if (strchr ("mM", (int) unit))
    	    diff = val * (3600 * 24 * 30);
    } else
	diff = val * (3600 * 24);

    then = now - diff;

    memset (t1, 0, SZ_TSTR);
    start = localtime (&then);
    sprintf (t1, "%4d-%02d-%02dT%02d:%02d:%02d",
	(start->tm_year+1900), start->tm_mon+1, start->tm_mday,
	start->tm_hour, start->tm_min, start->tm_sec);

    memset (t2, 0, SZ_TSTR);
    end = localtime (&now);
    sprintf (t2, "%4d-%02d-%02dT%02d:%02d:%02d",
	(end->tm_year+1900), end->tm_mon+1, end->tm_mday,
	end->tm_hour, end->tm_min, end->tm_sec);

    sprintf (tquery,
	"[@%s] >= '%s' AND [@%s] <= '%s'",
	(type == TIME_NEW ? "created" : "updated"), t1,
	(type == TIME_NEW ? "created" : "updated"), t2);
    /*
    sprintf (tquery, "[@%s] >= '%s'",
	(type == TIME_NEW ? "created" : "updated"), t1);
    */
    return ( strdup(tquery) );
}


/* Get JD from date. Not entirely accurate, but good enough for what we need.
*/
double
vot_calJD (int mn, double dy, int yr, int hrs, int min, int sec)
{
    int     a, b, d, m, y;
    long    c;
    double  mjp;


    m = mn;
    y = (yr < 0) ? yr + 1 : yr;
    if (mn < 3)
        m += 12, y -= 1;

    if (yr < 1582 || (yr == 1582 && (mn < 10 || (mn == 10 && dy < 15))))
        b = 0;
    else
        a = y/100, b = 2 - a + a/4;

    c = ((y < 0) ?  
        (long)((365.25*y) - 0.75) - 694025L : (long)(365.25*y) - 694025L);
    d = (int)(30.6001*(m+1));

    mjp = b + c + d + dy - 0.5;

    mjp += (((double)hrs / 24.) + ((double)min / 60.) + ((double)sec / 3600.));
    return (mjp);
}




/************************************************************************
**  PRINT_HELP --  Print a help summary for the task.
*/
static void
print_help()
{
  printf ("\n\
    Usage:   vodirectory [-<flags>] [ [<keyword>] | [<term>] ]\n\
  \n\
      -h,--help                print this help\n\
      -v,--verbose             verbose mode\n\
         --vverbose            very-verbose mode\n\
  \n\
      -c,--count               print a count of matching records\n\
      -e,--exact               print exactly matching records (resolve)\n\
      -l,--list                list full resource record\n\
      -m,--meta                list table metadata\n\
      -r,--resolve             resolution mode\n\
      -s,--search              search mode (default)\n\
  \n\
      -b,--bandpass <bandpass> constrain by bandpass\n\
      -C,--clevel <level>      constrain by content level\n\
      -t,--type <type>         constrain by service type\n\
  \n\
      -a,--all                 print all results (default)\n\
      -f,--fields <fields>     output only specified fields\n\
      -o,--or                  logically OR the search terms\n\
      -n,--index <index>       set index of result (-1 => list, 0 => all)\n\
  \n\
      -I,--Id                  print only the Identifier (resolve mode)\n\
      -L,--long                print long lines of output (no wrapping)\n\
      -R,--Resolve             print the ShortName, ServiceType and ID\n\
      -S,--SName               print only the ShortName (resolve mode)\n\
      -T,--Title               print only the Title string (resolve mode)\n\
  \n\
  \n\
  Resource Type Strings:\n\
      catalog           Cone search services\n\
      image             Simple Image Access services\n\
      spectra           Simple Spectral Access services\n\
      table             Vizier services\n\
      <literal>         ResourceType from Registry record\n\
  \n\
  Allowed Bandpass Strings:\n\
      Radio              Millimeter           Infrared (IR)\n\
      Optical            Ultraviolet (UV)     X-Ray (xray)\n\
      Gamma-Ray (GR)\n\
  \n\
  Allowed Resource Fields (for -f flag):\n\
      Title 		 ShortName 	      Identifier\n\
      ServiceURL 	 ReferenceURL         Description\n\
      Subject 		 ResourceType         Type\n\
      Creator 		 Publisher 	      CoverageSpatial\n\
      CoverageTemporal   Waveband             ContentLevel\n\
      Version\n\
  \n\
  \n\
  \n\
  Examples:\n  ---------\n\n\
  \n\
  1) Get a count of all the SIAP services available in the,\n\
     Registry, then list more information about each:\n\
  \n\
        %% vodirectory --count --type=image\n\
        171\n\
        %% vodirectory -rv -t image\n\
  \n\
  2) Find all catalog (i.e. Cone) services using the search\n\
     words 'radio' and 'galaxies':\n\
  \n\
        %% vodirectory -t catalog radio galaxies\n\
  \n\
  3) Print the full resource record of the GSC2.3 catalog at STScI:\n\
  \n\
        %% vodirectory --list GSC2.3\n\
  ");

  printf ("\n\n");
}
