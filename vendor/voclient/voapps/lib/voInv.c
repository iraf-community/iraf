/**************************************************************************
**
*/

/*
**  ra, dec, radius				// double
**  sources, resources, id, return		// string
**  sourceURL, resourceURL			// file
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef VO_INVENTORY
#include <curl/curl.h>
#include <curl/easy.h>
#include "VOClient.h"
#include "voAppsP.h"



char *base_url = 
    "http://irsa.ipac.caltech.edu/cgi-bin/VOInventory/nph-voInventory";

#define	SUBSET	    "subset"		/* 1 pos, 1 resource		*/
#define	MATCHES	    "matches"		/* N pos, 1 resource		*/
#define	REGION	    "region"		/* 1 pos, [N resources]		*/
#define	TABLE	    "table"		/* N pos, [N resources]		*/


#ifdef UNIT_TEST
int	verbose		= 0;
int	debug		= 0;
#endif
char	*id		= NULL;
char	*action		= NULL;
char	*rettype	= "votable";
double	ra		= 0.0;
double	dec		= 0.0;
double	radius		= 0.1;

char	*ofname		= NULL;
FILE	*outfile	= (FILE *) NULL;

int	nsources	= 1;
int	nresources	= 0;

extern	Object	*objList;
extern	Service	*svcList;
extern  char	*sources;
extern  char	*resources;
extern	double	sr;
extern	int 	format, count, verbose, debug, ecols, nservices, nobjects;
extern	char	*output, *tmpdir;


char   *vot_doInventory ();
char   *vot_execInv (double ra, double dec, double radius, char *sources,
    		     char *resources, char *id, char *rettype, FILE *outfile);

static size_t vot_invWrite (void *ptr, size_t size, size_t nmemb, FILE *stream);
static size_t vot_invRead (void *ptr, size_t size, size_t nmemb, FILE *stream);
static char  *vot_dbl2str (double dval);
static void   vot_printRegionCount (char *file, int extra);
static void   vot_printMatchCount (char *file);

extern char *vot_getTableCol (char *line, int col, int span);
extern void  ppMultiLine (char *result, int poffset, int pwidth, int maxch);



#ifdef UNIT_TEST
int 
main (int argc, char *argv[])
{
    register int i, j, len;

    CURL 	*curl;
    CURLcode 	res;
    struct curl_httppost *form = NULL;
    struct curl_httppost *last = NULL;


    for (i=1; i < argc; i++) {
        if (argv[i][0] == '-') {
            len = strlen (argv[i]);
            for (j=1; j < len; j++) {
                switch (argv[i][j]) {
                case 'h':    			/* help			*/
                    return (0);
                case 'd':    			/* debug		*/
		    debug++;
		    break;
                case 'v':    			/* verbose		*/
		    verbose++;
		    break;
                case 't':    			/* test			*/
		    base_url = "http://iraf.noao.edu/scripts/tpost";
		    break;
                case 'p':    			/* pos (ra dec)		*/
		    ra = atof (argv[i+1]);
		    dec = atof (argv[i+2]);
		    i += 2;
		    nsources = 1;
		    break;
                case 'r':    			/* radius		*/
		    radius = atof (argv[++i]);
		    break;
                case 'i':    			/* id			*/
		    id = argv[++i];
		    break;
                case 'o':    			/* output file		*/
		    ofname = argv[++i];
        	    outfile = fopen (ofname, "w+");
		    break;
                case 'R':    			/* resource file	*/
		    resources = argv[++i];
		    nresources = 2;
		    break;
                case 'S':    			/* source file		*/
		    sources = argv[++i];
		    nsources = 2;
		    break;

                case 'A': 	rettype = "ascii"; 	break;
                case 'C': 	rettype = "csv"; 	break;
                case 'H': 	rettype = "HTML"; 	break;	/* BROKE */
                case 'T': 	rettype = "tsv"; 	break;
                case 'V': 	rettype = "votable"; 	break;
		default:
		    break;
		}
	    }
	}
    }


    if (debug) {
        fprintf (stderr, "pos = (%f,%f)  radius = %f\n", ra, dec, radius);
        fprintf (stderr, "id = '%s'\n", id);
        fprintf (stderr, "sources = '%s'  N = %d\n", sources, nsources);
        fprintf (stderr, "resources = '%s'  N = %d\n", resources, nresources);
    }

    (void) vot_execInv (ra, dec, radius, sources, resources, id, rettype,
	NULL);

    return 0;
}
#endif


/*  VOT_EXECINV -- Execute the inventory service call.
*/
char * 
vot_doInventory ()
{
    double ra  = objList->ra, dec = objList->dec, radius = sr;
    char  *id = (svcList ? svcList->identifier : NULL),
	  *rettype = "tsv", *action;
    char   tmpfile[SZ_LINE];
    FILE  *fd = (FILE *) NULL;

    extern char *vot_mktemp();


    /* Get a temporary file to be used for the votable output.
    */
    strcpy (tmpfile, (debug ? "/tmp/vod.tmp" : vot_mktemp ("vodi")));

    if (! (fd = fopen (tmpfile, "w+")) ) {
	fprintf (stderr, "Cannot open output file '%s'\n", tmpfile);
	exit (1);
    }

    if (debug) {
	fprintf (stderr, "ra '%f' '%f'  radius '%f'....\n", ra, dec, radius);
	fprintf (stderr, "id '%s'   rettype '%s'....\n", id, rettype);
	fprintf (stderr, "sources '%s'   resources '%s'....\n", 
	    sources, resources);
	fprintf (stderr, "using file '%s'....\n", output);
    }

    /* Execute the query.
    vot_execInv (ra, dec, radius, sources, resources, id, "tsv", fd);
    */
    action = vot_execInv (ra, dec, radius, sources, resources, id, "tsv", fd);

    if (fd != stdout) {
	fclose (fd);

	if (debug) {
	    system("cat /tmp/vod.tmp"); 
	    printf ("\n\n");
	}

	if (count) {
	    if (strcmp (action, "region") == 0)
	        vot_printRegionCount (tmpfile, 0);
	    else if (strcmp (action, "table") == 0)
	        vot_printRegionCount (tmpfile, 1);
	    else if ((strcmp (action, "matches")) ||
	             (strcmp (action, "subset") == 0)) {
	        	vot_printMatchCount (tmpfile);
	    }
	}
    }

    if (!debug)
        unlink (tmpfile);

    return (action);
}


/*  VOT_EXECINV -- Execute the inventory service call.
*/
char * 
vot_execInv (double ra, double dec, double radius, char *sources,
    char *resources, char *id, char *rettype, FILE *outfile)
{
    CURL 	*curl;
    CURLcode 	res;
    struct curl_httppost *form = NULL;
    struct curl_httppost *last = NULL;



    nresources = nservices;
    nsources   = nobjects;

    /* Initialize the CURL call.
    */
    curl_global_init (CURL_GLOBAL_ALL);

    if ( (curl = curl_easy_init()) ) {

	struct curl_slist *headerlist = NULL;

    
        /* Fill in the fields. 
        */
        if (radius > 0.0) {
    	    curl_formadd (&form, &last, CURLFORM_COPYNAME, "radius",
               CURLFORM_COPYCONTENTS, vot_dbl2str(radius), CURLFORM_END);
    	    curl_formadd (&form, &last, CURLFORM_COPYNAME, "units",
               CURLFORM_COPYCONTENTS, "degree", CURLFORM_END);
	}

	if (debug)
	    fprintf (stderr, "\n\nnsources=%d  nresources=%d\n\n",
		nsources, nresources);

        switch ( nsources ) {
        case 0:
	    perror ("Invalid NSources=0, no src file or posn specified\n");
	    exit(1);
        case 1:
    	    curl_formadd (&form, &last, CURLFORM_COPYNAME, "ra",
               CURLFORM_COPYCONTENTS, vot_dbl2str(ra), CURLFORM_END);
    	    curl_formadd (&form, &last, CURLFORM_COPYNAME, "dec",
               CURLFORM_COPYCONTENTS, vot_dbl2str(dec), CURLFORM_END);
	    if (id) {
	        action = "subset";
    	        curl_formadd (&form, &last, CURLFORM_COPYNAME, "id",
                    CURLFORM_COPYCONTENTS, id, CURLFORM_END);
	    } else if (resources)
	        action = "table";
	    else
	        action = "region";
	    break;
        default:
            if (sources) {
                curl_formadd (&form, &last, CURLFORM_COPYNAME, "sources",
                    CURLFORM_FILE, sources, CURLFORM_END);
	        if (nresources < 0)
	            action = "table";
	        else if (nresources == 1 && id)
	           action = "matches";
	        else if (resources)
	           action = "table";
	    } else {
	        perror ("Invalid nsources=N, no source file specified\n");
	        exit (1);
	    }
	    break;
        }


        /* Set the matching resources.
        */
        if (nresources == 1 && id) {
    	    curl_formadd (&form, &last, CURLFORM_COPYNAME, "id",
               CURLFORM_COPYCONTENTS, id, CURLFORM_END);

        } else if (nresources >= 1 && resources) {
            curl_formadd (&form, &last, CURLFORM_COPYNAME, "resources",
               	CURLFORM_FILE, resources, 
               	CURLFORM_CONTENTTYPE, "text/xml", 
		CURLFORM_END);

        } else if (nresources > 0) {
	    perror ("Invalid NResources=2, no resource file specified\n");
	    exit (1);
        }


        /* Make sure we have a valid action to execute.
        */
        if (action) {
    	    curl_formadd (&form, &last, CURLFORM_COPYNAME, "action",
               CURLFORM_COPYCONTENTS, action, CURLFORM_END);
    	    curl_formadd (&form, &last, CURLFORM_COPYNAME, "searchType",
               CURLFORM_COPYCONTENTS, action, CURLFORM_END);
        } else {
	    perror ("No action specified.");
	    exit (1);
        }
    	curl_formadd (&form, &last, CURLFORM_COPYNAME, "return",
           CURLFORM_COPYCONTENTS, rettype, CURLFORM_END);


        /* Print some debug info.
        */
	if (debug) {
	    fprintf (stderr, "ACTION = '%s'  ret = '%s'\n", action, rettype);	

            curl_easy_setopt (curl, CURLOPT_VERBOSE, 1);
            curl_easy_setopt (curl, CURLOPT_HEADER, 1);
 	}

	/* Setup the output file, if we have one.
	*/
	if (outfile) {
	    curl_easy_setopt(curl, CURLOPT_WRITEDATA, outfile);
	    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, vot_invWrite);
	    curl_easy_setopt(curl, CURLOPT_READFUNCTION, vot_invRead);
	}

        /* Setup the call to the base URL as an HTTP/POST.
	headerlist = curl_slist_append (headerlist, expect);
        */
        curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headerlist);

        curl_easy_setopt (curl, CURLOPT_HTTPPOST, form);
        curl_easy_setopt (curl, CURLOPT_URL, base_url);

        /* Execute the query. 
        */
        res = curl_easy_perform (curl);

        curl_slist_free_all (headerlist);
    }

    curl_easy_cleanup (curl); 		/* always cleanup 		    */
    if (form)
        curl_formfree (form); 		/* then cleanup the formpost chain  */
    if (outfile)
        fclose (outfile);

    return (action);
}


/*
*/
static void
vot_printRegionCount (char *file, int extra)
{
    FILE  *fd;
    int   c_ivoid=1, c_sname=2, c_title=8, c_count=11, c_nrec=10, c_desc=9;
    int   c_corr=12;
    char  title[SZ_LINE], count[SZ_LINE], corr[SZ_LINE],
	  nrec[SZ_LINE], line[SZ_LINE], desc[SZ_LINE], *s;

    extern char *delim;


    delim = "\t";
    if ((fd = fopen (file, "r"))) {

	fgets (line, SZ_LINE, fd);		/* skip header */

	printf ("\n");
	while (fgets (line, SZ_LINE, fd)) {
	    strcpy (title, ((s=vot_getTableCol(line, c_title, 1))?s:" "));
	    strcpy (count, ((s=vot_getTableCol(line, c_count, 1))?s:" "));
	    strcpy (nrec,  ((s=vot_getTableCol(line, c_nrec,  1))?s:" "));
	    if (extra)
	        strcpy (corr,  ((s=vot_getTableCol(line, c_corr,  1))?s:" "));

	    if (verbose > 1) {
	        s = vot_getTableCol(line, c_sname, 1);
		printf ("   ShortName:  %s\n", s);

	        s = vot_getTableCol(line, c_ivoid, 1);
		printf ("  Identifier:  %s\n", s);

		printf ("       Title:  "); ppMultiLine (title, 15, 64, 1024);
	        printf ("\n");

	        strcpy (desc, ((s=vot_getTableCol(line, c_desc, 1))?s:" "));
		printf (" Description:  "); ppMultiLine (desc, 15, 64, 1024);
	        printf ("\n");

		printf ("       Count:  %s of %s \n", count, nrec);

		if (extra)
		    printf ("        Corr:  %s\n", corr);
	        printf ("-------------------------------------\n");

	    } else {
	        s = vot_getTableCol(line, c_ivoid, 1);
        	printf ("  %-20.20s  %4s    %s  ", s, count, "C"); 

	        s = (vot_getTableCol (line, c_title, 1))?s:" ";
		ppMultiLine (s, 35, 45, 1024);
		if (extra)
	            printf (" (Corr=%s)", corr);
	        printf (" (NRec=%s)\n", nrec);
	    }

	    bzero (line, SZ_LINE);
	}

    } else {
	fprintf (stderr, "Cannot open votable '%s'\n", file);
	exit (1);
    }
}


/*
*/
static void
vot_printMatchCount (char *file)
{
    FILE  *fd;
    int   nrows = 0;
    char  line[SZ_LINE];


    if ((fd = fopen (file, "r"))) {

	fgets (line, SZ_LINE, fd);		/* skip header */

	printf ("\n");
	while (fgets (line, SZ_LINE, fd))
	    nrows++;
    }

    if (verbose > 1) {
	printf ("   ShortName:  %s\n", svcList->name);
	printf ("  Identifier:  %s\n", svcList->identifier);
	printf ("       Title:  "); 
	    ppMultiLine (svcList->title, 15, 64, 1024);
	printf ("\n");
	printf ("       Count:  %d of %d objects from '%s' matched\n",
		nrows, nobjects, sources);
        printf ("-------------------------------------\n");

    } else {
        printf ("  %-20.20s  %4d    %s  ",
                svcList->name, nrows, "C"); 
	ppMultiLine(svcList->title,35,45,1024);
        printf ("\n");
    }
}



/*  Local utility functions.
*/

static size_t 
vot_invWrite (void *ptr, size_t size, size_t nmemb, FILE *stream)
{
    return fwrite (ptr, size, nmemb, stream);
}

static size_t 
vot_invRead (void *ptr, size_t size, size_t nmemb, FILE *stream)
{
    return fread(ptr, size, nmemb, stream);
}


static char *
vot_dbl2str (double dval)
{
    static char val[SZ_LINE];

    bzero (val, SZ_LINE);
    sprintf (val, "%f", dval);

    return (val);
}

#endif
