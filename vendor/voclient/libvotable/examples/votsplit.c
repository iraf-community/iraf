/**
 *  VOTGET -- Download all access references in a VOTable.
 *
 *  Usage:
 *
 *	votget [-b <base>] [-c <col>] [-u <ucd>] [-v] [-x] [-o fname] <votable>
 *
 *  Where
 *	    -b <base>		base output filename
 *	    -c <col>		col number to use as acref column (0-indexed)
 *	    -u <ucd>		use ucd to identify acref column
 *	    -t <type>  		Download only specified <type>
 *
 *	    -N <N> 		Number of simultaneous downloads
 *	    -D  		Detach, i.e. run in forked child process
 *
 *	    -o <fname> 		output filename (or 'stdout' or '-')
 *	    -x 			Extract access references
 *	    -v 			Verbose output
 *
 *	    <votable>		Name of file to dump, or '-' for stdin
 */


int     votget (int argc, char **argv);

/**
 *  Program Main.  This is just a wrapper around the interface routine.
 */
int
main (int argc, char **argv)
{
    return votget (argc, argv);
}



/************************************************************************ 
 *                                                                      *
 *  VOTGET -- Download the access references in a VOTable.              *
 *                                                                      *
 ************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <pthread.h>

#include <curl/curl.h>
#include <curl/types.h>
#include <curl/easy.h>

#include "votParse.h"


#define	SZ_FNAME	256
#define	SZ_URL		4096
#define	MIN_THREADS	4
#define	MAX_THREADS	64
#define	MAX_DOWNLOADS	4096

#define NAXIS_UCD   	"VOX:Image_Naxis"
#define NAXES_UCD   	"VOX:Image_Naxes"
#define SCALE_UCD   	"VOX:Image_Scale"
#define ACREF_UCD   	"VOX:Image_AccessReference"
#define FORMAT_UCD   	"VOX:Image_Format"


int	vot	  = 0;			/* VOTable handle	     	*/
int	verbose	  = 0;			/* verbose parameter	     	*/
int	debug	  = 0;			/* debug flag	     		*/
int	extract	  = 0;			/* extract references only	*/
int	detach	  = 0;			/* run as detached process	*/
int	nfiles    = 0;			/* number of download files	*/
int	ngot 	  = 0;			/* number of files downloaded	*/
int	nthreads  = 1;			/* number of download threads	*/
int	acol	  = -1;			/* access reference column 	*/
int	tcol	  = -1;			/* image type column 		*/

char   *base	  = "file";		/* output base filename    	*/
char   *afname 	  = NULL;		/* output acref filename	*/
char   *acref 	  = NULL;		/* acref url 		     	*/
char   *acref_ucd = NULL;		/* acref UCD 		     	*/
char   *fmt 	  = NULL;		/* image format 	     	*/
char   *fmt_ucd   = NULL;		/* image format UCD 	     	*/

FILE  *afd = (FILE *) NULL;		/* acref file descriptor	*/

pthread_mutex_t counter_mut = PTHREAD_MUTEX_INITIALIZER;


typedef struct {
    char   url[SZ_URL];			/* access URL			*/
    char   fname[SZ_URL];		/* local filename		*/
    int    tnum;			/* worker thread number		*/
} Acref, *AcrefP;

Acref   aclist[MAX_DOWNLOADS];		/* access list			*/


/*  Private methods.
 */
static int   vot_isVOTable (char *infile);
static int   vot_acrefColumn (handle_t tab);
static int   vot_typeColumn (handle_t tab);
static int   vot_getURL (char *url, char *ofname);
static int   vot_loadText (char *infile);
static int   vot_loadVOTable (char *infile);
static void  vot_saveAcref (char *acref, int num);
static void *vot_getAclist (void *arg);

static void  vot_printAclist ();


/**
 *  Program entry point.
 */
int
votget (int argc, char **argv)
{
    int   i, stat = OK;
    char  *fname;


    if (argc < 2) {
	fprintf (stderr, "Usage:  votget [-b <base>] <votable>\n");
	return (1);

    } else if (argc >= 2) {
	for (i=1; i < argc; i++) {
	    if (argv[i][0] == '-' && strlen (argv[i]) > 1) {
		switch (argv[i][1]) {
		case 'f':    fmt = argv[++i];			break;
		case 'u':    acref_ucd = argv[++i];		break;

		case 'b':    base = argv[++i]; 			break;
		case 'o':    afname = argv[++i];		break;
		case 'x':    extract++;				break;
		case 'v':    verbose++;				break;

		case 'A':    acol = atoi(argv[++i]); 		break;
		case 'D':    detach++;				break;
		case 'N':    nthreads = atoi(argv[++i]); 	break;
		case 'T':    tcol = atoi(argv[++i]); 		break;
		default:
		    fprintf (stderr, "Invalid argument '%c'\n", argv[i][1]);
		    return (1);
		}
	    } else if (argv[i][0] == '+' && strlen (argv[i]) > 1) {
		switch (argv[i][1]) {
		case 'd':    debug++;				break;
		}
	    } else
		fname = argv[i];
	}
    }


    /*  Setup defaults and initialize.
     */
    memset (&aclist[0], 0, MAX_DOWNLOADS);
    if (!fmt_ucd)
	fmt_ucd = FORMAT_UCD;
    if (!acref_ucd)
	acref_ucd = ACREF_UCD;

    if (afname && (afd = fopen (afname, "w+")) == (FILE *) NULL) {
	if (verbose)
	    fprintf (stderr, "Error: cannot open output file '%s'\n", afname);
	return (ERR);
    }


    /*  Determine the type of input file.
     */
    switch ((vot = vot_isVOTable (fname))) {
    case -1:  	stat = ERR; 			goto done_;
    case  0:    vot_loadText (fname); 		break;
    case  1:    vot_loadVOTable (fname); 	break;
    }


    /*  If all we're doing is extracting the URLs we can quit now.
     */
    if (extract)
        goto done_;

    if (debug) {
	fprintf (stderr, "acol = %d   tcol = %d\n", acol, tcol);
	fprintf (stderr, "Downloading %d files ....\n", nfiles);
	vot_printAclist ();
    }


    /*  If we've been asked to detach, fork off to do the downloads in
     *  a child, and return to the caller.
     */
    if (detach) {
	pid_t  pid;

        switch ((pid = fork ())) {
        case -1:  return (ERR);			/* We are an error      */
        case 0:   break;			/* We are the child     */
        default:  return (OK);			/* We are the parent    */
        }
    }

    /*  Do the downloads.
     */
    if (nfiles < MIN_THREADS)
	nthreads = nfiles;

    if (nthreads == 1) {
	vot_getAclist (NULL);

    } else {
	/* Spawn the worker threads.
	 */
	int  rc = 0, tc = 0, status = 0, tnum[MAX_THREADS];
	pthread_attr_t  attr;               	/* thread attributes 	*/
	pthread_t  thread[MAX_THREADS];


        /*  Initialize the service processing thread attributes and run 'em.
         */
        pthread_attr_init (&attr);
        pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_JOINABLE);

        if (verbose)
	    fprintf (stderr, "Starting download ....\r");

        for (tc=0; tc < nthreads; tc++) {
	    tnum[tc] = tc;
            if ((rc = pthread_create (&thread[tc], &attr, vot_getAclist,
                (void *)&tnum[tc]))) {
                    fprintf (stderr, "ERROR: pthread_create() fails, code=%d\n",
			rc);
                    exit (-1);
            }
        }

        /* Free attribute and wait for the threads to complete.
        */
        pthread_attr_destroy (&attr);
        for (tc=0; tc < nthreads; tc++) {
            if ((rc = pthread_join (thread[tc], (void **)&status)) ) {
		if (rc != ESRCH) {
                    fprintf (stderr, 
		        "ERROR: pthread_join() fails, code=%d status=%d\n", 
		        rc, status);
                    exit (-1);
		}
            } 
        }

        if (verbose) {
	    fprintf (stderr, "Downloaded %d files -- Download complete\n", 
		nfiles);
	    fflush (stderr);
        }
    }


    /*  Close the table and clean up.
     */
done_:

    return (OK);
}


/**********************************************************************
**  Private Procedures
**********************************************************************/


/**
 *  VOT_LOADTEXT -- Load the access list from a text file.  We assume the 
 *  list is simply one url per line.
 */
static int
vot_loadText (char *infile)
{
    int   i = 0, fd, nread = 0, tnum = 0, sz = 0;
    char  *acref, *buf, *ip;
    struct stat info;

    if ((fd = open (infile, O_RDONLY)) < 0)
	return (-1);

    if (stat (infile, &info) < 0)		/* read the whole file	*/
	return (-1);
    sz = info.st_size;
    buf = calloc (sz + 1, sizeof(char));
    nread = read (fd, buf, sz);
    close (fd);

    acref = buf;				/* point to 1st url	*/
    for (i=0; *acref; i++) {
	for (ip=acref; *ip && *ip != '\n'; ip++)
	    ;
	*ip = '\0';

	vot_saveAcref (acref, i);

	acref = buf + 1;
	nfiles++;
	tnum++;
    }

    if (afd)					/* close the acref file	*/
	fclose (afd);

    return (nfiles);
}


/**
 *  VOT_LOADVOTABLE -- Load the access list from a VOTable.
 */
static int
vot_loadVOTable (char *infile)
{
    int   i, tnum = 0;
    int   res, tab, data, tdata, tr;
    char  *acref;


    /*  Open the table.  This also parses it.
     */
    if ( (vot = vot_openVOTABLE (infile) ) <= 0) {
	if (verbose)
	    fprintf (stderr, "Error opening VOTable '%s'\n", infile);
	return (ERR);
    }

    /*  Loop over all the resources in the file.  In most cases there will
     *  only be one <RESOURCE>, if not then the selection applies to all 
     *  valid tables.
     */
    for (res = vot_getRESOURCE (vot); res; res = vot_getNext (res)) {

	/*  Get the <TABLE> element.
	 */
        if (! (tab = vot_getTABLE (res))) {
	    if (verbose) fprintf (stderr, "Error: No <TABLE> in <RESOURCE>\n");
	    continue;
        }
        data  = vot_getDATA (tab);
        tdata = vot_getTABLEDATA (data);

        /*  Loop through the FIELDs to find the acref.  Let the cmdline param
         *  override the acref column ucd.
         */
	acol = (acol < 0 ? vot_acrefColumn (tab) : acol);
	tcol = (tcol < 0 ? vot_typeColumn (tab) : tcol);
	
        /*  Now scan the data table for acrefs.  We got the acref column above
         *  so lookup the table cell directly for each row, either printing
	 *  out the acref for a simple extract, or by adding to the access
	 *  list to be processed below.
         */
	i = 0;
        for (tr=vot_getTR (tdata); tr; tr=vot_getNext(tr)) {
	    acref = vot_getTableCell (tdata, i, acol);
	    if (tcol >= 0) {
		char  *format = vot_getTableCell (tdata, i, tcol);

		if (format && fmt && strcasestr (format, fmt) == NULL) 
		    continue;
	    }

	    vot_saveAcref (acref, i);

	    nfiles++;
	    tnum++;
	    i++;
        }
    }

    vot_closeVOTABLE (vot);			
    if (afd)					/* close the acref file	*/
	fclose (afd);

    return (nfiles);
}


/**
 *  VOT_SAVEACREF -- Save the URL to the access list.
 */
static void
vot_saveAcref (char *acref, int num)
{
    if (afd)
	fprintf (afd, "%s\n", acref);
    else if (extract)
	printf ("%s\n", acref);
    else {
	/*  Save to the access list.
	 */
	aclist[num].tnum = ((nthreads == 1) ? 0 : (num % nthreads));
	strcpy (aclist[num].url, acref);
	sprintf (aclist[num].fname, "%s%03d", base, num);
    }
}


/**
 *  VOT_ISVOTABLE -- Determine in the input file is a VOTable or URL @file.
 *  We return zero if the file cannot be parsed as a valid VOTable (i.e.
 *  we assume it is an @file of URLs), or else we return the root handle to
 *  the parsed file.
 */

#define  SZ_READ	2880

static int
vot_isVOTable (char *infile)
{
    FILE  *fd = (FILE *) NULL;
    char  buf[SZ_READ];
    register int nread;;


    /* read the first 1024 bytes and search for a 'votable' string... */
    if (access (infile, F_OK) < 0) {
	if (verbose)
	    fprintf (stderr, "Error: Cannot open input file '%s'\n", infile);
	return (-1);

    } else if ((fd = fopen (infile, "r"))) {
	memset (buf, 0, SZ_READ);
	nread = fread (buf, sizeof (char), SZ_READ, fd);
	fclose (fd);

	return (strcasestr (buf, "votable") ? 1 : 0);
    }
    return ( 0 );
}


/**
 *  VOT_ACREFCOLUMN -- Determine the access column for the given table.
 */
static int
vot_acrefColumn (handle_t tab)
{
    register  int i = 0, acol = -1;
    handle_t  field;
    char     *ucd;


    /*  Loop through the FIELDs to find the acref.
     */  
    for (field=vot_getFIELD(tab); field; field=vot_getNext(field),i++) {
	ucd  = vot_getAttr (field, "ucd");
	if (ucd && strcasecmp (acref_ucd, ucd) == 0) {
	    acol = i;
	    break;
	}
    }

    if (acol < 0) { 		/*  make sure we found a column	*/
	if (verbose)
	    fprintf (stderr, "Error: no acref column found (%s)\n", acref);
	return (-1);
    }

    return (acol);
}


/**
 *  VOT_TYPECOLUMN -- Determine the type column for the given table.
 */
static int
vot_typeColumn (handle_t tab)
{
    register  int i = 0;
    handle_t  field;
    char     *ucd;


    /*  Loop through the FIELDs to find the type.  Use a generous match.
     */  
    if (tcol < 0) {
        for (field=vot_getFIELD(tab); field; field=vot_getNext(field),i++) {
	    ucd  = vot_getAttr (field, "ucd");
	    if (ucd && strcasestr (ucd, fmt_ucd)) {
	        tcol = i;
	        break;
	    }
        }
    }

    return (tcol);
}


/** 
 *  VOT_GETACLIST -- Download all the files for the specified thread.
 */
static void *
vot_getAclist (void *arg)
{
    register int i, done = 0;
    int threadNum = 0;

    if (arg) 
	threadNum = *(int *)arg;

    for (i=0; i < nfiles; i++) {
	if (aclist[i].tnum == threadNum)
	    done += vot_getURL (aclist[i].url, aclist[i].fname);
    }
    
    pthread_exit (NULL);
}


/** 
 *  VOT_GETURL -- Utility routine to do a simple URL download to the file.
 */
static int 
vot_getURL (char *url, char *ofname)
{
    CURL *curl_handle;
    FILE *fd;


    curl_global_init (CURL_GLOBAL_ALL);     	/* init curl session	*/
    curl_handle = curl_easy_init ();

    if ((fd = fopen (ofname, "wb")) == NULL) { 	/* open the output file */
	if (verbose)
	    fprintf (stderr, "Error: cannot open output file '%s'\n", ofname);
        curl_easy_cleanup (curl_handle);
        return 0;
    }

    /*  Set cURL options
     */
    curl_easy_setopt (curl_handle, CURLOPT_URL, url);
    curl_easy_setopt (curl_handle, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt (curl_handle, CURLOPT_WRITEDATA, fd);

    curl_easy_perform (curl_handle); 	    	/* get it! 		*/

    fclose (fd); 			    	/* close the file 	*/
    curl_easy_cleanup (curl_handle); 	    	/* cleanup curl stuff 	*/


    pthread_mutex_lock (&counter_mut);
    ngot++;
    if (verbose) {
	fprintf (stderr, "Downloaded %d of %d files ....\r", ngot, nfiles);
	fflush (stderr);
    }
    pthread_mutex_unlock (&counter_mut);

    return 1;
}


/******************************************************************************
**  Debug Utilities
******************************************************************************/

/** 
 *  VOT_GETACLIST -- Download all the files for the specified thread.
 */
static void
vot_printAclist ()
{
    register int i;

    fprintf (stderr, "\nAccess List:  nfiles = %d\n", nfiles);
    for (i=0; i < nfiles; i++) {
	fprintf (stderr, "%2d: url='%20.20s...'  fname='%s'  tnum=%d\n",
	    i, aclist[i].url, aclist[i].fname, aclist[i].tnum);
    }
}
