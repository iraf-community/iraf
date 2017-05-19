/**
 *  VOTGET -- Download select/all access references in a VOTable.
 *
 *  Usage:
 *
 *	votget [<opts>] <votable.xml>
 *	votget [<opts>] --samp 		# to listen for SMAP messages
 *
 *  Where
 *	    -b,--base <base>	    Base output filename
 *	    -e,--extn [<extn>] 	    Extension to add to filename (or auto)
 *	    -f,--fmt <fmt>  	    Download only specified <type>
 *	    -s,--sum 		    Use checksum as file number
 *	    -t,--tmp 		    Input file is temporary, delete when done
 *	    -u,--ucd <ucd>	    Use ucd to identify acref column
 *
 *	    -o,--output <fname>     Output filename (single download)
 *	    -v,--verbose 	    Verbose output
 *	    -x,--extract 	    Extract access references
 *
 *	    -A,--acref <colnum>	    Col number for acref column (0-indexed)
 *	    -B,--bkg  		    Background, i.e. run in forked child process
 *	    -C,--cache  	    Cache the downloaded file
 *	    -D,--download  	    Set download directory
 *	    -F,--fmtcol <colnum>    Col number for format column (0-indexed)
 *	    -N,--num <N> 	    Number of simultaneous downloads
 *	    -S,--samp		    start as SAMP listener
 *	    -m,--mtype <mtype>	    mtype to wait for
 *
 *	    -h,--help		    Print help summary
 *	    -d,--debug		    Debug output
 *	       --test		    Run unit tests
 *
 *	    <votable>		    VOTable to process
 *
 *  @file       votget.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/12
 *
 *  @brief      Download select/all access references in a VOTable.
 */


#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <pthread.h>

#include <curl/curl.h>
#include <curl/easy.h>

#include "samp.h"
#include "votParse.h"
#include "voApps.h"


#define	MIN_THREADS	4		/* min no. simultaneous thread	*/
#define	MAX_THREADS	64		/* max no. simultaneous threads */
#define	MAX_DOWNLOADS	4096		/* max no. files to download	*/
#define	MAX_TRYS	3		/* max download attempts	*/

#define NAXIS_UCD   	"VOX:Image_Naxis"
#define NAXES_UCD   	"VOX:Image_Naxes"
#define SCALE_UCD   	"VOX:Image_Scale"
#define ACREF_UCD   	"VOX:Image_AccessReference"
#define FORMAT_UCD   	"VOX:Image_Format"


static int   vot	= 0;		/* VOTable handle	     	*/
static int   verbose	= 0;		/* verbose parameter	     	*/
static int   debug	= 0;		/* debug flag	     		*/
static int   extract	= 0;		/* extract references only	*/
static int   detach	= 0;		/* run as detached process	*/
static int   nfiles     = 0;		/* number of download files	*/
static int   ngot 	= 0;		/* number of files downloaded	*/
static int   seq 	= 1;		/* use sequential file numbers  */
static int   isCache    = 0;		/* is this a cache file?	*/
static int   isTemp     = 0;		/* is this a temp file?		*/
static int   force      = 0;		/* overwrite existing file      */
static int   acol	= -1;		/* access reference column 	*/
static int   tcol	= -1;		/* image type column 		*/
static int   filenum	= 0;		/* running download file number	*/

static int   nthreads   = MIN_THREADS;	/* number of download threads	*/
static int   maxTrys	= MAX_TRYS;	/* download attempts		*/

static char *base	= NULL;		/* output base filename    	*/
static char *extn	= NULL;		/* output filename extension   	*/
static char *dir	= NULL;		/* download directory		*/
static char *afname 	= NULL;		/* output acref filename	*/

static char *acref 	= NULL;		/* acref url 		     	*/
static char *acref_ucd  = NULL;		/* acref UCD 		     	*/
static char *fmt 	= NULL;		/* image format 	     	*/
static char *fmt_ucd    = NULL;		/* image format UCD 	     	*/

static FILE  *afd = (FILE *) NULL;	/* acref file descriptor	*/

static pthread_mutex_t counter_mut = PTHREAD_MUTEX_INITIALIZER;

typedef void  (*SIGFUNC)();           	/* signal handler type		*/

typedef struct {
    char   url[SZ_URL];			/* access URL			*/
    char   fname[SZ_URL];		/* local filename		*/
    int    tnum;			/* worker thread number		*/
} Acref, *AcrefP;

Acref   aclist[MAX_DOWNLOADS];		/* access list			*/


/*  Task specific option declarations.
 */
int  votget (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "votget",  votget,  0,  0,  0  };
static char  *opts      = "%:hb:e:f:dstu:o:vxA:BCD:F:N:Sm:";
static struct option long_opts[] = {
        { "base",         1, 0,   'b'},         /* task option          */
        { "extn",         1, 0,   'e'},         /* task option          */
        { "fmt",          1, 0,   'f'},         /* task option          */
        { "sum",          2, 0,   's'},         /* task option          */
        { "tmp",          2, 0,   't'},         /* task option          */
        { "ucd",          1, 0,   'u'},         /* task option          */
        { "output",       1, 0,   'o'},         /* task option          */
        { "verbose",      2, 0,   'v'},         /* task option          */
        { "extract",      2, 0,   'x'},         /* task option          */
        { "acref",        1, 0,   'A'},         /* task option          */
        { "bkg",          2, 0,   'B'},         /* task option          */
        { "cache",        2, 0,   'C'},         /* task option          */
        { "download",     1, 0,   'D'},         /* task option          */
        { "fmtcol",       1, 0,   'F'},         /* task option          */
        { "num",          1, 0,   'N'},         /* task option          */
        { "force",        2, 0,   'O'},         /* task option          */
        { "samp",         2, 0,   'S'},         /* task option          */
        { "mtype",        1, 0,   'm'},         /* task option          */

        { "help",         2, 0,   'h'},         /* required             */
        { "debug",        2, 0,   'd'},         /* required             */
        { "test",         1, 0,   '%'},         /* required             */
        { NULL,           0, 0,    0 }
};

static int   do_return	= 0;			/* NOT USED		*/
static int   do_samp	= 0;			/* samp listener?	*/
static char *mtype	= NULL; 		/* samp mtype		*/


static void Usage (void);
static void Tests (char *input);


/*  Public methods.
 */
extern int   vot_isValidFormat (char *fmt);
extern int   vot_atoi (char *val);
extern int   vot_sum32 (char *str);
extern char *strcasestr ();


/*  Private methods.
 */
static void  votableHandler (char *url, char *tblId, char *name);

static int   vot_procFile (char *iname);
static int   vot_isVOTable (char *infile);
static int   vot_acrefColumn (handle_t tab);
static int   vot_typeColumn (handle_t tab);
static int   vot_loadText (char *infile);
static int   vot_loadVOTable (char *infile);
static int   vot_getData (char *url, char *ofname);

static void  vot_saveAcref (char *acref, int num, int fnum);
static void *vot_getAclist (void *arg);
static void  vot_printAclist ();
static void  vot_reaper (int sig, int *arg1, int *arg2);




/**
 *  Program entry point.
 */
int
votget (int argc, char **argv, size_t *reslen, void **result)
{
    char **pargv, optval[SZ_FNAME];
    char  *iname = NULL, ch;
    int    samp = 0, pos = 0, stat = OK;


    /*  Initialize. 
     */
    *reslen = 0;
    *result = NULL;


    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0) {
        if (ch > 0) {

	    switch (ch) {
	    case '%':   Tests (optval);			return (self.nfail);
	    case 'h':   Usage ();			return (OK);

	    case 'b':   base = strdup (optval); 	break;
	    case 'e':   extn = strdup (optval);		break;
            case 'f':   if (!vot_isValidFormat ((fmt = strdup (optval)))) {
                            fprintf (stderr, "Error: invalid format '%s'\n",
                                fmt);
                            return (ERR);
                        }
                        break;
	    case 'd':   debug++;			break;
	    case 's':   seq=0;				break;
	    case 't':   isTemp++;			break;
	    case 'u':   acref_ucd = strdup (optval);	break;

	    case 'o':   afname = strdup (optval);	break;
	    case 'v':   verbose++;			break;
	    case 'x':   extract++;			break;

	    case 'A':   acol = vot_atoi (optval);	break;
	    case 'B':   detach++;			break;
	    case 'C':   isCache++;			break;
	    case 'D':   dir = strdup (optval);		break;
	    case 'F':   tcol = vot_atoi (optval);	break;
	    case 'N':   nthreads = vot_atoi (optval); 	break;
	    case 'O':   force++;			break;
	    case 'S':   do_samp++;			break;
	    case 'm':   mtype = strdup (optval);;	break;

	    default:
		fprintf (stderr, "Invalid option '%c'\n", ch);
		return (1);
	    }

        } else if (ch == PARG_ERR) {
            return (ERR);

	} else {
	    iname = strdup (optval);
	}
    }


    /*  Sanity checks
     */
    if (iname == NULL) iname = strdup ("stdin");
    if (strcmp (iname, "-") == 0) { free (iname), iname = strdup ("stdin"); }


    /*  Setup defaults and initialize.
     */
    do_return = 0;
    memset (&aclist[0], 0, (sizeof (Acref) * MAX_DOWNLOADS));

    if (afname && (afd = fopen (afname, "a+")) == (FILE *) NULL) {
	if (verbose)
	    fprintf (stderr, "Error: cannot open aclist file '%s'\n", afname);
	return (ERR);
    }

    if (!base)      base = strdup ("file");
    if (!mtype)     mtype = strdup ("table.load.votable");
    if (!fmt_ucd)   fmt_ucd = strdup (FORMAT_UCD);
    if (!acref_ucd) acref_ucd = strdup (ACREF_UCD);


    if (do_samp) {
        /*  Initialize and startup the SAMP interface.  Wait for a message.
         */
	if (verbose)
	    fprintf (stderr, "Initializing samp ....\n");
        samp = sampInit ("votget", "VOClient Task");
        samp_Subscribe (samp, mtype, votableHandler);
	if (sampStartup (samp) < 0)
	    return (ERR);

	if (verbose)
	    fprintf (stderr, "Type <cr> to quit ....");
	while (fgetc(stdin) != EOF)
	    break;

	sampShutdown (samp);

    } else {
	/*  Process the file.
	 */
        stat = vot_procFile (iname);
    }


    /*  Close the table and clean up.
     */
    if (base)      free (base);
    if (extn)      free (extn);
    if (dir)       free (dir);
    if (afname)    free (afname);
    if (acref)     free (acref);
    if (acref_ucd) free (acref_ucd);
    if (fmt)       free (fmt);
    if (fmt_ucd)   free (fmt_ucd);
    if (mtype)     free (mtype);

    vo_paramFree (argc, pargv);
    if (detach)
        exit (OK);
    else
        return (stat);
}


/**
 *  VOTABLEHANDLER -- Callback for the load.table.votable message.
 */
static void 
votableHandler (char *url, char *tblId, char *name) 
{
    char  fname[SZ_FNAME];


    memset (fname, 0, SZ_FNAME);

    if (verbose) 
	printf ("\n");


    if (strncmp (url, "http://", 7) == 0) {
        strcpy (fname, "/tmp/votgetXXXXXX");    /* temp download name    */
        mktemp (fname);
        if (vot_getData (url, fname) < 0) 
	    fprintf (stderr, "Error accessing url '%s'\n", url);

        vot_procFile (fname);
        unlink (fname);

    } else if (strncmp (url, "file://", 7) == 0) {
	strcpy (fname, &url[7]);
        vot_procFile (fname);

    } else {
	fprintf (stderr, "Error: unsupported URL type '%s'\n", url);
	return;
    }

    /*  Clean up for the next file to process.
     */
    memset (&aclist[0], 0, (sizeof (Acref) * MAX_DOWNLOADS));
    nfiles = 0;
}


/**
 *  VOT_PROCFILE -- Process a VOTable file.
 */
static int
vot_procFile (char *iname)
{
    int   stat = OK;


    /*  Determine the type of input file.
     */
    if (strncmp (iname, "http://", 7) == 0) {
        if (vot_loadVOTable (iname) < 0) {
	   fprintf (stderr, "Error opening votable '%s'\n", iname);
	   return ( (stat = ERR) );
	}

    } else {
        switch ((vot = vot_isVOTable (iname))) {
        case -1:  fprintf (stderr, "Error opening file '%s'\n", iname);
		  return ( (stat = ERR) );
        case  0:  if (vot_loadText (iname) < 0) {
		      fprintf (stderr, "Error opening text file '%s'\n", iname);
		      return ( (stat = ERR) );
		  }
		  break;
        case  1:  if (vot_loadVOTable (iname) < 0) {
		      fprintf (stderr, "Error opening votable '%s'\n", iname);
		      return ( (stat = ERR) );
		  }
		  break;
        }
    }


    /*  If all we're doing is extracting the URLs we can quit now.
     */
    if (extract)
        return (OK);

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

        signal (SIGCHLD, (SIGFUNC)vot_reaper);
        switch ((pid = fork ())) {
        case -1:  return (ERR);			/* We are an error      */
        case 0:   break;			/* We are the child     */
        default:  return (OK);			/* We are the parent    */
        }
    }


    /*  Initialize the download directory.
     */
    if (dir) {
	if (access (dir, F_OK) < 0)
	    mkdir (dir, 0755);
	if (access (dir, W_OK) < 0) {
	   if (verbose)
	       fprintf (stderr, "Error: Cannot write to directory '%s'\n", dir);
	   return (ERR);
	}
	chdir (dir);
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
                    return (-1);
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
                    return (-1);
		}
            } 
        }

        if (verbose) {
	    fprintf (stderr, 
		"Downloaded %d files -- Download complete (Total:  %d)\n", 
		nfiles, (filenum+1));
	    fflush (stderr);
        }
    }


    /*  Remove input file if it is temporary.
     */
    if (isTemp)
	unlink (iname);

    return (stat);
}




/**
 *  VOT_USAGE -- Print the task usage and exit.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "votget [<opts>] [ <votable.xml> | <urls.txt> ]\n\t"
        "votget [<opts>] --samp          # to listen for SMAP messages\n"
        "\n" 
        "Where\n"
        "   -b,--base <base>        Base output filename\n"
        "   -e,--extn [<extn>]      Extension to add to filename (or auto)\n"
        "   -f,--fmt <fmt>          Download only specified <type>\n"
        "   -s,--sum                Use 32-bit checksum as file numbers\n"
        "   -t,--tmp                Input file is temporary, delete when done\n"
        "   -u,--ucd <ucd>          Use ucd to identify acref column\n"
        "\n" 
        "   -o,--output <fname>     Output filename (single download)\n"
        "   -v,--verbose            Verbose output\n"
        "   -x,--extract            Extract access references\n"
        "\n" 
        "   -A,--acref <colnum>     Col number for acref column (0-indexed)\n"
        "   -B,--bkg                Background, i.e. run in forked child\n"
        "   -C,--cache              Cache the downloaded file\n"
        "   -D,--download           Set download directory\n"
        "   -F,--fmtcol <colnum>    Col number for format column (0-indexed)\n"
        "   -N,--num <N>            Number of simultaneous downloads\n"
        "   -S,--samp               start as SAMP listener\n"
        "\n" 
        "   -h,--help               Print help summary\n"
        "   -d,--debug              Debug output\n"
        "      --test               Run unit tests\n"
        "\n" 
        "   <votable>               VOTable to process\n"
        "\n"
        "  Examples:\n\n"

        "  1) Download all files in the VOTable 'results.xml', 3 files at\n"
        "     a time:\n\n"
        "       %% votget -N 3 results.xml\n"
        "\n"
        "  2) Start as a SAMP listener waiting for VOTable events to be \n"
        "     broadcast, saved files will begin with the string 'foo' and \n"
        "     contain a 'fits' filename extension:\n\n"
        "       %% votget -b foo -e fits -S\n"
        "\n"
        "     To exit the task, hit the <CR>.\n"
        "\n"
        "  3) Download all the urls in the file 'urls.txt':\n\n"
        "       %% votget -b foo urls.txt\n"
        "\n"
        "  4) Extract all the access references in a VOTable:\n\n"
        "       %% votget -x results.xml\n"
        "\n"
    );
}


/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
    Task *task = &self;
    char *urls = "http://iraf.noao.edu/votest/sia.xml\n";


    vo_taskTest (task, "--help", NULL);

    if (access (input, F_OK) != 0) {
	fprintf (stderr, "Warning: cannot open file '%s'\n", input);
	return;
    }

    vo_taskTestFile (urls, "urls.txt");

    vo_taskTest (task, "-N", "3", input, NULL);				// Ex 1
    vo_taskTest (task, "-b", "foo", input, NULL);			// Ex 3
    vo_taskTest (task, "-x", input, NULL);				// Ex 4
    vo_taskTest (task, "-x", "-o", "/tmp/acref", input, NULL); 

    if (access ("/tmp/acref", F_OK) == 0)  unlink ("/tmp/acref");
    if (access ("urls.txt", F_OK) == 0)    unlink ("urls.txt");

    vo_taskTestReport (self);
}



/**********************************************************************
**  Private Procedures
**********************************************************************/

/**
 *  VOT_REAPER -- Catch a SIGCHLD signal and reap all children.
 */
static void
vot_reaper (
  int     sig,                  /* signal which was trapped     */
  int     *arg1,                /* not used */
  int     *arg2                 /* not used */
)
{
    int status=0, pid=0;

    while ((pid = waitpid ((pid_t) 0, &status, WNOHANG)) > 0)
        ;
}


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


    nfiles = 0;
    memset (&aclist[0], 0, (sizeof (Acref) * MAX_DOWNLOADS));

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

	vot_saveAcref (acref, i, filenum++);

	acref = ip + 1;
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


    nfiles = 0;
    memset (&aclist[0], 0, (sizeof (Acref) * MAX_DOWNLOADS));

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
        if ((data  = vot_getDATA (tab)))
            tdata = vot_getTABLEDATA (data);
	else
	    continue;		/* empty data table */

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

	    vot_saveAcref (acref, i, filenum++);

	    nfiles++;
	    tnum++;
	    i++;
        }
    }


    /*  Clean up.
     */
    if (afd)					/* close the acref file	*/
	fclose (afd);

    vot_closeVOTABLE (vot);			

    return (nfiles);
}


/**
 *  VOT_SAVEACREF -- Save the URL to the access list.
 */
static void
vot_saveAcref (char *acref, int num, int fnum)
{
    if (afd)
	fprintf (afd, "%s\n", acref);
    else if (extract)
	fprintf (stderr, "%s\n", acref);
    else {
	/*  Save to the access list.
	 */
	aclist[num].tnum = ((nthreads == 1) ? 0 : (num % nthreads));
	strcpy (aclist[num].url, acref);
	if (seq)
	    sprintf (aclist[num].fname, "%s%04d", base, (int) fnum);
	else
	    sprintf (aclist[num].fname, "%s%d", base, vot_sum32 (acref));
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
    char  buf[SZ_READ], fname[SZ_READ];
    register int nread;;


    memset (fname, 0, SZ_READ);
    if (strncmp (infile, "file://", 7) == 0) 
	strcpy (fname, &infile[7]);
    else
	strcpy (fname, infile);

    /* read the first 1024 bytes and search for a 'votable' string... */
    if (access (fname, F_OK) < 0) {
	if (verbose)
	    fprintf (stderr, "Error: Cannot open input file '%s'\n", fname);
	return (-1);

    } else if ((fd = fopen (fname, "r"))) {
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
    register int i, j, done = 0, ret = 0;
    int threadNum = 0;


    if (arg) 
	threadNum = *(int *)arg;

    for (i=0; i < nfiles; i++) {
	if (aclist[i].tnum == threadNum) {
    	    for (j=0; j < maxTrys; j++) {
	        if ((ret = vot_getData (aclist[i].url, aclist[i].fname)))
		    break;
	    }
	    done += ret;
	}
    }
    
/*
    pthread_exit (NULL);
*/
    return ((void *) NULL);
}


/** 
 *  VOT_GETDATA -- Utility routine to do a simple URL download to the file.
 */
static int 
vot_getData (char *url, char *ofname)
{
    int  stat = 0;
    char lockfile[SZ_FNAME], dot[SZ_FNAME], errBuf[CURL_ERROR_SIZE];
    char fname[SZ_FNAME], ffname[SZ_FNAME];
    FILE *fd;
    CURL *curl_handle;


    /*   FIXME   */
    if (extn)
        sprintf (ffname, "%s.%s", ofname, extn);
    else {
        sprintf (ffname, "%s.fits", ofname);
    }

    if (access (ofname, F_OK) == 0 || access (ffname, F_OK) == 0) {
	/* file already exists	*/
	if (force)
	    unlink (ofname);
	else 
	    return (1);
    }


    /*  Initialize the lock file.
     */
    memset (lockfile, 0, SZ_FNAME);
    memset (dot, 0, SZ_FNAME);
	
    sprintf (lockfile, ".%s.LOCK", ofname);
    sprintf (dot, ".%s", ofname);

    if (access (lockfile, F_OK) == 0 && access (dot, F_OK) < 0) {
	/*  Download currently in progress, perhaps on another thread?
	  */
	return (0);
    } else if (access (lockfile, F_OK) == 0 && access (dot, F_OK) == 0) {
	/*  Download complete, stray lockfile.
	 */
	unlink (lockfile);
    } else if (access (lockfile, F_OK) < 0) {
	/*  No lock file, create one.
	 */
        creat (lockfile, O_CREAT);
    }


    /*  Append filename extension if specified.
     */
    if (extn)
	sprintf (fname, "%s.%s", ofname, extn);
    else
	strcpy (fname, ofname);


    /*  For the CURL operation to download the file.
     */
    curl_global_init (CURL_GLOBAL_ALL);     	/* init curl session	*/
    curl_handle = curl_easy_init ();

    /*  Open the output file.
     */
    if ((fd = fopen (fname, "wb")) == NULL) { 	
	if (verbose)
	    fprintf (stderr, "Error: cannot open output file '%s'\n", fname);
        curl_easy_cleanup (curl_handle);
        return 0;
    }

    /*  Set cURL options
     */
    curl_easy_setopt (curl_handle, CURLOPT_URL, url);
    curl_easy_setopt (curl_handle, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt (curl_handle, CURLOPT_WRITEDATA, fd);
    curl_easy_setopt (curl_handle, CURLOPT_ERRORBUFFER, errBuf);
    curl_easy_setopt (curl_handle, CURLOPT_FOLLOWLOCATION, 1);

    /*  Do the download.
     */
    if ((stat = curl_easy_perform (curl_handle)) != 0) {
	/*  Error in download, clean up.
	 */
	if (verbose)
	    fprintf (stderr, "Error: can't download '%s' : %s\n", url, errBuf);
	unlink (fname); unlink (lockfile);
        fclose (fd); 			    	/* close the file 	*/
        curl_easy_cleanup (curl_handle);    	/* cleanup curl stuff 	*/
	return (0);
    }

    fflush (fd);
    fclose (fd); 			    	/* close the file 	*/
    curl_easy_cleanup (curl_handle); 	    	/* cleanup curl stuff 	*/

    /*  Save the URL to a "dotfile" is we're downloading to a cache.
     */
    if (isCache) {
        if ((fd = fopen (dot, "w")) == NULL) { /* open cache file   */
	    if (verbose)
	        fprintf (stderr, "Error: cannot open cache file '%s'\n", dot);
            return 0;
	}
	fprintf (fd, "%s\n", url);
	fclose (fd);
    }

    /*  If we didn't specify an extension, try to determin the file type
     *  automatically.
     */
    if (!extn) {
	int  i = 0, dfd, maxtrys = 30;

	if ((dfd = open (fname, O_RDONLY)) > 0) {
	    char  buf[1024], new[SZ_FNAME];
	    unsigned short *s = (unsigned short *) NULL;

	    (void) read (dfd, buf, 1024);

	    s = (unsigned short *) buf;
	    memset (new, 0, SZ_FNAME);
	    if ((s[0] == 35615 && s[1] == 2056) ||	/* GZIP file	*/
	        (s[0] ==  8075 && s[1] == 2048)) {
		    char gz[SZ_FNAME], cmd[SZ_FNAME];

		    memset (gz, 0, SZ_FNAME);
		    sprintf (gz, "%s.gz", fname);
		    memset (cmd, 0, SZ_FNAME);
		    sprintf (cmd, "gunzip %s", gz);

		    rename (fname, gz);			/* FIXME !!!	*/
		    system (cmd);

	    	    close (dfd);
		    if ((dfd = open (fname, O_RDONLY)) > 0) {
	    	        (void) lseek (dfd, 0, SEEK_SET);
	    	        (void) read (dfd, buf, 1024);
		    }
	    }

	    memset (new, 0, SZ_FNAME);
	    if (strncmp ("SIMPLE", buf, 6) == 0) {	/* FITS		*/
		sprintf (new, "%s.fits", fname);
		rename (fname, new);
		for (i=0; i < maxtrys; i++) {
		    if (access (new, F_OK) != 0)
			sleep (1);
		}
	    }

	    close (dfd);
	}
    }

    pthread_mutex_lock (&counter_mut);
    ngot++;
    if (verbose) {
	fprintf (stderr, "Downloaded %d of %d files ....\r", ngot, nfiles);
	fflush (stderr);
    }
    pthread_mutex_unlock (&counter_mut);

    /*  Remove the lock file to indicate we are done.
     */
    unlink (lockfile);

    return (1);
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
