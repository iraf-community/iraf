/************************************************************************
**  VOACLIST.C -- Procedures for handling the AccessList of images/data
**  to be  downloaded.
**
**  M. Fitzpatrick, NOAO, July 2007
*/

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include "VOClient.h"
#include "voAppsP.h"


/* Local processing definitions.
*/

extern Acref  *acList, *acTail;		/* Access reference linked list	*/
extern int     nacrefs;			/* number of access refs	*/
extern int     file_get;		/* number of files to get	*/
extern time_t  as_time, ae_time;	/* processing times		*/

extern  int  debug, verbose, quiet;
extern  int  max_download;
extern  char *output;
extern  int  errno;			/* system error code		*/

extern  Range fileRange;		/* download file range list	*/

extern  char *vot_validateFile (char *fname);
extern  char *vot_urlFname (char *url);


/* Local task prototypes.
*/
void    vot_addToAclist (char *url, char *fname);
void    vot_freeAclist (void);
void    vot_procAclist (void);

static pid_t vot_dlProc (Acref *ac, int filenum);
static int   vot_acGetURL (char *url, char *fname, long *size);



/************************************************************************
**  VOT_ADDTOACLIST --  Add the given url/id to the file access list.
*/
void
vot_addToAclist (char *url, char *outfile)
{
    Acref *new = calloc (1, sizeof (Acref));

    bzero (new->url, SZ_LINE);
    bzero (new->fname, SZ_FNAME);

    strncpy (new->url, url, strlen (url));
    if (outfile)
	strncpy (new->fname, outfile, strlen (outfile));
    new->index = nacrefs;

    new->status = AC_PENDING;
    if (!acList) {
	acList = acTail = new;

    } else {
	acTail->next = new;
	acTail = new;
    }
    nacrefs++;
}


/************************************************************************
**  VOT_FREEACLIST --  Free the Aclist structure and reset the counter.
*/
void
vot_freeAclist ()
{
    register int i;
    Acref *cur, *next;


    cur = acList;
    for (i=0; i < nacrefs; i++) {
	next = cur->next;
	if (cur)
	    free ((void *)cur);
	cur = next;
    }
	
    nacrefs = 0;
    acList = acTail = (Acref *) NULL;
}


/************************************************************************
**  VOT_PROCACLIST -- Begin processing the access list.  We do this by
**  spawning up to MAX_DOWNLOADS procs to download the references in
**  parallel.
*/
void
vot_procAclist (void)
{
    int   nthreads, nfile, nrunning, nremaining;
    pid_t pid, rc, status;
    Acref *ac = acList;

    
    /* Initialize.
    */
    nthreads = (nacrefs > max_download) ? max_download : nacrefs;
    nrunning = 0;
    nremaining = nacrefs;

    as_time = time ((time_t) NULL);	/* get start time	*/


    if (verbose && !quiet)
	printf ("\n# Beginning download of %d files....\n", nacrefs);

    for (nfile=1; nremaining > 0;) {

	/* Spawn a process thread for each access reference.
	*/
	if (nrunning < nthreads && nfile <= nacrefs) {

	    if (debug)
		fprintf (stderr, "Starting download for '%s'\n", ac->url);

	    if ((pid = vot_dlProc (ac, nfile)) < 0) {
	        fprintf (stderr, "ERROR: process fork() fails\n");
	        exit (-1);
 	    }
	    nrunning++;
	    nfile++;

	    if (ac)
		ac = ac->next;

	} else {
	    /* All processes running, wait for one to finish.
	    */
	    if (debug)
		fprintf (stderr, "Waiting on download\n");

	    if ((rc = waitpid ((pid_t)-1, &status, (int) 0)) < 0) {
		fprintf (stderr, "ERROR: aclist waitpid() fails, code: %d\n",
		    (int)rc);
		exit (-1);
	    }
	    status = WEXITSTATUS(status);

	    nrunning--;
	    nremaining--;
	}
    }

    if (verbose && !quiet)
	printf ("#\n# Downloads complete.\n");


    ae_time = time ((time_t) NULL);	/* get end time		*/

    return;
}


/************************************************************************
**  VOT_DLPROC -- Procedure used to spawn a download child process.  We
**  return the child pid, and fork off the actual download.  The caller
**  will wait for completion. 
*/
static pid_t
vot_dlProc (Acref *ac, int filenum)
{
    pid_t  cpid;
    int    nerrs = 0;
    long   nbytes = 0;
    char   *urlFname, *out, svc[SZ_FNAME], idx[10], fname[SZ_FNAME];


    if (debug)
	fprintf (stderr, "vot_dlProc(%d): %s\n", filenum, ac->url);

    if ((cpid = fork()) < 0) {
	fprintf (stderr,
	    "vot_dlProc: Unable to create child process, exiting\n");
	exit (-1);

    } else if (cpid > 0) {
	return (cpid);				/* parent		*/

    } else {					/* child		*/
        /*  Initialize the VOClient code.  Error messages are printed by the
        **  interface so we just quit if there is a problem.
        */
        if (voc_initVOClient ((char *) NULL) == ERR) 
            exit (1);

	bzero (idx, 10);
	bzero (svc, SZ_FNAME);
	if (ac->fname[0]) {
	    char *ip, *op;

	    strcpy (fname, ac->fname);
	    for (ip=fname,op=svc; *ip && *ip != '.'; )
		*op++ = *ip++;
	    ip++;
	    if (isdigit(*ip))
	        strcpy (idx, ip);
	    else
	        strcpy (idx, "001");
	} else {
            bzero (fname, SZ_FNAME);
            sprintf (fname, "%s%03d", (output ? output : "file"), filenum);
	    strcpy (svc, "file");
            sprintf (idx, "%03d", filenum);
	}

	urlFname = vot_urlFname(ac->url);
        if (verbose) {
	    if (strcmp (idx, "001") == 0 && !quiet)
	        fprintf (stderr, "#\n# Downloading URLs from service:  %s...\n",
		    svc);
	    if (!quiet)
	        fprintf (stderr, "#   File %s...", idx);
	}

        nerrs = vot_acGetURL (ac->url, fname, &nbytes);

        out = vot_validateFile (fname);

        if (verbose && !quiet && !nerrs)
	    fprintf (stderr, " (%ld bytes)  file:  %s\n", nbytes, out);

	strcpy (ac->fname, fname);
	ac->nbytes = nbytes;
	ac->status = (nerrs ? AC_ERROR : AC_COMPLETE);

        voc_closeVOClient (0);			/* close VOClient connection */
    }

    exit (0);
}


/************************************************************************
**  Download a raw URL to the named file.
*/

static int
vot_acGetURL (char *url, char *fname, long *size)
{
    int  fd, nbytes, err = 0;
    char *res;
    extern char *output;

                    
    if ((res = voc_getRawURL (url, &nbytes)) == NULL) {
        fprintf (stderr, "Cannot access URL\n");
        err++;
    } else {
        if (output && output[0] == '-')                 /* use stdout   */
            fd = fileno(stdout);
        else {
            if (access (fname, R_OK|W_OK) == 0)         /* overwrite    */
                unlink (fname);

            /* Open the file.
            */
            if ((fd = open (fname, O_RDWR|O_CREAT, 0644)) < 0) {
                fprintf (stderr, "Cannot open file '%s'\n",
                    fname);
                err++;
            }
        }

        /* Write the (binary) result string to the file.
        */
        if (write (fd, res, nbytes) != nbytes) {
            fprintf (stderr, "Short file write\n");
            err++;
        }

        if (fd != fileno(stdout))               /* close if not stdout  */
            close (fd);
    }

    if (res) 
        free ((char *) res);

    *size = nbytes;                             /* return the size      */
    return (err);
}
