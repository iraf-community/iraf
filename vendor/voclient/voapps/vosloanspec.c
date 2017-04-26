/*
 *  VOTSLOANSPEC -- Query and access spectra from SDSS/BOSS
 *
 *    Usage:
 *	vosloanspec [<opts>] <obj> | {<ra> <dec>} | {<ra> <dec> <radius>}
 *
 *    where
 *       -%%,--test              Run unit tests
 *       -h,--help               This message
 *       -d,--debug              Debug Flag
 *       -v,--verbose            Verbose Flag
 *       -r,--return=<obj>       Return object
 *
 *	 -P,--pos=<ra>,<dec>	 Set query position (dec degrees)
 *	 -R,--release=<rel>	 Data release (dr8/dr9/current) (def=current)
 *	 -s,--size=<radius>	 Set query radius (dec degrees)
 *	 -t,--type=<type>	 Object type (all|galaxy|qso|star)
 *	 -z,--redshift=<zrange>  Select by redshift range string(s)
 *
 *	 -c,--count		 Return only count of results
 *	 -m,--meta		 Print result position metadata
 *	 -D,--delete		 Delete spectra after printing metadata
 *	 -l,--limit=<N>		 Limit to top <N> results
 *	 -u,--urls		 Get urls to spectra
 *
 *	 -S,--samp		 Broadcase urls to SAMP (as spectrum)
 *	 -T,--table		 Broadcase urls as VOTable message
 *	 -N,--num=<N>		 Number of download threads
 *
 *       -f,--file=<file>        Input file
 *       -b,--base=<file>        Base filename
 *       -O,--output=<file>      Output file
 *       -o,--object=<obj>       Object name
 *
 *  Interface based on API at:  	http://api.sdss3.org/index.html
 *
 *
 *  @file       vosloanspec.c
 *  @author     Mike Fitzpatrick
 *  @date       9/93/12
 *
 *  @brief       Query and access spectra from SDSS/BOSS
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <pthread.h>
#include <errno.h>
#include <sys/stat.h>

#include "samp.h"
#include "VOClient.h"
#include "voApps.h"


#define	SZ_RESBUF	8192
#define	SZ_URL		1024

#define	MAX_THREADS	64
#define DEF_THREADS	12
#define MAX_DOWNLOADS	65537

#define	NEXTARG(a)	strdup(optval);argnum++;


static int  verbose   	= FALSE;	/* verbose flag			*/
static int  debug   	= FALSE;	/* debug flag			*/
static int  count	= FALSE;	/* count results		*/
static int  urls	= FALSE;	/* get URLs to data?		*/
static int  asSpec	= TRUE;		/* SAMP as Spectrum (or Table)?	*/
static int  nthreads	= DEF_THREADS;	/* Number of downlaod threads   */
static int  ndownloads  = 0;		/* Number of file downloads	*/
static int  specnum     = 0;		/* spectrum number		*/

static int  have_infile = FALSE;	/* have input file?		*/
static int  do_return   = FALSE;	/* return result?		*/
static int  do_samp     = FALSE;	/* broadcast SAMP result?	*/
static int  do_meta     = FALSE;	/* get spectrum metadata?	*/
static int  do_delete   = FALSE;	/* delete files after metadata?	*/

static char  *pos	= NULL;		/* position string		*/
static char  *release	= NULL;		/* data release 		*/
static char  *limit	= NULL;		/* limit results		*/
static char  *size	= NULL;		/* search radius		*/
static char  *type	= NULL;		/* spectrum type 		*/
static char  *redshift	= NULL;		/* redshift range		*/
static char  *iname	= NULL;		/* input position name		*/
static char  *oname	= NULL;		/* output name			*/
static char  *object	= NULL;		/* object name			*/
static char  *basename	= NULL;		/* base filename		*/

static double ra	= -999.9;	/* RA query position		*/
static double dec	= -999.9;	/* Dec query position		*/
static double rad	= 0.25;		/* query size (deg)		*/

static char   ra_str[SZ_FNAME];
static char   dec_str[SZ_FNAME];
static char   rad_str[SZ_FNAME];

#ifdef USE_RESBUF
static char *resbuf;			/* result buffer		*/
#endif

char   url[SZ_URL];
char  *query_base	= "http://api.sdss3.org/spectrumQuery?";
char  *spec_base	= "http://api.sdss3.org/spectrum?";


typedef struct {
    char   url[SZ_URL];                 /* access URL                   */
    char   fname[SZ_URL];               /* local filename               */
    int    tnum;                        /* worker thread number         */
} Acref, *AcrefP;

Acref   aclist[MAX_DOWNLOADS];          /* access list                  */


/*  Task specific option declarations.
 */
int  vosloanspec (int argc, char **argv, size_t *len, void **result);

static Task  self	= {  "vosloanspec",  vosloanspec,  0,  0,  0  };
static char  *opts 	= "%hrvdDP:N:R:STs:t:z:cl:uf:o:bm";
static struct option long_opts[] = {
        { "test",         2, 0,   '%'},		/* required		*/
        { "help",         2, 0,   'h'},		/* required		*/
        { "verbose",      2, 0,   'v'},		/* verbose flag		*/
        { "debug",        2, 0,   'd'},		/* debug flag		*/
        { "return",       1, 0,   'r'},		/* task return option	*/

        { "pos",          1, 0,   'P'},		/* position param	*/
        { "release",      1, 0,   'R'},		/* data release		*/
        { "table",        2, 0,   'T'},		/* data release		*/
        { "size",         1, 0,   's'},		/* search radius	*/
        { "type",         1, 0,   't'},		/* spectra type 	*/
        { "redshift",     1, 0,   'z'},		/* redshift range	*/

        { "count",        2, 0,   'c'},		/* count results	*/
        { "meta",         2, 0,   'm'},		/* count results	*/
        { "limit",        1, 0,   'l'},		/* limit results	*/
        { "urls",         2, 0,   'u'},		/* get urls     	*/
        { "samp",         2, 0,   'S'},		/* broadcast SAMP     	*/
        { "num",          1, 0,   'N'},		/* num downloads     	*/
        { "delete",       2, 0,   'D'},		/* num downloads     	*/

        { "file",         1, 0,   'f'},		/* input file		*/
        { "output",       1, 0,   'O'},		/* output file		*/
        { "object",       1, 0,   'o'},		/* object name		*/
        { "base",         1, 0,   'b'},		/* base filename	*/
        { NULL,           0, 0,    0 }
};


static void  Usage (void);
static void  Tests (char *input);
static int   vos_getCountValue (char *fname);
static int   vos_sampInit (void);
static char *vos_getURLValues (char *fname);
static void  vot_procAclist (void);
static void  vot_procMeta (void);
static void *vot_getAclist (void *arg);

extern int    vos_getURL ();
extern int    vot_atoi (char *v);
extern double vot_atof (char *v);
extern char  *vot_mktemp ();

extern int  vosesame (int argc, char **argv, size_t *len, void **result);
extern char *vo_urlEncode (char *target);



/**
 *  Application entry point.
 */
int
vosloanspec (int argc, char **argv, size_t *reslen, void **result)
{
    int    apos, argnum = 0, status = OK;
    FILE  *fd = (FILE *) NULL;
    char **pargv, optval[SZ_FNAME], ch, *urlList = NULL;;
    char  *tmp = vot_mktemp ("vosloan");


    /*  Initialize. 
     */
    oname   = NULL;
    iname   = NULL;
    *reslen = 0;
    *result = NULL;

    ra = -999.9;
    dec = -999.9;
    memset (ra_str, 0, SZ_FNAME);
    memset (dec_str, 0, SZ_FNAME);
    memset (rad_str, 0, SZ_FNAME);
    memset (url, 0, SZ_URL);		/* initialize the query URL	*/
    strcpy (url, query_base);
    strcpy (rad_str, "900");		/* 0.25 deg default		*/

    asSpec	= TRUE;
    do_samp     = FALSE;
    do_meta     = FALSE;
    do_delete   = FALSE;

    pos		= NULL;			/* position string		*/
    release	= NULL;			/* data release 		*/
    limit	= NULL;			/* limit results		*/
    size	= NULL;			/* search radius		*/
    type	= NULL;			/* spectrum type 		*/
    redshift	= NULL;			/* redshift range		*/
    iname	= NULL;			/* input position name		*/
    oname	= NULL;			/* output name			*/
    object	= NULL;			/* object name			*/
    basename	= NULL;			/* base filename		*/


    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&apos)) != 0) {
	argnum++;
        if (ch > 0) {
	    switch (ch) {
	    case '%':   Tests (NULL);			return (self.nfail);
	    case 'h':   Usage ();			return (OK);

	    case 'v':   verbose++;				break;
	    case 'd':   debug++;				break;
	    case 'r':   do_return++;	    	    		break; /* FIX */

	    case 'N':  	nthreads = vot_atoi (optval); argnum++;	break;
	    case 'P':  	pos      = NEXTARG (optval);		break;
	    case 'R':  	release  = NEXTARG (optval);		break;
	    case 's':  	size     = NEXTARG (optval); 
			sprintf (rad_str, "%g", (rad = vot_atof(size) * 3600.));
			break;
	    case 't':  	type     = NEXTARG (optval);		break;
	    case 'z':  	redshift = NEXTARG (optval);		break;

	    case 'c':  	count++;				break;
	    case 'm':  	do_meta++;				break;
	    case 'u':  	urls++; 				break;
	    case 'S':  	do_samp++; 				break;
	    case 'D':  	do_delete++; 				break;
	    case 'T':  	asSpec--; 				break;
	    case 'l':  	limit 	 = NEXTARG (optval);		break;

	    case 'f':   iname 	 = NEXTARG (optval);		/* NYI	*/
			have_infile++;
			break;
	    case 'O':  	oname 	 = NEXTARG (optval);		break;
	    case 'o':  	object 	 = NEXTARG (optval);		
			if (argv[argnum]) {
    		    	    Sesame sr;
		    	    char  *opts = "runid=voc.vosloanspec,use_cache=no";

                    	    if (voc_initVOClient (opts) != OK) {
                        	fprintf (stderr,"ERROR: can't open VOClient\n");
                        	return (ERR);
                    	    }
		    	    sr = voc_nameResolver (vo_urlEncode (argv[argnum]));
			    if (sr == 0) {
                        	fprintf (stderr,"ERROR: can't resolve '%s'\n",
				    argv[argnum]);
                        	return (ERR);
			    } else {
		    	        sprintf (ra_str, "%f", 
				    (ra=voc_resolverRA (sr)));
		    	        sprintf (dec_str, "%f", 
				    (dec=voc_resolverDEC (sr)));
			    }
			}
			break;
	    case 'b':  	basename = NEXTARG (optval);		break;
	    default:
		fprintf (stderr, "Invalid option '%s'\n", optval);
		return (1);
	    }

        } else if (ch == PARG_ERR) {
            return (ERR);

	} else {

	    switch (argc - argnum) {
	    case -1:					/* obj */
		argnum--;
	    case 0:					/* obj */
		argnum--;
	    case 1:					/* obj */
		/*  resolve object pos
		 */
		if (argv[argnum]) {
    		    Sesame sr;
		    char  *opts = "runid=voc.vosloanspec,use_cache=no";

                    if (voc_initVOClient (opts) != OK) {
                        fprintf (stderr, "ERROR: cannot open VOClient\n");
                        return (ERR);
                    }
		    sr = voc_nameResolver (vo_urlEncode (argv[argnum]));
		    if (sr == 0) {
                       	fprintf (stderr,"ERROR: can't resolve '%s'\n",
			    argv[argnum]);
                       	return (ERR);
		    }
		    sprintf (ra_str,  "%f", (ra  = voc_resolverRA (sr)));
		    sprintf (dec_str, "%f", (dec = voc_resolverDEC (sr)));
		}
		break;
	    case 2:					/* ra dec */
		ra  = vot_atof (argv[argnum]);
		dec = vot_atof (argv[argnum+1]);
		strcpy (ra_str, argv[argnum]);
		strcpy (dec_str, argv[argnum+1]);
		break;
	    case 3:					/* ra dec rad */
		ra  = vot_atof (argv[argnum]);
		dec = vot_atof (argv[argnum+1]);
		rad = vot_atof (argv[argnum+2]);
		strcpy (ra_str, argv[argnum]);
		strcpy (dec_str, argv[argnum+1]);
		strcpy (rad_str, argv[argnum+2]);
		break;
	    }
	    break;
	}
    }

    if (debug)
	fprintf (stderr, "ra = %f  dec = %f  rad = %f\n", ra, dec, rad);

    /* Sanity checks
     */
    if (iname == NULL) iname = strdup ("stdin");
    if (oname == NULL) oname = strdup ("stdout");
    if (strcmp (iname, "-") == 0) { free (iname), iname = strdup ("stdin"); }
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }

    if (rad > 36000)		/* enforce max query size of 10 deg	*/
	rad = 36000;
	

    /*  Append parameters to the base URL based on the cmdline options.  We
     *  parse the positions/sizes and deal with files below.
     */
    if (limit)    	 strcat (url, "&limit="),    strcat (url, limit); 
    if (type) 	  	 strcat (url, "&class="),    strcat (url, type); 
    if (release)  	 strcat (url, "&release="),  strcat (url, release); 
    if (redshift) 	 strcat (url, "&redshift="), strcat (url, redshift); 
    if (count)    	 strcat (url, "&count");
    strcat (url, "&urls");				/* always get URLS    */

    if (ra != -999.9) 
	strcat (url, "&ra="), strcat (url, ra_str), strcat (url,"d");; 
    if (dec != -999.9) 
	strcat (url, "&dec="), strcat (url, dec_str); 
    strcat (url, "&radius="); strcat (url, rad_str); 

    if (debug)
	fprintf (stderr, "url[0] = %s\n", url);


    /*  Access the query URL, saving the result to the named filed.  We'll
     *  process the options on the resulting file.
     */
    if (vos_getURL (url, tmp) >= 0) {
        if (count) {
	    printf ("%d\n", vos_getCountValue (tmp));

        } else if (urls) {
	    printf ("%s", (urlList= vos_getURLValues (tmp)));

        } else {
	    char *url = NULL, *base = NULL, tok[SZ_URL], fname[SZ_FNAME];
	    int   sampH = vos_sampInit ();

	    url = urlList = vos_getURLValues (tmp);
    	    memset (tok, 0, SZ_URL);
	    base = (basename ? basename : "sdss");

	    if (do_samp) {
		if (verbose)
		    fprintf (stderr, "Initializing SAMP ....\n");
		sampH = sampInit ("vosloanspec", "VOClient SDSS Task");
	    }

    	    for (specnum=0; *url && sscanf (url, "%s", tok); specnum++) {
		memset (fname, 0, SZ_FNAME);
		sprintf (fname, "%s%04d.fits", base, specnum);

		if (do_samp) {
		    /*  Broadcast URL as a SAMP message, either as a bintable
		     *  or an ssa-generic spectrum.
		     */
		    if (sampH) {
			int  stat = -1;

			if (verbose)
		    	    fprintf (stderr, "Loading '%s' ....\n", tok);
			if (asSpec) {
        	            stat = samp_specLoadSSAGeneric (sampH, "all", 
                	        tok,            /* URL          */
                	        0,              /* Map meta     */  /* NYI */
                	        fname,          /* spectrumId   */
                	        fname);         /* name         */
			} else {
        		    stat = samp_tableLoadFITS (sampH, "all", 
                		tok,            /* URL/file     */
                		fname,          /* tblId        */
                		fname);         /* name         */
			}
		    }

		} else {
		    /*  Download URL to a local file.
		     */
		    aclist[specnum].tnum = ((nthreads == 1) ? 0 : 
						(specnum % nthreads));
		    strcpy (aclist[specnum].url, tok);
		    strcpy (aclist[specnum].fname, fname);
		    ndownloads++;
		}

		url += strlen (tok) + 1;
        	memset (tok, 0, SZ_URL);
    	    }

	    /*  Process the access list (downloads) and metadata request.
	     */
	    if (ndownloads) {
		vot_procAclist ();
		if (do_meta)
		    vot_procMeta ();
	    }

	    if (do_samp && sampH)
		sampClose (sampH);
        }
    } else 
	status = ERR;
	    

    /* Clean up.
     */
    if (fd != stdout)
	fclose (fd);
    if (urlList) 
	free ((void *) urlList);

    if (pos)  free (pos);
    if (type)  free (type);
    if (size)  free (size);
    if (limit)  free (limit);
    if (iname)  free (iname);
    if (oname)  free (oname);
    if (object)  free (object);
    if (release)  free (release);
    if (redshift)  free (redshift);
    if (basename)  free (basename);

    vo_paramFree (argc, pargv);

    return (status);
}


/** 
 *  VOT_PROCACLIST -- Process the access list.
 */
static void
vot_procAclist (void)
{
    /* Spawn the worker threads.
     */
    int  rc = 0, tc = 0, status = 0, tnum[MAX_THREADS];
    pthread_attr_t  attr;                   /* thread attributes    */
    pthread_t  thread[MAX_THREADS];


    /*  Initialize the service processing thread attributes and run 'em.
     */
    pthread_attr_init (&attr);
    pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_JOINABLE);

    for (tc=0; tc < nthreads; tc++) {
        tnum[tc] = tc;
        if ((rc = pthread_create (&thread[tc], &attr, vot_getAclist,
            (void *)&tnum[tc]))) {
                fprintf (stderr, "ERROR: pthread_create() fails, code=%d\n",
                    rc);
                return;
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
                return;
            }
        } 
    }

    if (verbose)
        fprintf (stderr, "Downloaded %d files -- complete\n", ndownloads);
}


/** 
 *  VOT_GETACLIST -- Download all the files for the specified thread.
 */
static void *
vot_getAclist (void *arg)
{
    register int i;
    int threadNum = 0;


    if (arg) {
        threadNum = *(int *)arg;

        for (i=0; i < ndownloads; i++) {
            if (aclist[i].tnum == threadNum) {
		if (verbose)
		    fprintf (stderr, "%s  ->  %s\n", 
			aclist[i].fname, aclist[i].url);
                vos_getURL (aclist[i].url, aclist[i].fname);
            }
        }
    }
    return ((void *) NULL);
}


/** 
 *  VOT_PROCMETA -- Process the metadata request.
 */
static void
vot_procMeta (void)
{
    int  i, fd, nread;
    char rec[(4 * 2880)];

    for (i=0; i < ndownloads; i++) {
	if (access (aclist[i].fname, R_OK) == 0) {
	    if ((fd = open (aclist[i].fname, O_RDONLY)) >= 0) {
	        nread = read (fd, rec, (size_t) (4 * 2880));
	        printf ("%s\t", aclist[i].fname);
	        printf ("%11.6f\t", atof (strstr (rec, "PLUG_RA") + 11));
	        printf ("%12.6f\n", atof (strstr (rec, "PLUG_DEC") + 11));
	        close (fd);

	        if (do_delete)
	            unlink (aclist[i].fname);
	    }
	}
    }
}


/**
 *  VOS_GETCOUNTVALUE -- Get the value from a count request.
 */
static int
vos_getCountValue (char *fname)
{
    char  buf[SZ_LINE], *line;
    FILE  *fd;

    if (fname && (fd = fopen (fname, "r"))) {
        memset (buf, 0, SZ_LINE);
        line = fgets (buf, SZ_LINE, fd);
        fclose (fd);
        unlink (fname);

        return (vot_atoi (line));
    }
    return (-1);
}


/**
 *  VOS_GETURLVALUES -- Get the values from a URL request.
 */
static char *
vos_getURLValues (char *fname)
{
    char  *ptr, *out, *ip, *op;
    FILE  *fd;
    extern int vos_fileRead();


    if (fname && (fd = fopen (fname, "r"))) {
	struct stat info;
	size_t  sz, nr;

	if (stat (fname, &info) < 0)		/* read the whole file 	*/
            return (NULL);
    	sz = info.st_size;
    	ptr = calloc (sz + 1, sizeof(char));
    	out = calloc (sz + 1, sizeof(char));
	nr = vos_fileRead (fileno(fd), ptr, sz);

	/*  Check for a service outage.
	 */
	if (strstr (ptr, "uWSGI Error"))  {
	    if (ptr) free (ptr);
	    if (out) free (out);
	    return ( strdup ("SDSS Service not available\n") );
	}

	for (ip=ptr, op=out; *ip; ip++) {
	    if (*ip == '[')
		;
	    if (*ip == '"') {
		ip++;
		while (*ip != '"')		/* copy token		*/
		    *op++ = *ip++;
		*op++ = '\n';
		while (*ip != '"')		/* skip to next  token	*/
		    ip++;
	    }
	}

	free ((void *) ptr);
        fclose (fd);
        unlink (fname);

        return (out);
    }

    return (NULL);
}


/**
 *  VOS_SAMPINIT -- Initialize the SAMP interface connection.
 */
static int
vos_sampInit (void)
{
    int  sampH = 0;


    /*  If there's no Hub running, don't bother ....
     */
    if (!samp_hubRunning())
	return (0);

    /*  Initialize the SAMP interface.
    */
    sampH = sampInit ("vosloanspec", "VOClient SDSS Spectrum Task");
    if (samp_hubActive (sampH) == 0)
        return (0);

    /*  Force 'asynch' messaging.
     */
    samp_setSyncMode (sampH);

    /*  Load metadata.
     */
    samp_Metadata (sampH, "author.name",   "Mike Fitzpatrick, NOAO");
    samp_Metadata (sampH, "author.email",  "fitz@noao.edu");
    samp_Metadata (sampH, "samp.description.html",
        	"http://iraf.noao.edu/voclient/vosamp.html");
    samp_Metadata (sampH, "samp.icon.url", 
		"http://iraf.noao.edu/voclient/vao_icon.gif");

    /*  Register with the Hub and begin messaging.
     */
    sampStartup (sampH);


    return (sampH);
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
  fprintf (stderr, "\n  Usage:\n\t"
    "vosloanspec [<opts>] <obj> | {<ra> <dec>} | {<ra> <dec> <radius>}\n\n"
    "  where\n"
    "     -%%,--test               Run unit tests\n"
    "     -h,--help               This message\n"
    "     -v,--verbose            Verbose Flag\n"
    "     -d,--debug              Debug Flag\n"
    "     -r,--return=<obj>       Return object\n"
    "\n"
    "     -P,--pos=<ra>,<dec>     Set query position (dec degrees)\n"
    "     -R,--release=<rel>      Data release (dr8/dr9/def=current)\n"
    "     -s,--size=<radius>      Set query radius (dec degrees)\n"
    "     -t,--type=<type>        Object type (all|galaxy|qso|star)\n"
    "     -z,--redshift=<zrange>  Select by redshift range string(s)\n"
    "\n"
    "     -c,--count              Return only count of results\n"
    "     -m,--meta               Print result position metadata\n"
    "     -D,--delete             Delete spectra after printing metadata\n"
    "     -l,--limit=<N>          Limit to top <N> results\n"
    "     -u,--urls               Get urls to spectra\n"
    "\n"
    "     -S,--samp               Broadcase urls to SAMP (as spectrum)\n"
    "     -T,--table              Broadcase urls as VOTable message\n"
    "     -N,--num=<N>            Number of download threads\n"
    "\n"
    "     -f,--file=<file>        Input file\n"
    "     -b,--base=<file>        Base filename\n"
    "     -O,--output=<file>      Output file\n"
    "     -o,--object=<obj>       Object name\n"
    "\n\n"
    "  Examples:\n\n"
    "    1)  Download all galaxy spectra w/in 0.1 deg of the Hubble UDF\n\n"
    "	    %% vosloanspec -s 0.1 -t galaxy HUDF\n"
    "	    %% vosloanspec --size=0.1 --type=galaxy HUDF\n"
    "\n"
    "    2)  Get only the positions of the SDSS spectra around a point\n\n"
    "	    %% vosloanspec -m -d m51\n"
    "	    %% vosloanspec --meta --delete m51\n"
    "\n"
    "    3)  Broadcast 5 spectra around 3c273 to SAMP-enabled apps\n\n"
    "	    %% vosloanspec -l 5 --samp 3c273\t# as a spectrum msg\n"
    "	    %% vosloanspec -l 5 --samp --table 3c273\t# as a table msg\n"
    "\n"
    "    4)  Get all QSO spectra with a redshift > 0.3 (Note an upper range\n"
    "        must be specified for the redshift range for a valid query)\n\n"
    "	    %% vosloanspec --redshift=0.3-1.0\n"
    "\n\n"
    );
}


/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
    Task *task = &self;

    vo_taskTest (task, "--help", NULL);

    vo_taskTest (task, "-s", "0.1", "-t", "galaxy", "HUDF", NULL);	// Ex 1a
    vo_taskTest (task, "--size=0.1", "--type=galaxy", "HUDF", NULL);	// Ex 1b
    vo_taskTest (task, "-m", "-D", "m51", NULL);			// Ex 2a
    vo_taskTest (task, "--meta", "--delete", "m51", NULL); 		// Ex 2b
    vo_taskTest (task, "-l", "5", "--samp", "3c273", NULL); 		// Ex 3a
    vo_taskTest (task, "-l", "5", "--samp", "--table", "3c273", NULL); 	// Ex 3b
    vo_taskTest (task, "--redshift=0.3-1.0", NULL);			// Ex 4

    vo_taskTestReport (self);
}
