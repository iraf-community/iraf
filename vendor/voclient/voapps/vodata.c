/**
 *  VODATA -- Query VO Data services (Cone, SIAP, etc)
 *
 *  Usage:  vodata [-<flags>] [<service>] [object|file|position]
 *
 *  Where
 *       -%%,--test             run unit tests
 *       -h,--nelp              this message
 *       -d,--debug             enable debug messages
 *       -r,--return            return result from method
 *
 *
 *
 *
 *  @file       vodata.c
 *  @author     Mike Fitzpatrick
 *  @date       7/13/07
 *
 *  @brief      Query VO Data services (Cone, SIAP, etc)
 */

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <getopt.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <sys/time.h>
#include <time.h>
#include "VOClient.h"
#include "voAppsP.h"
#include "samp.h"




#define VOD_DEBUG  (getenv("VOD_DBG")||access("/tmp/VOD_DBG",F_OK)==0)


/*  Task structure.
 */
typedef struct {
   char  *name;                         /* task name                    */
   int  (*func)(int argc, char **argv, size_t *len, void **result);

   int   ntests;                        /* number of unit tests         */
   int   npass;                         /* number of passed tests       */
   int   nfail;                         /* number of failed tests       */
} Task;



/* Local processing definitions.
*/
svcParams pars[MAX_THREADS];		/* parameter array		*/

int	format	    = F_CSV;		/* output format		*/
int	apos	    = 0;		/* argv position		*/
int	sv_apos	    = -1;		/* saved argv position		*/
int     nservices   = 0;		/* num. of cone services	*/
int     nobjects    = 0;		/* num. objects to query	*/
int     filenum     = 0;		/* download file number		*/
int     rd_stdin    = 0;		/* reading from stdin?		*/
int     wr_stdout   = 0;		/* writing to stdout?		*/
int     do_votable  = 0;                /* output a Resource VOTable    */
int     svcNumber   = -1;               /* service number to call	*/
int     dalOnly     = 1;                /* only query DAL services?     */
int     simple_out  = 0;                /* use simple output name?	*/

int	inventory   = FALSE;		/* do inventory?		*/
int	quiet	    = FALSE;		/* no output at all?		*/
int	verbose     = TRUE;		/* DAL verbose level		*/
int     all_data    = FALSE;		/* get all the data?		*/
int     file_get    = FALSE;		/* file number to get		*/
#ifdef REG10_KLUDGE
int     reg10       = FALSE;		/* use Registry 1.0 scheme?     */
#endif
int     raw_vizier  = FALSE;		/* use Registry 1.0 scheme?     */
int     count	    = TRUE;		/* return count of results	*/
int     meta	    = FALSE;		/* metadata return?		*/
int     extract	    = EX_NONE;		/* extract pos/acrefs?		*/
int     count_only  = FALSE;		/* print only matched records?  */
int     save_res    = TRUE;		/* save results?		*/
int     all_named   = FALSE;		/* all objects named?		*/
int     use_name    = FALSE;		/* use object name in output?	*/
int     url_proc    = FALSE;		/* processing URLs only?	*/
int     force_svc   = FALSE;		/* assume URLs are ServiceURLs?	*/
int     svc_list    = FALSE;		/* list services queried        */
int     obj_list    = FALSE;		/* list objects queried         */
int     fixed_svc   = FALSE;		/* service is fixed on cmdline  */
int     fixed_obj   = FALSE;		/* object is fixed on cmdline   */
int     fixed_pos   = FALSE;		/* position is fixed on cmdline */
int     data_type   = DT_ANY;		/* data type			*/
int     proxy       = FALSE;		/* use proxy server		*/
int     res_all     = FALSE;		/* print all results?		*/
int     force_read  = FALSE;		/* force reading of input table	*/
int     longlines   = FALSE;		/* output long lines?		*/
int     iportal     = FALSE;		/* iportal support?		*/
int     numout      = FALSE;		/* numeric output sorting?	*/
int     samp        = FALSE;		/* broadcast table via SAMP	*/

int	max_download= DEF_DOWNLOADS;	/* max download procs to run	*/
int	max_procs   = DEF_NPROCS;	/* max children to run		*/
int	max_threads = DEF_NTHREADS;	/* max threads to run		*/

int     table_hskip = 0;		/* no. of table eeader to skip	*/
int     table_nlines= 0;		/* max lines of table to read	*/
int     table_sample= 1;		/* table sample			*/

int     group           = 0;		/* resolve identifiers in groups*/
int     nterms          = 0;		/* No. of terms			*/
char   *terms[128];			/* search terms			*/

char	*typestr    = (char *) NULL;	/* service type string		*/
char	*bpass      = (char *) NULL;	/* bandpass type string		*/
char	*output     = (char *) NULL;	/* root output filename		*/
char    *delim	    = " \t,|;";		/* input table delimiter	*/
char    *cols	    = "1,2";		/* input table columns		*/
char    *ecols	    = (char *) NULL;	/* input table exact columns	*/
char    *sources    = (char *) NULL;    /* source file			*/
char    *resources  = (char *) NULL;    /* resource file		*/
char    *sampName   = (char *) NULL;    /* SAMP appName 		*/

char	*d2_band    = (char *) NULL;	/* DAL2 BAND parameter		*/
char	*d2_time    = (char *) NULL;	/* DAL2 TIME parameter		*/
char	*d2_format  = (char *) NULL;	/* DAL2 FORMAT parameter	*/
char	*d2_version = (char *) NULL;	/* DAL2 VERSION parameter	*/

char	*tmpdir	    = "/tmp/";		/* temp directory		*/
char 	wrkdir[SZ_FNAME];		/* working directory		*/

double	sr	    = DEF_SR;		/* default search radius	*/

int	dverbose    = 0;		/* verbose debug output?	*/
int	debug	    = 0;		/* debug output?		*/
int     samp_p      = 0;		/* SAMP interface handler	*/

static  int status  = OK;		/* return status		*/


time_t	rs_time	    = (time_t) 0, 
	re_time	    = (time_t) 0;	/* reg/obj resolution times	*/
time_t	qs_time	    = (time_t) 0, 
	qe_time	    = (time_t) 0;	/* data query times		*/
time_t	as_time	    = (time_t) 0, 
	ae_time	    = (time_t) 0;	/* data access times		*/

Service  *svcList, *svcTail;		/* Service linked list		*/
Object   *objList, *objTail;		/* Obj/Posn linked list		*/
Acref    *acList, *acTail;		/* Acref linked list		*/
int      nacrefs    = 0;		/* no. of acrefs		*/

FILE	*arg_fd	    = (FILE *) NULL;	/* argument-file descriptor	*/


/* KML Options.
*/
int     kml_max         = 50,           /* max placemarks to write      */
        kml_sample      = 0,            /* output sample step           */
        kml_region      = TRUE,         /* draw bounding region poly    */
        kml_verbose     = TRUE,         /* verbose labels		*/
        kml_label       = TRUE,         /* draw placemark labels        */
        kml_byObj       = TRUE,         /* group by object/position	*/
        kml_bySvc       = FALSE,        /* group by service name	*/
        kml_byBoth      = FALSE;        /* group by both		*/

/* HTML Options.
*/
int     html_header     = TRUE,         /* print a HTML header on page	*/
        html_border     = TRUE,         /* put a border on the table?	*/
        html_color      = TRUE;         /* colorize the table		*/


extern int   errno;			/* system error code		*/

extern int   ra_col,  ra_span,		/* table input			*/
	     dec_col, dec_span,
	     id_col,  id_span;
extern int   svcIndex, objIndex;

extern int   isDecimal (char *s);
extern int   isSexagesimal (char *s);

extern int   vot_parseObjectList (char *list, int isCmdLine);
extern int   vot_countObjectList (void);
extern int   vot_parseServiceList (char *list, int dalOnly);
extern int   vot_countServiceList (void);
extern int   vot_decodeRanges (char *range_string, int *ranges, int max_ranges,
		int *nvalues);
extern int   is_in_range (int ranges[], int number);
extern int   vot_atoi (char *v);

extern void  vot_addToAclist (char *url, char *fname);
extern void  vot_procAclist (void);
extern void  vot_freeAclist (void);
extern void  vot_freeServiceList (void);
extern void  vot_resetServiceCounters (void);
extern void  vot_freeObjectList (void);
extern void  vot_printCountHdr (void);
extern void  vot_readObjFile (char *fname);
extern void  vot_readSvcFile (char *fname, int dalOnly);

extern double vot_atof (char *v);

/*  Tasking execution procedure.
 */
extern int  vo_runTask (char *method, Task *apps, int argc, char **argv, 
		size_t *len, void **result);
extern int  vo_taskTest (Task *self, char *arg, ...);
extern void vo_taskTestFile (char *str, char *fname);
extern void vo_taskTestReport (Task self);

extern int  vo_setResultFromFile (char *fname, size_t *len, void **data);
extern int  vo_setResultFromString (char *str, size_t *len, void **data);
extern int  vo_setResultFromInt (int value, size_t *len, void **data);
extern int  vo_setResultFromReal (float value, size_t *len, void **data);
extern int  vo_appendResultFromString (char *str, size_t *len, void **data,
                size_t *maxlen);


#ifdef VO_INVENTORY
extern char *vot_doInventory (void);
#endif
extern char *vot_urlFname (char *url);
extern char *vot_getOFName (svcParams *pars, char *extn, int pid);
extern char *vot_getOFIndex (svcParams *pars, char *extn, int pid);
extern char *vot_normalize (char *str);
extern char *vot_svcTypeCode (int type);

static int   vot_parseArgToken (char *arg, char *next, int pos, int *inc);
static int   vot_validateOptions (void);
static int   vot_getNextCmdline (void);
static void  vot_runSvcThreads (void);
static void  vot_printProcStat (Proc *procList, char *svc_name, int fail_only);
static void  vot_setProcStat (Service *svc, int pid, int status);

static void  vot_printProcTime ();
static char *vot_requiredArg (char *arg);
static char *vot_optionalArg (char *arg);
static char  vot_setArgWord (char *arg, char *val);
static char *vot_stat2code (int status);
static char *vizPatch (char *url);
/*
static char *vot_cnvType (char *intype);
*/

static void  vot_printUsage (void);
static void  vot_printExamples (void);

void   *vot_procObjs (void *arg);
void    vot_printSvcList (Service *sl);
void    vot_printSvcHdr (void);

    
pthread_mutex_t svc_mutex = PTHREAD_MUTEX_INITIALIZER;


/*  Task specific option declarations.
 */
int  vodata (int argc, char **argv, size_t *len, void **result);

static int mf = 0;
static Task  self  	= {  "vodata",  vodata,  0,  0,  0  };

/*  Note:  the leading ':' in the opts string is required to suppress errors
 */
static char *opts  	= ":%hrNSACFHIKMO:RTVXab:ce:fgi:mno:p:qr:s:t:uv";

static struct option long_opts[] = {
    { "test",        2, 0, '%' },	/* test (std)			*/
    { "help",        2, 0, 'h' },	/* help (std)			*/
    { "return",      2, 0, 'r' },	/* return (std)			*/

    { "numeric",     2, 0, 'N' },	/* numeric output name		*/
    { "simple",      2, 0, 'S' },	/* simple output name		*/

    { "ascii",       2, 0, 'A' },	/* ASCII output			*/
    { "csv",         2, 0, 'C' },	/* CSV output			*/
    { "fits",        2, 0, 'F' },	/* FITS table output		*/
    { "html",        2, 0, 'H' },	/* HTML output			*/
    { "inventory",   2, 0, 'I' },	/* inventory (not used)		*/
    { "kml",         2, 0, 'K' },	/* KML output			*/
    { "verbmeta",    2, 0, 'M' },	/* verbose metadata		*/
    { "output",      2, 0, 'O' },	/* root output name		*/
    { "raw",         2, 0, 'R' },	/* Raw output			*/
    { "tsv",         2, 0, 'T' },	/* TSV output			*/
    { "votable",     2, 0, 'V' },	/* VOTable output		*/
    { "xml",         2, 0, 'X' },	/* XML output			*/

    { "all",         0, 0, 'a' },	/* all data			*/
    { "bandpass",    1, 0, 'b' },	/* bandpass			*/
    { "count",       2, 0, 'c' },	/* count results		*/
    { "debug",       2, 0, 'd' },	/* debug			*/
    { "extract",     1, 0, 'e' },	/* extract			*/
    { "force",       2, 0, 'f' },	/* force table read		*/
    { "get",         2, 0, 'g' },	/* get specific results		*/
    { "input",       1, 0, 'i' },	/* get args from input file	*/
    { "meta",        2, 0, 'm' },	/* metadata			*/
    { "nosave",      2, 0, 'n' },	/* no-save results		*/
    { "object",      1, 0, 'o' },	/* query object name		*/
    { "pos",         1, 0, 'p' },	/* query position		*/
    { "quiet",       2, 0, 'q' },	/* suppress output		*/
    { "sr",          1, 0, 'r' },	/* search radius		*/
    { "svc",         1, 0, 's' },	/* data service			*/
    { "type",        1, 0, 't' },	/* type string			*/
    { "url",         2, 0, 'u' },	/* url download			*/
    { "verbose",     2, 0, 'v' },	/* verbose			*/
 
    { "bandpass",    1, &mf,  1 },	/* req'd arg word		*/
    { "cols",        1, &mf,  2 },	/* req'd arg word		*/
    { "ecols",       1, &mf,  3 },	/* req'd arg word		*/
    { "delim",       1, &mf,  4 },	/* req'd arg word		*/
    { "kml",         1, &mf,  5 },	/* req'd arg word		*/
    { "max",         1, &mf,  6 },	/* req'd arg word		*/
    { "nlines",      1, &mf,  7 },	/* req'd arg word		*/
    { "object",      1, &mf,  8 },	/* req'd arg word		*/
    { "output",      1, &mf,  9 },	/* req'd arg word		*/
    { "pos",         1, &mf, 10 },	/* req'd arg word		*/
    { "sample",      1, &mf, 11 },	/* req'd arg word		*/
    { "svc",         1, &mf, 12 },	/* req'd arg word		*/
    { "type",        1, &mf, 13 },	/* req'd arg word		*/
    { "web",         1, &mf, 15 },	/* req'd arg word		*/
    { "band",        1, &mf, 16 },	/* req'd arg word		*/
    { "time",        1, &mf, 17 },	/* req'd arg word		*/
    { "format",      1, &mf, 18 },	/* req'd arg word		*/
    { "version",     1, &mf, 19 },	/* req'd arg word		*/

    { "onefile",     2, &mf, 20 },	/* one-file output		*/
    { "extract",     2, &mf, 21 },	/* opt arg word			*/
    { "ep",          2, &mf, 22 },	/* opt arg word			*/
    { "eu",          2, &mf, 23 },	/* opt arg word			*/
    { "eh",          2, &mf, 24 },	/* opt arg word			*/
    { "ek",          2, &mf, 25 },	/* opt arg word			*/
    { "eK",          2, &mf, 26 },	/* opt arg word			*/
    { "hskip",       2, &mf, 27 },	/* opt arg word			*/

    { "wh",          2, &mf, 30 },	/* opt arg word			*/
    { "wb",          2, &mf, 31 },	/* opt arg word			*/
    { "wc",          2, &mf, 32 },	/* opt arg word			*/

    { "webborder",   2, &mf, 40 },	/* no arg word			*/
    { "webheader",   2, &mf, 41 },	/* no arg word			*/
    { "webcolor",    2, &mf, 42 },	/* no arg word			*/
    { "webnoborder", 2, &mf, 43 },	/* no arg word			*/
    { "webnoheader", 2, &mf, 44 },	/* no arg word			*/
    { "webnocolor",  2, &mf, 45 },	/* no arg word			*/
    { "vverbose",    0, &mf, 46 },	/* no arg word			*/

    { "debug",       0, &mf, 99 },	/* no arg word			*/

    { NULL,       0, 0,  0 }
};

extern char **vo_paramInit (int argc, char *argv[], 
		char *opts, struct option long_opts[]);
extern int    vo_paramNext (char *opts, struct option long_opts[], 
                int argc, char *argv[], char *optval, int *posindex);
extern void   vo_paramFree (int argc, char *argv[]);


static void Usage (void);
static void Tests (char *input);





/************************************************************************
**  Program main()
*/
int
vodata (int argc, char *argv[], size_t *reslen, void **result)
{
    int    i, ch, pos=0;
    char  *eval, *argfile, *next_arg, posn[SZ_LINE];
    char  **pargv, optval[SZ_FNAME];


    /*  Initialize the VOClient code.  Error messages are printed by the
    **  interface so we just quit if there is a problem.
    */
    if (voc_initVOClient ("runid=voc.vodata") == ERR)  {
	fprintf (stderr, "Error: cannot connect to VOClient daemon\n");
        return (ERR);
    }

			
    /* Initialize the global structs.
    */
    memset (&colRange, 0, sizeof (Range));
    memset (&rowRange, 0, sizeof (Range));
    memset (&fileRange, 0, sizeof (Range));
    colRange.nvalues  = RANGE_ALL;
    rowRange.nvalues  = RANGE_ALL;
    fileRange.nvalues = RANGE_NONE;


    /* Get some environment definitions.  We allow the command-line flags
    ** to override these values.
    */
    if ((eval = getenv("VOC_MAX_DOWNLOADS")))
	max_download = vot_atoi (eval);
    if ((eval = getenv("VOC_MAX_PROCS")))
	max_procs = vot_atoi (eval);
    if ((eval = getenv("VOC_MAX_THREADS")))
	max_threads = vot_atoi (eval);


    /*  Initializations.
     */
    *reslen   = 0;
    *result   = NULL;
    apos      = 0;
    svcIndex  = 0;
    objIndex  = 0;
    all_data  = 0;


    rs_time = time ((time_t) NULL);

    /*  Now process the command line arguments. 
     */
    if (VOD_DEBUG) {
        fprintf (stderr, "Command:");
        for (i=0; i < argc; i++) fprintf (stderr, " '%s'", argv[i]);
        fprintf (stderr, "\n");
        for (i=0; i < argc; i++) fprintf (stderr, "%s ", argv[i]);
        fprintf (stderr, "\n\n");
    }



    /*  Parse the argument list.  
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    for (i=1; i < argc; i++) {

	memset (optval, 0, SZ_FNAME);
	ch = vo_paramNext (opts, long_opts, argc, pargv, optval, &pos);
#ifdef PARAM_DBG
	fprintf (stderr, "ch = '%c' %d  optval = '%s'\n", 
  	    ch, ch, (optval[0] ? optval : "N/A"));
#endif

        if (ch > 0) {
            switch (ch) {
 	    case '?':				/* unknown		*/
		break;

 	    case 'h':				/* help			*/
		Usage ();
    		voc_closeVOClient (0);
		return (OK);

 	    case '%':				/* unit tests		*/
    		voc_closeVOClient (0);			
		Tests (optval);
		return (self.nfail);

            case 'N':				/* numeric output name	*/
		numout++;
		break;
            case 'S':				/* simple output name	*/
		simple_out++;
		break;

            case 'A':				/* ASCII output		*/
		format = F_ASCII;
		break;
            case 'C':				/* CSV output		*/
		format = F_CSV;
		break;
            case 'F':    			/* FITS table output	*/
		fprintf (stderr,
		    "FITS tables not yet implemented, using ASCII\n");
		format = F_ASCII;           
		break;
            case 'H':				/* HTML output		*/
                extract |= EX_HTML;
		format = F_CSV | F_HTML;   
		if (output && output[0] == '-')
                    extract |= EX_COLLECT;
		break;
#ifdef VO_INVENTORY
            case 'I':				/* inventory count	*/
		inventory++;
		count++;
		break;
#endif
            case 'K':				/* KML output		*/
                extract |= EX_KML;
		format = F_CSV | F_KML;    
		break;
            case 'R':				/* RAW output		*/
            case 'V':				/* VOTable output	*/
		format = F_RAW;
		if (output && output[0] == '-')
                    	extract |= EX_COLLECT;
		break;
            case 'T':				/* TSV output		*/
		format = F_TSV;
		break;
            case 'X':				/* XML output		*/
                extract |= EX_XML;
		format = F_RAW | F_XML;    
		if (output && output[0] == '-')
                    extract |= EX_COLLECT;
		break;

            case 'O':    			/* root output name	*/
		//VOT_NEXTARG(argc,argv,i);
		output = strdup (optval);
		if (output[0] == '-') {
		    wr_stdout++;
		    quiet++;				
		    if ((extract & EX_XML) || (extract & EX_KML))
                    	extract |= EX_COLLECT;

		    memset (wrkdir, 0, SZ_FNAME);
		    sprintf (wrkdir, "%s/vod%d", tmpdir, (int)getpid());
		    if (access (wrkdir, R_OK|W_OK) != 0)
			mkdir (wrkdir, (mode_t)0666);
		    chdir (wrkdir);
		}
		break;

            case 'a':    			/* all data 		*/
		all_data++;				
		res_all++;
		/*sr = -1.0; */			/* flag to get all data */
		break;
            case 'b':				/* forced type string	*/
		if (optval[0] == '/' || isdigit(optval[0]))
		    d2_band = strdup (optval);
		else 
		    if (strncasecmp (optval, "any", 3) != 0)
		        bpass = strdup (optval);
		break;
            case 'c':    
		count++;				
		count_only = TRUE;
		break;

            case 'e':    			/* extract pos/acrefs?	*/
		if (strncmp (optval, "pos", 3) == 0)
		    extract |= EX_POS;
		else if (strncmp (optval, "url", 3) == 0)
		    extract |= EX_ACREF;
		else if (strncmp (optval, "head", 4) == 0)
		    extract |= EX_HTML;
		else if (strncmp (optval, "kml", 3) == 0)
		    extract |= EX_KML;
		else if (strncmp (optval, "xml", 3) == 0)
		    extract |= EX_XML;
		else if (strncmp (optval, "all", 3) == 0)
                    extract  = EX_ALL;
		else 
                    extract  = EX_ALL, i++;
		break;

            case 'f':    			/* force table read	*/
		force_read++;
		break;

            case 'g':			/* get specific results */    
		extract |= EX_ACREF;
		fileRange.nvalues = RANGE_ALL;
		file_get = RANGE_ALL;
		break;

            case 'i':		/* take remaining args from file */    
		VOT_NEXTARG(argc,argv,i);
		sv_apos = apos;
		argfile = strdup (argv[++i]);
		if (argfile[0] == '-') {
		    if (strlen (argfile) > 1) {
			fprintf (stderr, "ERROR: the '-i' flag requires ");
			fprintf (stderr, "a filename or '-' for stdin\n");
			exit (1);
		    } else if (rd_stdin) {
			fprintf (stderr,
				"ERROR: stdin can only be used once\n");
			exit (1);
		    } else {
			rd_stdin++;
			arg_fd = stdin;
		    }

		} else {
		    if (access (argfile, R_OK) == 0) {
		        /* Open the file, we'll process it below. 
		        */
		        if ((arg_fd = fopen(argfile,"r")) == (FILE *)NULL) {
		            fprintf (stderr,
				    "ERROR: Cannot open file '%s'\n", argfile);
		            return (ERR);
		        }
		    }
		}
		break;

            case 'M':			/* verbose meta 	*/
		meta++, verbose = 3;
		res_all++;
		break;
            case 'm':			/* meta flag		*/
		meta++;
		res_all++;
		break;

            case 'n':			/* no-save results	*/
		save_res = FALSE;
		count_only = TRUE;
	 	break;

            case 'o':			/* query object name	*/
		VOT_NEXTARG(argc,argv,i);
		use_name++;
		next_arg = argv[i+1];
		if (next_arg[0] == '-') {
		    if (rd_stdin) {
                        fprintf (stderr,
                            "ERROR: stdin can only be used once\n");
                        exit (1);

		    } else {
                        rd_stdin++;
		    	fixed_obj++;
			vot_readObjFile ("-");
		    }
		    i++;			/* advance argv 	*/
		} else if (inventory) {
		    vot_parseObjectList (argv[++i], TRUE);
		    sources = strdup (argv[i]);
		    if (debug)
			fprintf (stderr, "setting 'obj' sources = '%s'\n",
			    sources);
		    apos++;
		} else
		    vot_parseObjectList (argv[++i], TRUE);
		if (svcList)
		    apos++;
		break;

            case 'p':				/* query position	*/
		//VOT_NEXTARG(argc,argv,i);
		next_arg = argv[i+1];
		memset (posn, 0, SZ_LINE);
		if (next_arg[0] == '-') {
		    if (strlen (next_arg) > 1) {
			fprintf (stderr,"ERROR: the '-p' flag requires");
			fprintf (stderr," coords or '-' for stdin\n");
			return (ERR);
                    } else if (rd_stdin) {
                        fprintf(stderr, "ERROR: stdin can only be used once\n");
                        exit (1);
		    } else {
                        rd_stdin++, fixed_pos++;
			vot_readObjFile ("-");
		    }
		    i++;			/* advance argv 	*/
		} else {
		    if (isSexagesimal(optval) || isDecimal(optval)) {
		        vot_parseObjectList (optval, TRUE);
		    } else if (inventory) {
		        vot_parseObjectList (optval, TRUE);
			sources = strdup (optval);
			if (debug)
			    fprintf (stderr, 
				"setting pos sources='%s'\n", sources);
		    } else
		        vot_parseObjectList (optval, TRUE);
		    fixed_pos++;
		}
		break;

            case 'q':
		quiet++;				
		break;

            case 'r':  			/* search radius	*/
		VOT_NEXTARG(argc,argv,i);
		sr = vot_atof (argv[++i]);
		break;

            case 's':			/* data service		*/
		VOT_NEXTARG(argc,argv,i);
		if (strncmp (argv[i+1], "http", 4) == 0)
		   force_svc++;
		next_arg = argv[i+1];
		if (next_arg[0] == '-') {
		    if (strlen (next_arg) > 1) {
			fprintf(stderr,"ERROR: the '-s' flag requires ");
			fprintf(stderr,"a service name or '-' for stdin\n");
			exit (1);
		    } else
			vot_readSvcFile ("-", dalOnly);
		    i++;			/* advance argv 	*/
		} else if (isdigit (argv[i+1][0])) {
		    svcNumber = vot_atoi (argv[++i]);
		    vot_parseServiceList ((resources = strdup (argv[++i])), 
			dalOnly);
		    if (debug)
			fprintf (stderr, "setting resources = '%s'\n",
			    resources);
		} else {
		    vot_parseServiceList ((resources = strdup (argv[++i])), 
			dalOnly);
		    if (debug)
			fprintf (stderr, "setting resources = '%s'\n",
			    resources);
		}
		fixed_svc++;
		apos++;
		break;

            case 't':			/* forced type string	*/
		//VOT_NEXTARG(argc,argv,i);
		if (strncasecmp (optval, "any", 3) != 0)
		    typestr = strdup (optval), i++;
		break;

            case 'u':			/* forced url download	*/
		url_proc++;
		break;

            case 'v':			/* verbose flag		*/
		verbose = min(3,(verbose+1));
		break;
            }

        } else if (ch == 0) {
	    if ((ch=vot_setArgWord((char *)long_opts[pos].name, optval)) == 0) {
		fprintf (stderr, "unknown long option '%s'\n", 
		    long_opts[pos].name);
		status = ERR;
		goto cleanup_;
            }


#ifdef ENG_FLAGS
        } else if (argv[i][0] == '+') {
            len = strlen (argv[i]); 		/* "Engineering" flags.  */
            for (j=1; j < len; j++) {
                switch (argv[i][j]) {
                case 's':    
		    table_hskip = vot_atoi (argv[++i]);
		    break;
                case 'S':    
		    samp++;				
		    sampName = strdup (argv[++i]);
		    break;
                case 'i':    
		    iportal++;				
		    break;
                case 'd':    
		    debug++;				
		    break;
                case 'l':    
		    svc_list = 1;
		    obj_list = 1;
		    break;
                case 'q':    
		    quiet = 0;
		    break;
                case 'a':    			/* null all flag	*/
		    break;
                case 'n':    			/* null flag		*/
		    break;

#ifdef REG10_KLUDGE
                case 'r':    
		    reg10++;				
		    break;
#endif
                case 'r':    
		    raw_vizier = TRUE;				
		    break;
                case 't':    
		    tmpdir = strdup (argv[++i]);
		    break;
                case 'v':    
		    dverbose++;				
		    break;
                }
            }
#endif

        } else {
	    /* Parse the arguments according to an assumed calling 
	    ** order, e.g.
	    **
	    **	  vodata [ url_list | [ resource [obj | ra dec] [sr] ]
	    **
	    ** By default we assume 'sr' is in degrees, the ra/dec will
	    ** be assumed to be ICRS J200 and may be sexagesimal or decimal.
	    ** Since we're parsing the argv[], be sure to take into account
	    ** any increments added because of consumed arguments.
	    */
	    int  inc = 0;

    	    for ( ; i < argc; i++) {
//if (i > 2 && apos == 0) i--;
//fprintf (stderr, "pos: '%s' '%s' apos=%d i=%d\n", argv[i], argv[i+1], apos, i);
	        if (vot_parseArgToken (argv[i], argv[i+1], apos, &inc) != OK) {
	    	    exit (1);
	        } else 
	            i += inc;		/* add the argv[] increment  	*/

	        apos++;			/* update arg position		*/
    	    }
	}
    }
    re_time = time ((time_t) NULL);


    /* Close VOClient connection.  Each child process will need to reopen
    ** their own connection, and if we're processing an argument file we'll
    ** open/close it again as needed.
    */
    voc_closeVOClient (0);			


    /*  If we're broadcasting the result tables, open the SAMP connection
    **  now and let the child processes simply send the message.
    if (samp) {
        samp_p = sampInit ((sampname ? sampName : "VOData"), 
	    "VOClient Data Access");
        samp_setSyncMode (samp_p);
        sampStartup (samp_p);
    }
    */

    
    /*  The control logic below allows us to process more than one
    **  "command-line".  If we got our services and objects from the 
    **  true command-line the loop will break at the bottom and we only
    **  execute once.  The service and object lists were created when
    **  processing the arguments above.  OTOH, if we're using an argument
    **  file, those lists are now NULL and we'll read each line of the
    **  argfile and create them now.  In either case, we clean up and
    **  reset the lists at the bottom of the loop.
    */
    
    while (1) {
    
	/*  If we're processing from the stdin or an argument file, 
	**  get the next line of the file and fake the commandline to
	**  set the resource and object lists.  Note we clean up and
	**  reset below to permit each iteration to process a different
	**  number of resources or objects.
	*/
        if (arg_fd) {
    	    if (vot_getNextCmdline () != OK)
		break;
	}
    
    	
        /*  Tally up the the number of services and objects to be queried.
        */
        if ((nservices = vot_countServiceList()) == 1) 
	    svc_list = 0;
        if ((nobjects = vot_countObjectList()) == 1)
	    obj_list = 0;
    
#ifdef VO_INVENTORY
	/*  If we're calling the Inventory service, branch to do it here and
	**  then continue with the loop.
	*/
	if (inventory) {
	    if (debug)
		fprintf (stderr, "inventory nserv = %d    nobj = %d\n",
		    nservices, nobjects);

	    if (nobjects == 0 && sources == NULL) {
		fprintf (stderr, "No object position(s) specified.\n");
		exit (1);

	    } else {
    	        vot_printSvcHdr ();
    		qs_time = time ((time_t) NULL);
		(void) vot_doInventory ();
    		qe_time = time ((time_t) NULL);
	    }

            vot_printProcTime ();
	    break;
	}
#endif

        /*  If we have services to call and aren't simply downloading URLs,
        **  process the service queries first.
        */
        if (svcList && !url_proc) {
    
            /* See whether any flags negate other options.
            */
    	    if (vot_validateOptions() == ERR)
		break;
    
            /* Print header information.
            */
    	    vot_printSvcHdr ();
    
            /* Now run the serice queries.  Each service is run on a separate
            ** thread, we'll handle summary output and any postprocessing later.
            */
            vot_runSvcThreads ();
        }
    
        /*  Process the access reference list to download any pending data.
        */
        if (acList && nacrefs)
    	    vot_procAclist ();
    
        /*  Free up any memory we may have allocated.  Counters and pointers
	**  are reset in each routine.
        */
        vot_freeAclist ();		/* access reference list	*/
        vot_freeObjectList ();		/* object list			*/
	if (sv_apos < 0)
            vot_freeServiceList ();	/* VO resource list		*/
	else
            vot_resetServiceCounters ();
    

        /*  Print an approximate summary of the processing time required.
        */
        vot_printProcTime ();

	/*  If we're not processing from an argument file or the 
	**  stdin, we've processed the argv from the commandline, so
	**  break here.
	*/
        if (!arg_fd)
    	    break;
    }


cleanup_:
    if (arg_fd && arg_fd != stdin)	/* close the argument file	*/
	fclose (arg_fd);
			
    if (wr_stdout && wrkdir[0] && access (wrkdir, R_OK|W_OK) == 0)
	rmdir (wrkdir); 		/* clean up wrkdirs		*/

    /*
    if (samp)
	samp_UnRegister (samp_p);
    */

    if (bpass)    free ( (void *) bpass);
    if (typestr)  free ( (void *) typestr);

    if (d2_band)    free ( (void *) d2_band);
    if (d2_time)    free ( (void *) d2_time);
    if (d2_format)  free ( (void *) d2_format);
    if (d2_version) free ( (void *) d2_version);

    return ( status );
}




/*  Set an argument that may optionally be specified as an entire word.
*/

#define	ARG_DONE	-1

static char
vot_setArgWord (char *arg, char *val)
{
#ifdef PARAM_DBG
    fprintf (stderr, "setArg = '%s'  val = '%s'\n", arg, val);
#endif

    if (arg[0] == '-') {
	fprintf (stderr, "Invalid argument string '--'\n");
	return (0);
						/* '--' only  FLAGS	*/
    } else if (strncmp (arg, "debug", 5) == 0) {
	debug++;
    } else if (strncmp (arg, "vdebug", 6) == 0) {
	debug=3;
    } else if (strncmp (arg, "bandpass", 8) == 0) {
	bpass = vot_requiredArg (val);

    } else if (strncmp (arg, "cols", 4) == 0) {
	cols = vot_requiredArg (val);

    } else if (strncmp (arg, "delim", 5) == 0) {
	delim = vot_requiredArg (val);
	switch (delim[0]) {
	case 's':	delim = " ";  break;
	case 'c':	delim = ",";  break;
	case 't':	delim = "\t"; break;
	case 'b':	delim = "|";  break;
	}

    } else if (strncmp (arg, "extract", 7) == 0) {

        if (!val) {			    	/* just "--extract"	*/
           extract |= EX_ALL;

        } else {
           switch (*val) {
            case 'h':   extract |= EX_HTML;         break;  /* html         */
            case 'k':   extract |= EX_KML;          break;  /* KML          */
            case 'K':   extract |= EX_KML;
                        extract |= EX_COLLECT; 	    break;  /* one-file KML */
            case 'p':   extract |= EX_POS;          break;  /* positions    */
            case 'u':   extract |= EX_ACREF;        break;  /* urls         */
            case 'X':   extract |= EX_XML;
                        format   = (F_RAW | F_XML);
                        extract |= EX_COLLECT;      break;  /* one-file XML */
            default:    extract  = EX_BOTH;         break;  /* all          */
            }
	}

    } else if (strncmp (arg, "ecols", 5) == 0) {
	ecols = vot_requiredArg (val);

    } else if (strncmp (arg, "get", 3) == 0) {
	char *ip = vot_optionalArg (val);

	extract |= EX_ACREF;
	if (ip == NULL || !(isdigit(*ip)) ) {
            /* No option implies we get all rows.
            */
            fileRange.nvalues = RANGE_ALL;
            file_get = RANGE_ALL;
	} else {
	    /* Next arg is a range string.
	    */
	    strcpy (fileRange.rstring, ip);
	    if (vot_decodeRanges (fileRange.rstring,
		fileRange.ranges, MAX_RANGES, &fileRange.nvalues) < 0) {
		    fprintf (stderr, "Error decoding range string.\n");
	    }
	    file_get = fileRange.nvalues;

	    free ( (void *) ip);
	}

    } else if (strncmp (arg, "hskip", 5) == 0) {
	char *ip = vot_optionalArg (val);
	if (ip) {
	    table_hskip = vot_atoi ( ip );
	    free ( (void *) ip);
	}

    } else if (strncmp (arg, "kml", 3) == 0) {
	char *ip = vot_requiredArg (val);

        switch (arg[3]) {
        case 'm':                   /* max placemarks   --kmlmax=N 	*/
            kml_max = vot_atoi(val);
            break;
        case 'g':		    /* grouping		--kmlgroup=type */
            switch (*val) {
            case 'b':               /* group by both        */
                kml_byBoth = TRUE;
                kml_byObj = FALSE, kml_bySvc = FALSE;
                break;
            case 'o':               /* group by object      */
                kml_byObj = TRUE;
                kml_byBoth = FALSE, kml_bySvc = FALSE;
                break;
            case 's':               /* group by service     */
                kml_bySvc = TRUE;
                kml_byObj = FALSE, kml_byBoth = FALSE;
                break;
            }
            break;
        case 's':                   /* sample placemarks --kmlsample=N 	*/
            kml_sample = vot_atoi(ip);
            break;

        case 'n':                   /* sample placemarks --kmlno<opt> 	*/
            switch (arg[5]) {
            case 'l':                   /* disable labels       */
                kml_label = FALSE;
                break;
            case 'r':                   /* disable region box   */
                kml_region = FALSE;
                break;
            case 'v':                   /* disable verbose label*/
                kml_verbose = FALSE;
                break;
            }
        }

        if (strncmp (val, "kmlgroup", 8) == 0)
            extract |= EX_KML;
	if (ip) 
	    free ((void *) ip);

    } else if (strncmp (arg, "max", 3) == 0) {
	char *ip = vot_requiredArg (val);

        switch (arg[3]) {
        case 'd':			/* --maxdownloads=<N>	*/
            max_download = vot_atoi(ip);
            max_download = min(MAX_DOWNLOADS,max_download);
            break;
        case 'p':			/* --maxprocs=<N>	*/
            max_procs = vot_atoi(ip);
            max_procs = min(MAX_PROCS,max_procs);
            break;
        case 't':			/* --maxthreads=<N>	*/
            max_threads = vot_atoi(ip);
            max_threads = min(MAX_THREADS,max_threads);
            break;
        }

    } else if (strncmp (arg, "nlines", 6) == 0) {
	char *ip = vot_requiredArg (val);
	table_nlines = vot_atoi ( ip );
	if (ip) 
	    free ((void *) ip);

    } else if (strncmp (arg, "object", 6) == 0) {
	char *ip = vot_requiredArg (val);

        use_name++;
        if (!ip || ip[0] == '-') {
            if (rd_stdin) {
                fprintf (stderr, "ERROR: stdin can only be used once\n");
                exit (1);

            } else {
                rd_stdin++;
                fixed_obj++;
                vot_readObjFile ("-");
            }
        } else if (inventory) {
            vot_parseObjectList (ip, TRUE);
            sources = ip;
            if (debug)
                fprintf (stderr, "setting 'obj' sources = '%s'\n", sources);
            apos++;
        } else
            vot_parseObjectList (ip, TRUE);
        if (svcList)
            apos++;
	if (ip) 
	    free ((void *) ip);

    } else if (strncmp (arg, "output", 6) == 0) {
	char *ip = vot_requiredArg (arg);

        output = (ip ? ip : "=");
        if (output[0] == '-') {
            wr_stdout++;
            quiet++;                                
            if ((extract & EX_XML) || (extract & EX_KML))
                extract |= EX_COLLECT;

            memset (wrkdir, 0, SZ_FNAME);
            sprintf (wrkdir, "%s/vod%d", tmpdir, (int)getpid());
            if (access (wrkdir, R_OK|W_OK) != 0)
                mkdir (wrkdir, (mode_t)0666);
            chdir (wrkdir);
        }
	if (ip) 
	    free ((void *) ip);

    } else if (strncmp (arg, "pos", 3) == 0) {
	char posn[SZ_LINE];
	char *ip = vot_requiredArg (val);

        if (ip[0] == '-') {
            if (strlen (ip) > 1) {
                fprintf (stderr,"ERROR: the '--pos' flag requires ");
                fprintf (stderr,"coords or '-' for stdin\n");
                exit (1);
            } else if (rd_stdin) {
                fprintf (stderr, "ERROR: stdin can only be used once\n");
                exit (1);
                rd_stdin++, fixed_pos++;
            } else {
                vot_readObjFile ("-");
            }
        } else {
	    char v1[SZ_FNAME], v2[SZ_FNAME], *op;

	    op = strchr (val, (int)',');   /* find delimiter  	*/
	    memset (v1, 0, SZ_FNAME);	   /* clear arrays    	*/
	    memset (v2, 0, SZ_FNAME);
	    if (op) {
		*op = '\0';
		strcpy (v1, ip);	   /* first arg		*/
		strcpy (v2, op+1);	   /* second arg	*/
	    } else {
                fprintf (stderr, "ERROR: Invalid '--pos' argument\n");
                exit (1);
	    }

            if (isSexagesimal(v1) || isDecimal(v1)) {
                sprintf (posn, "%s %s", v1, v2);
                fixed_pos++;
                vot_parseObjectList (posn, TRUE);
            } else if (inventory) {
                vot_parseObjectList ((sources = ip), TRUE);
                apos++;
            } else
                vot_parseObjectList (ip, TRUE);
        }
	if (fixed_svc)
            apos++;
	if (ip) 
	    free ((void *) ip);

    } else if (strncmp (arg, "sample", 6) == 0) {
	table_sample = vot_atoi ( vot_requiredArg (arg) );

    } else if (strncmp (arg, "sr", 2) == 0) {
	char *ip = vot_requiredArg (val);
	int  len = strlen(ip);
	char units = ip[len-1];

	if (!isdigit(units))
	    ip[len-1] = '\0';
	else
	    units = 'd';
        switch (units) {
        case 's':  sr = vot_atof (ip) / 3600.; 	break;
        case 'm':  sr = vot_atof (ip) / 60.; 	break;
        case 'd':  sr = vot_atof (ip); 		break;
        }
	if (ip) 
	    free ((void *) ip);


    } else if (strncmp (arg, "svc", 3) == 0) {
	char *ip = vot_requiredArg (val);

        if (strncmp (ip, "http", 4) == 0)
            force_svc++;
        if (ip[0] == '-') {
            if (strlen (ip) > 1) {
                fprintf (stderr,"ERROR: the '-s' flag requires ");
                fprintf (stderr,"a service name or '-' for stdin\n");
                exit (1);
            } else
                vot_readSvcFile ("-", dalOnly);
        } else {
            vot_parseServiceList ((resources = ip), dalOnly);
            if (debug)
                fprintf (stderr, "setting resources = '%s'\n", resources);
            if (inventory)
                apos++;
	}
        fixed_svc++;
        apos++;
	if (fixed_pos)
            apos++;
	if (ip) 
	    free ((void *) ip);

    } else if (strncmp (arg, "type", 4) == 0) {
	typestr = vot_requiredArg (val);

    } else if (strncmp (arg, "verbose",  7) == 0) {
	verbose = 2;
    } else if (strncmp (arg, "vverbose",  8) == 0) {
	verbose = 3;

    } else if (strncmp (arg, "onefile",  7) == 0) {
	extract |= EX_COLLECT;			/* one-file output	*/

    } else if (strncmp (arg, "wb", 2) == 0 || 
	strncmp (arg, "webnoborder", 9) == 0) {
            html_border = FALSE;  		/* disable table border    */
    } else if (strncmp (arg, "webborder", 7) == 0) {
            html_border = TRUE;  		/* enable table border     */

    } else if (strncmp (arg, "wc", 2) == 0 ||
	strncmp (arg, "webnocolor", 9) == 0) {
            html_color = FALSE;   		/* disable border color    */
    } else if (strncmp (arg, "webcolor", 9) == 0) {
            html_color = TRUE;   		/* enable border color     */

    } else if (strncmp (arg, "wh", 2) == 0 ||
	strncmp (arg, "webnoheader", 9) == 0) {
            html_header = FALSE;   		/* disable table header    */
    } else if (strncmp (arg, "webheader", 7) == 0) {
            html_header = TRUE;   		/* enable table header     */


    } else if (strncmp (arg, "web", 3) == 0) {
	char *ip = vot_requiredArg (val);
        switch (ip[0]) {
        case 'b':   html_border = FALSE; break;  /* disable table border    */
        case 'c':   html_color  = FALSE; break;	 /* disable verbose label   */
        case 'h':   html_header = FALSE; break;	 /* disable region box	    */
	}
	if (ip) 
	    free ((void *) ip);
						/* DAL2 OPTION FLAGS	*/
    } else if (strncmp (arg, "band", 4) == 0) {
	d2_band = vot_requiredArg (val);
    } else if (strncmp (arg, "time", 4) == 0) {
	d2_time = vot_requiredArg (val);
    } else if (strncmp (arg, "format", 6) == 0) {
	d2_format = vot_requiredArg (val);
    } else if (strncmp (arg, "version", 7) == 0) {
	d2_version = vot_requiredArg (val);
    }

    return (ARG_DONE);
}


static char *
vot_requiredArg (char *arg)
{
    if (! arg) {
        fprintf (stderr, "ERROR: Missing '--%s' argument.\n", arg);
        exit (1);
    }

    return ( strdup (arg) );
}

static char *
vot_optionalArg (char *arg)
{
    return ( (arg ? strdup (arg) : NULL) );
}



/************************************************************************
**  PARSEARGTOKEN -- Parse the argument based on it's position on the 
**  command line.  Some actions will require the next argument in the 
**  command, otherwise the 'next' pointer will usually be NULL.  This 
**  routine can be used to parse either the argv commands or those from 
**  a file.  The 'inc' variable will indicate how far to advance the 
**  argument counter as a result of this procedure.
*/
static int
vot_parseArgToken (char *arg, char *next, int pos, int *inc)
{
    char posn[64];

    *inc = 0;

    /* Parse the arguments according to an assumed calling 
    ** order, i.e.
    **
    **	  vodata [ url_list | [ resource [obj | ra dec] [sr] ]
    **
    ** By default we assume size is in degrees, the ra/dec will
    ** be assumed to be ICRS J200 and may be sexagesimal or decimal.
    */
//fprintf (stderr, "parseArg:  '%s' '%s' %d apos=%d\n", arg, next, pos, apos);
    switch (apos) {
    case 0:				/* <resource> | <url>	*/

	if (arg[0] != '/' && (isSexagesimal (arg) || isDecimal (arg))) {
    	    fprintf (stderr,
	      "\nERROR: First argument required to be resource or url.\n");
    	    return (ERR);
	}

	if (strncmp (arg, "http", 4) == 0 && !force_svc && url_proc) {
	    /* If this is a simple URL, simply download the result. */
	    vot_addToAclist (arg, NULL);
	    if (!fixed_pos)
	        apos--;
	    url_proc++;

#ifdef VO_INVENTORY
	} else if (inventory) {
	    /* If we're doing an incentory call, the first arg is only
	    ** allowed to be either 'any', and 'ivorn', or (eventually) a
	    ** file of resources to be uploads.
	    */
	    if (strcasecmp ("any",arg) == 0 ||
		strncmp (arg, "ivo://", 6) == 0 ||
		access (arg, R_OK) == 0) {
		    if (strncmp (arg, "ivo://", 6) == 0) {
			extern char *id;
			id = arg;
		    }
		    if (access (arg, R_OK) == 0)
		        resources = arg;
                    vot_parseServiceList (arg, 0);

	    } else {
		fprintf (stderr, "Invalid resource type for Inventory, '%s'\n",
		    arg);
		exit (1);
	    }
#endif

	} else if (raw_vizier && strncmp ("ivo://CDS", arg, 9) == 0) {
	    char  url[SZ_LINE];
	    char *res = &arg[17];
	    char *base =
		"http://vizier.u-strasbg.fr/viz-bin/votable/-dtd/-A?-source=";

	    sprintf (url, "%s%s", base, res);

	    /* 
	    */
	    vot_parseServiceList (url, 0);
	    if (!fixed_pos)
	        apos--;
	    url_proc++;

	} else {
	    /*  Restrict 'any' searches to DAL only to save time.
	     */
	    if (strncasecmp ("any", arg, 3) == 0)
		dalOnly = 1;
	    vot_parseServiceList (arg, dalOnly);
	    if (fixed_pos)
	        apos++;
	}

	break;

    case 1:				/* <obj> | <pos>*/
	/* Sanity checks.  */
	if (force_svc && typestr && strchr(typestr,(int)',')) {
    	    fprintf (stderr,
	      "\nERROR: Only one type may be given to used-defined service.\n");
    	    return (ERR);

	} else if (force_svc && !typestr && !fixed_svc) {
    	    fprintf (stderr,
	      "\nERROR: No type specified for used-defined service.\n");
    	    return (ERR);

	} else if (!url_proc && !svcList && !inventory) {
	    fprintf (stderr,
		"\nERROR: No supported DAL service types found.\n");
	    return (ERR);
	}

	/* Parse the object or position.  */
	if (fixed_pos) {
	    *inc = 1;
	} else if (isSexagesimal (arg) || isDecimal (arg)) {
	    memset (posn, 0, 64);
	    sprintf (posn, "%s %s", arg, next);
	    vot_parseObjectList (posn, TRUE);
	    *inc = 1;
	} else {
	    use_name++;
	    if (inventory && access (arg, R_OK) == 0)
		sources = arg;
	    vot_parseObjectList (arg, TRUE);
	}
	break;

    case 2:				/* <size> (degrees)	*/
	sr = vot_atof (arg);
	break;

    default:
	fprintf (stderr, "Warning: Unknown argument syntax\n");
	vot_printUsage ();
	return (ERR);
    }

    return (OK);
}


/************************************************************************
**  GETNEXTCMDLINE -- Process the next command line from the argument 
**  file.  We do this by reading a line of the file and tokenizing it
**  as if the values were given to us on the command-line.  For the 
**  moment we don't allow options to be set and permit only the positional
**  arguments (e.g. resource, object, coords).
*/
static int
vot_getNextCmdline ()
{
    int   i, inc;
    char  *tok, *sep = " ", *line, cmdline[SZ_LINE];
    int   a_argc = 0;
    char  a_argv[4][SZ_LINE];

    extern char *vot_getline (FILE *fd);


    memset (a_argv, 0, (4 * SZ_LINE));
    memset (cmdline, 0, SZ_LINE);

    if (( line =  vot_getline (arg_fd)))
        strcpy (cmdline, line);
    else
        return (ERR);


    for (tok = strtok(cmdline, sep); tok; tok = strtok(NULL, sep)) {
	strcpy (a_argv[a_argc++], tok);
    }

    if (voc_initVOClient ("runid=voc.vodata") == ERR) 
        exit (-1);

    apos = sv_apos;
    for (i=0; i < a_argc; i++) {
        if (vot_parseArgToken (a_argv[i], a_argv[i+1], apos, &inc) != OK) {
    	    exit (1);
        } else 
            i += inc;		/* add the argv[] increment  	*/

        apos++;			/* update arg position		*/
    }


    /* Close the VOClient connection since each child will use its own.
    */
    voc_closeVOClient (0);			/* close VOClient connection */

    return (OK);
}


/************************************************************************
**  VALIDATEOPTIONS -- Verify that all the commandline options make sense 
**  before we begin processing.  Some options will require others and we 
**  can only check after all the options have been set and parsed from 
**  the command line.
*/
static int
vot_validateOptions ()
{
    if (count)
	fileRange.nvalues = RANGE_NONE;

    if (nobjects < 1) {
	if (apos && !meta && !all_data) { 
	    /* User provided an object, but it was invalid.
	    */
	    fprintf (stderr, "Error: No valid position or object found.\n");
	    return (ERR);

	} else {
	    /* No position/object supplied, use default.
	    */
	    if (!quiet && !meta) 
	        fprintf (stderr, "# Using default position: 0.0 0.0\n");
	    vot_parseObjectList ("0.0 0.0", TRUE);
    	    nobjects = vot_countObjectList();
	}
    }

    if (output && output[0] == '-')
	extract |= EX_COLLECT;

    if (kml_sample)
	kml_max *= kml_sample;

    if (meta && nservices == 0) {
	fprintf (stderr, "ERROR: No service specified.\n");
	return (ERR);
    } else if (meta && nservices == 1) {
	output = "-";
	wr_stdout++;
	quiet++;				
    }

    return (OK);
}



/************************************************************************
**  PRINTSVCHDR -- Print a header for service output.
*/
void
vot_printSvcHdr ()
{
    if (!quiet) {
	printf ("\n");
        if (meta) {
	    if (nservices == 1)
	        fprintf (stderr, "# Service:  %s\n", svcList->name);
	    fprintf (stderr, "# No. of Services: %d\n", nservices);
	} else {
	    if (nservices == 1) {
	        fprintf (stderr, "# Service:  %s\n", svcList->name);
	        fprintf (stderr, "# Title:  %s\n", svcList->title);
	    }
	    fprintf (stderr, "# No. of Objects:  %d\n", nobjects);
	    fprintf (stderr, "# No. of Services: %d\n", nservices);
	}
	fprintf (stderr, "# Search size:  %f (degrees)\n#\n", sr);
        if (count || inventory) 
	    vot_printCountHdr ();
    }

    fflush (stdout);
}


/************************************************************************
**  RUNSVCTHREADS --  Begin a processing thread for each data service.  We
**  split the object list over each service in parallel threads.
*/
static void
vot_runSvcThreads ()
{
    int     t, tc, rc, status, t_start, t_end, nthreads;
/*
    pthread_t  thread[nservices];
*/
    static pthread_t  thread[10000];
    Service *svc = svcList;
    Proc    *new = (Proc *)NULL;
    Proc    *cur = (Proc *)NULL;
    pthread_attr_t  attr;		/* thread attributes		*/


    qs_time = time ((time_t) NULL);

    if (verbose && !count && !meta && nservices > 1)
	fprintf (stderr, "# Creating service processing threads...\n");

    /* Pre-allocate the process lists so they're in the global memory
    ** space.
    */
    for (svc=svcList; svc; svc=svc->next) {
	for (t=0; t < nobjects; t++) {
	    new = (Proc *) calloc (1, sizeof (Proc));
	    new->svc = (Service *) svc;		/* set back pointer	*/
	    if (t == 0) {
		svc->proc = new;
		cur = svc->proc;
	    } else {
		cur->next = new;
		cur = cur->next;
	    }
	}
    }

    nthreads = min (nservices, max_threads);
    t_start = 0;
    t_end   = nthreads;


    /* Spawn the processing threads.
    */
    svc = svcList;
    for (tc=0; tc < nservices; tc += nthreads) {
        t_end   = min((nservices-tc),nthreads);

        /* Initialize the service processing thread attributes and run 'em.
        */
        pthread_attr_init (&attr);
        pthread_attr_setdetachstate (&attr, PTHREAD_CREATE_JOINABLE);

        for (t=0; t < t_end; t++, svc=svc->next) {
	    if ((rc = pthread_create (&thread[t], &attr, vot_procObjs, 
		(void *)svc))) {
	            fprintf (stderr,
			"ERROR: pthread_create() fails, code: %d\n", rc);
	            exit (-1);
	    }
        }

        /* Free attribute and wait for the threads to complete.
        */
        for (t=0; t < t_end; t++) {
	    if ((rc = pthread_join (thread[t], (void **)&status)) ) {
		/*
	        fprintf (stderr, "ERROR: pthread_join() fails, code: %d\n", rc);
	        exit (-1);
		*/
		;
	    }
        }

        pthread_attr_destroy (&attr);
    }
    qe_time = time ((time_t) NULL);

    if ((debug && verbose > 1)) {
	fprintf (stderr, "\n\n..........THREAD PROCS COMPLETED.....\n");
	vot_printSvcList (svcList);
	fprintf (stderr, "..........THREAD PROCS COMPLETED.....\n\n\n");
    }


    /*  If we're only writing a KML file and not simply extracting them
    **  as an extra, concatenate the results from each query to a single
    **  file for easier browsing.
    */
    if (format & F_KML || (extract & EX_KML && extract & EX_COLLECT)) {
 	char  fname[SZ_FNAME];
	extern void vot_concatKML (char *fname);

	memset (fname, 0, SZ_FNAME);
	if (output && output[0] != '-')
	    sprintf (fname, "Query_%d.kml", (int)getpid());
	else
	    strcpy (fname, "-");
	if (nservices > 1 && nobjects > 1)
	    vot_concatKML (fname);	

    } else if (format & F_XML || (extract & EX_XML && extract & EX_COLLECT)) {
 	    char  fname[SZ_FNAME];
	    extern void vot_concatXML (char *fname);

	    memset (fname, 0, SZ_FNAME);
	    if (output && output[0] != '-')
	        sprintf (fname, "Query_%d.xml", (int)getpid());
	    else
	        strcpy (fname, "-");

	    if (nservices > 1 && nobjects > 1)
	        vot_concatXML (fname);	

    } else if (extract & EX_COLLECT) {
	extern void vot_concat (void);

	vot_concat ();			/* concatenate results		*/
    }


    /* For verbose output, print a summary of the processing history.
    */
    if (!quiet && count && !meta) {
        Proc *curproc  = (Proc *) NULL;
        Proc *proc  = (Proc *) NULL;
	char *pad, *lpad;

	int  tot_rec   = 0;		/* Total records found		*/
	int  tot_fail  = 0;		/* No. failed service calls	*/
	int  tot_nodata= 0;		/* No. of no-data results	*/
	int  tot_query = 0;		/* No. of queries		*/

	tot_query = (nservices * nobjects);/* Total No. queries made	*/

	for (svc=svcList; svc; svc=svc->next) {
	    tot_rec  += (svc->count > 0 ? svc->count : 0);
	    tot_fail += svc->nfailed;
	    tot_nodata += svc->nnodata;
	}

	pad = (nobjects == 1) ? "\t\t\t" : "\t\t\t\t\t";
	lpad = (nobjects == 1) ? "---------------" : "";
	printf ("\n");
	printf ("#%s-------------------------------------%s\n", pad, lpad);
/*
	printf ("#%s%4d    (Records Found)\n", pad, tot_rec);
	printf ("#%s%4d    (Resources Queried)\n", pad, tot_query);
	printf ("#%s%4d    (Failed Requests)\n", pad, tot_fail);
	printf ("#%s%4d    (Completed Requests)\n", pad, (tot_query-tot_fail));
	printf ("#%s\t  (%d Results w/ Data)\n", pad,
	    (tot_query - tot_fail - tot_nodata));
*/
	fprintf (stderr, "#%s%4d    (Records Found)\n", pad, tot_rec);
	fprintf (stderr, "#%s%4d    (Resources Queried)\n", pad, tot_query);
	fprintf (stderr, "#%s%4d    (Failed Requests)\n", pad, tot_fail);
	fprintf (stderr, "#%s%4d    (Completed Requests)\n", pad, 
	    (tot_query - tot_fail));
	fprintf (stderr, "#%s\t  (%d Results w/ Data)\n", pad,
	    (tot_query - tot_fail - tot_nodata));
	if (tot_nodata)
	    printf ("#%s\t  (%d Results w/ No Data)\n", pad, tot_nodata);
	printf ("#\n");

	if (verbose < 3)
	    return;

        svc  = svcList;
        for (t=0; t < nservices; t++, svc=svc->next) {
	    if (svc->nfailed || !FAILED_ONLY)
	        vot_printProcStat (svc->proc, svc->name, FAILED_ONLY);

            /* Free the proctable structure.
            */
            for (curproc=svc->proc; curproc; curproc=proc) {
	        proc = curproc->next;
                if (curproc) 
		    free ((void *) curproc);
            }
        }
    }
}


/************************************************************************
**  PRINTSVCLIST -- Debug routine to print the SvcList.
*/
void
vot_printSvcList (Service *sl)
{
    Service *s;
    Proc    *p;

    for (s=sl; s; s=s->next) {
	fprintf (stderr, "%15s:  nfail=%d  nrefs=%d  type=%d\ts=%ld\n",
	    s->name, s->nfailed, s->nrefs, s->type, (long)s);

        for (p=s->proc; p; p=p->next) {
	    fprintf (stderr, "\t\tpid=%d  status=%d  obj=%s\t    p=%ld\n",
		p->pid, p->status, p->obj->name, (long)p);
        }
    }
}


/************************************************************************
**  PROCOBJS --  Create threads to process the object list.  Our only 
**  argument is the Service object to run.
*/
void *
vot_procObjs (void *arg)
{
    int    nobj, nprocs, nrunning, nremaining, nupdate;
    int    lock,  nrep, status;
    pid_t  r_pid, pid;

    Object *obj    = objList;
    Service *svc   = (Service *)arg;
    Proc *curproc  = (Proc *) svc->proc;
    svcParams  pars;

		
    /* Figure out how many threads to run at a time.
    */
    nprocs = ( (nobjects > max_threads) ?  max_threads : nobjects );
    nupdate  = 10;

    nrep = nrunning = 0;
    nremaining = nobjects;

    for (nobj=1; nremaining > 0; ) {

	if (debug)
	    fprintf (stderr, "procObjs(%s): n=%d/%d nprocs=%d nrun=%d sr=%f\n",
    	  	svc->name, nobj, nobjects, nprocs, nrunning, sr);

        /* Spawn a process thread for each object/position.
        */
	if (nrunning < nprocs && nobj <= nobjects) {

	    /* Set up the service parameter struct.  Each thread gets its
	    ** own instance.
	    */
	    strcpy (pars.service_url, vizPatch(svc->service_url));
	    strcpy (pars.identifier, svc->identifier);
	    strcpy (pars.name, svc->name);
	    if (id_col && obj->id && obj->id[0])
	        strcpy (pars.oname, obj->id);
	    else
	        strcpy (pars.oname, obj->name);
	    strcpy (pars.title, svc->title);
	    pars.ra    = obj->ra;
	    pars.dec   = obj->dec;

	    /*  Prior to Registry 1.0 we didn't have a real cone capability
	    **  for Vizier tables and needed to set flags to download the
	    **  entire table.  This is no longer necessary, the user can set
	    **  a negative search radius to get the entire table if they choose.

	    pars.sr    = sr;
	    */
	    if (all_data && svc->type == SVC_VIZIER)
	 	pars.sr = -1.0;
	    else
		pars.sr = sr;
	    pars.fmt   = format;
	    pars.type  = svc->type;
	    pars.index = nobj;
	    pars.obj_index = nobj - 1;		/* zero-indexed		*/
	    pars.svc_index = svc->index;

	    if ((pid = (*(PFI)(*svc->func))((void *)&pars)) < 0) {
	        fprintf (stderr,"ERROR: process fork() fails\n");
	        pthread_exit ((void *) NULL);
	    }
	    nrunning++;
	    nobj++;

            /* Allocate the process summary struct.  Only save the status if
	    ** we'll be printing a summary later.
            */
    	    if ((extract & EX_COLLECT) || !quiet) {

		/* Lock the thread to protect us from messing with the
		** service list data.
		*/
	        lock = pthread_mutex_lock (&svc_mutex);
	        curproc->pid = pid;	    /* load the process struct	*/
	        curproc->obj = obj;
	        curproc->status = 0;
    		memset (curproc->root, 0, SZ_FNAME);
		if (use_name || all_named || id_col)
		    strcpy (curproc->root, vot_getOFName(&pars,NULL,(int)pid));
                else
		    strcpy (curproc->root, vot_getOFIndex(&pars,NULL,(int)pid));


	        curproc = curproc->next;    /* move on			*/
		lock = pthread_mutex_unlock (&svc_mutex);
	    }

	    if (debug)
		fprintf (stderr, "procObjs(%s): %d ra=%f dec=%f pid=%d\n",
    		    svc->name, nobj, obj->ra, obj->dec, pid);

	    if (obj) 
		obj = obj->next;
		
        } else {

            /* Process table full, wait for any child processes to complete.
            */
	    if (debug)
	        fprintf (stderr, "procObjs(%s:%d): waiting %d remaining....\n", 
    		    svc->name, getpid(), nremaining);

	    if ((r_pid = waitpid ((pid_t)-1, &status, (int) 0)) < 0 ) {
		/*  FIXME
		if (verbose || debug) 
	            fprintf (stderr, "ERROR: waitpid() fails, code[%d] %s\n",
		        (int)errno, strerror(errno));
	        pthread_exit ((void *) NULL);
		*/
	    }

	    status = WEXITSTATUS(status);
	    if (debug)
		fprintf (stderr, "pid = %d  stat = %d\n", r_pid, status);
		
	    lock = pthread_mutex_lock (&svc_mutex);
	    vot_setProcStat (svc, (int)r_pid, status);
	    lock = pthread_mutex_unlock (&svc_mutex);

	    nrunning--;
	    nremaining--;
        }

	if (!quiet && nobj < nobjects && (nobj % nupdate) == 0 && 
	    !count && !file_get && nrep == 0) {
	      fprintf (stderr,
		"# Service %15s: Completed %3d of %4d objects (%d running)\n",
    	        svc->name, nobj, nobjects, nrunning);
	    nrep = 1;
	} else if (nrep == 1) 
	    nrep = 0;
    }

    if (!quiet && !count && !file_get && !meta) {
	fprintf (stderr, "# Service %25s: ", svc->name);
	fprintf (stderr, "Finished processing (%d of %d succeeded).\n",
	    (nobjects - (svc->nfailed + svc->nnodata)), 
	    nobjects);
    }
    if (debug)  
	fprintf (stderr, "procObjs done (%s).\n", svc->name);


    pthread_exit (NULL);
}


static char *
vizPatch (char *url)
{
    static char new[SZ_LINE], *ip, *op;

    memset (new, 0, SZ_LINE);
    for (ip=url, op=new; *ip; ) {
	if (*ip == '&' && *(ip+1) == '/')
	   ip++;
	*op++ = *ip++;
    }

    return (new);
}


/************************************************************************
**  SETPROCSTAT -- Set the process return status.  'procList' is the 
**  process list running on this particular thread.
*/
static void
vot_setProcStat (Service *svc, int pid, int status)
{
    Service *s = svcList;
    Proc *pp;
    int    i, rc, sem_id, n = nservices;

    if (svcList == (Service *) NULL)
	return;

    for (s=svcList; s; s=s->next, n--) {
        for (pp=s->proc; pp; pp=pp->next) {
	    if (pp->pid == pid) {
		/* Set the status for this svc/obj process.
		*/
	        pp->status = status;
        	if (status == E_REQFAIL)
            	    s->nfailed++;
        	if (status == E_NODATA)
            	    s->nnodata++;

		/* If we created a URL extraction, add the URLS to the 
		** access list.
		*/
		if (extract == EX_ACREF || extract == EX_BOTH) {
		    char  fname[SZ_FNAME], url[SZ_URL];
		    FILE  *fd;
		    int   nf = 1;

		    /* Get the filename of the URLs we'll get.
		    */
		    memset (fname, 0, SZ_FNAME);
		    sprintf (fname, "%s.urls", pp->root);

		    /* Construct a template for each file.
		    */
		    if (access (fname, R_OK) == 0) {
			fd = fopen (fname, "r");
			for (i=1; fgets (url, SZ_URL, fd); i++) {
			    url[strlen(url)-1] = '\0';  /* kill newline   */
		    
			    memset (fname, 0, SZ_FNAME);
			    if (file_get > 1)
			        sprintf (fname, "%s.%03d", pp->root, i);
			    else
			        sprintf (fname, "%s", pp->root);

			    if (file_get && is_in_range(fileRange.ranges,i)) {
			        vot_addToAclist (url, fname);
			        nf++;
			    }
			}
			fclose (fd);
		    }
		}

		/* Get the semaphore set by the child indicating the
		** result count.
		*/
		sem_id = semget (pid, 0, 0);
		pp->count = semctl (sem_id, 0, GETVAL, 0);  /* get value  */
		s->count += pp->count;
    		rc = semctl (sem_id, 0, IPC_RMID, NULL);    /* release it */
        	if (status != E_NODATA && s->count == 0)
            	    s->nnodata++;

	        break;
	    }
        }
    }
    if (s == (Service *) NULL && n) {	/* no matching service found */
	fprintf (stderr, "Warning: NO SERVICE FOUND....pid=%d  status=%d\n",
	    pid, status);
	return;
    }
}



/************************************************************************
**  PRINTPROCSTAT -- Print a summary of the processing results.  If 
**  'failed_only' is set this is an error summary only.
*/
static void
vot_printProcStat (Proc *procList, char *svc_name, int failed_only)
{
    Proc *pp = procList;
    FILE *fd = (failed_only ? stderr : stdout);
    
    if (procList == (Proc *) NULL)
	return;

    fprintf (fd, "\nError Summary for '%s' :\n", svc_name);
    while (pp) {
	if (!failed_only || pp->status) {
	    fprintf (fd, "  Pid %6d: Source: %-12.12s (%.6f,%.6f)  %s\n",
		pp->pid,
		(pp->obj->name ? pp->obj->name : "(none)"), 
		pp->obj->ra, pp->obj->dec, vot_stat2code(pp->status));
	}
	pp = pp->next;
    }
}


/************************************************************************
**  STAT2CODE -- Convert an error code to a text string for printing.  
*/
static char *
vot_stat2code (int status)
{
    switch (status) {
    case E_NONE:	return ("OK");				break;
    case E_NODATA:	return ("No Data Returned");		break;
    case E_REQFAIL:	return ("Request Failed");		break;
    case E_FILOPEN:	return ("File Open Error");		break;
    case E_VOCINIT:	return ("VOClient init fails");		break;
    default:		return ("Unknown Error");		break;
    }

    return (NULL);
}


/************************************************************************
**  CNVTYPE -- Convert an error code to a text string for printing.  
static char *
vot_cnvType (char *intype)
{
    if (strcasecmp ("image", intype) == 0)
	return ("siap");
    else if (strcasecmp ("catalog", intype) == 0)
	return ("cone");
    else if (strcasecmp ("table", intype) == 0)
	return ("tabularskyservice");
    else 
	return (intype);
}
*/


/************************************************************************
**  PRINTPROCTIME -- Print a summary of the processing times.
*/
static void
vot_printProcTime ()
{
    if (!quiet) {
        int  r = ((int)re_time - (int)rs_time),
	     q = ((int)qe_time - (int)qs_time),
	     a = ((int)ae_time - (int)as_time);
        int  tot = (r + q + a);

        extern  char *toSexaTime (int n);


        printf ("#\n");
        printf ( "# Approx Time:  %02d:%02d:%02d\t",
	    (tot / 3600), (tot / 60), (tot % 60)); 
        printf ("(%02d:%02d Resolution, %02d:%02d Query, %02d:%02d Access)\n",
	    r / 60,  r % 60, 
	    q / 60,  q % 60, 
	    a / 60,  a % 60);
    }
}


/************************************************************************
**  USAGE --  Print a summary of the help options.
*/
static void
Usage()
{
  vot_printUsage();

  printf ("Where:\n");
  printf ("\n");
  printf ("    <resource>       ShortName/Identifier of data service \n");
  printf ("    <obj>            Name of object to be resolved\n");
  printf ("    <ra> <dec>       Decimal or Sexagesimal coords for query \n");
  printf ("    <url>            URL to access or ServiceURL to query \n");
  printf ("    <sr>             Search radius in degrees (def: 0.1) \n");
  printf ("\n");
  printf ("    These values may optionally be in a file containing them.\n");
  printf ("    Resource and object names may be comma-delimited lists\n");
  printf ("\n");
  printf ("\n");
  printf ("    -h               Print this help summary\n");
  printf ("    -v, --vverbose   Verbose or very-verbose mode\n");
  printf ("\n\tTask Behavior Flags:\n");
  printf ("    -a, --all        Query all data for the resource\n");
  printf ("    -c, --count      Print a count\n");
  printf ("    -g, --get <rng>  Get the files associated with a query\n");
  printf ("    -m, --meta       Print the column metadata for the resource\n");

  printf ("\n\tQuery Options:\n");
  printf ("    -b <bandpass>    Constrain by bandpass\n");
  printf ("    -i <file>        Take arguments from file (or stdin)\n");
  printf ("    -o <obj>         Specify object list\n");
  printf ("    -p <pos>         Specify position list\n");
  printf ("    -r               Set search radius\n");
  printf ("      -rs              Set radius in arc-seconds\n");
  printf ("      -rm              Set radius in arc-minutes\n");
  printf ("      -rd              Set radius in degrees (default: 0.1)\n");
  printf ("    -s <svc>         Specify the service name or url\n");
  printf ("    -t <type>        Constrain by service type\n");

  printf ("\n\tOutput Options:\n");
  printf ("    -A,--ascii       ASCII table output (.txt extension)\n");
  printf ("    -C,--csv         CSV table output (.csv extension)\n");
  printf ("    -H,--html        HTML table output (.html extension)\n");
#ifdef VO_INVENTORY
  printf ("    -I               Output results from the Inventory Service\n");
#endif
  printf ("    -K,--kml         KML output (.kml extension)\n");
  printf ("    -R,--raw         Raw (VOTable) output (.xml extension)\n");
  printf ("    -V,--votable     VOTable output (.xml extension)\n");
  printf ("    -T,--tsv         Tab-separated table output (.tsv extension)\n");
  printf ("    -O <root>	Set the root part of output name\n");
  printf ("\n");
  printf ("    -e,--extract     Extract results to separate file\n");
  printf ("      --ep             Extract only positions (to <file>.pos)\n");
  printf ("      --eu             Extract only access urls (to <file>.urls)\n");
  printf ("      --eh             Extract table to HTML (to <file>.html)\n");
  printf ("      --ek             Extract table to individual KML (to <file>.kml)\n");
  printf ("      --eK             Extract table to aggregate KML (to <file>.kml)\n");
  printf ("    -n,--nosave      No-save results, print a count only\n");
  printf ("    -q,--quiet       Suppress output to the screen\n");

  printf ("\n\tFormat-specific Options:\n");
  printf ("    --km <N>         Set max downloads (def: 100)\n");
  printf ("    --kgb            Group KML output by both object and service\n");
  printf ("    --kgo            Group KML output by object\n");
  printf ("    --kgs            Group KML output by service\n");
  printf ("    --ks <N>         Set result sample\n");
  printf ("    --kl             Disable Placemark labels\n");
  printf ("    --kr             Disable region box\n");
  printf ("    --kv             Disable verbose labels\n");
  printf ("\n");
  printf ("    --wb             Disable HTML table borders\n");
  printf ("    --wc             Disable HTML table verbose label\n");
  printf ("    --wh             Disable HTML page header\n");

  printf ("\n\tProcessing Options:\n");
  printf ("    --md <N>         Set max downloads (def: 1)\n");
  printf ("    --mp <N>         Set max number of processes per obj query\n");
  printf ("    --mt <N>         Set max number of resource threads to run\n");
  printf ("    \n");

  printf ("\n    Notes:\n");
  printf ("\t- Data Services may be listed by Registry ShortName of ");
  printf ("Identifier\n");
  printf ("\t- Sources may be object name, positions in decimal or sexages");
  printf ("imal\n\t  coords, or a file containing either.\n");

  printf ("\n\n");
  printf ("Resource Type Strings:\n");
  printf ("    catalog           Cone search services\n");
  printf ("    image             Simple Image Access services\n");
  printf ("    spectra           Simple Spectral Access services\n");
  printf ("    table             Vizier services\n");
  printf ("    <literal>         ResourceType from registry record\n");
  printf ("\n");
  printf ("Allowed Bandpass Strings:\n");
  printf ("    Radio             Millimeter           Infrared (IR)\n");
  printf ("    Optical           Ultraviolet (UV)     X-Ray (xray)\n");
  printf ("    Gamma-Ray (GR)\n");
  printf ("\n");

  printf ("\n\n");
  vot_printExamples();
  printf ("\n\n");
}


static void
vot_printExamples()
{

  printf ("Examples:\n---------\n\n");


  printf ("\
  1) Query the GSC 2.3 catalog for stars a) within the 0.1 degree \n\
     default search size around NGC 1234:  b) around all positions \n\
     contained in file 'pos.txt':  c) for the list of objects given \n\
     on the command line:  d) query a list of services for a list \n\
     of positions: e) print a count of results that would be returned\n\
     from 3 services for each position in a file:\n\
\n\
        %% vodata gsc2.3 ngc1234                 (a)\n\
        %% vodata gsc2.3 pos.txt                 (b)\n\
        %% vodata gsc2.3 m31,m51,m93             (c)\n\
        %% vodata svcs.txt pos.txt               (d)\n\
        %% vodata hst,chandra,gsc2.3 pos.txt     (e)\n\
\n\
  2) Find all images by HST of NGC 4258, create a KML file for Google Sky:\n\
\n\
        %% vodata -K -t image -all hst ngc4258\n\
\n\
  3) Query all (142) image services having data of the subdwarf\n\
     galaxy IC 10, print a count of the results only:\n\
\n\
        %% vodata -c -t image any IC10\n\
\n\
  4) Print a count of X-ray catalog data around Abell2712:\n\
\n\
        %% vodata -count -t catalog -b x-ray any abell2712\n\
\n\
  5) Print the column metadata returned by the RC3 catalog service:\n\
\n\
        %% vodata -meta -t catalog rc3\n\
\n\
  6) Use the Registry to query for resources using the search terms\n\
     'cooling flow'.  Upon examining the output the user notices a\n\
     Vizier paper titled 'Cooling Flows in 207 clusters of Galaxies'\n\
     that looks interesting.  Use the vodata task to download all\n\
     tables associated with this paper, save tables in the default\n\
     CSV format:\n\
\n\
        %% voregistry cooling flow\n\
        %% vodata -O white97 -all J/MNRAS/292/419\n\n\n\
  ");

}


static void
vot_printUsage()
{
  printf ("\n");
  printf ("Usage:\n");
  printf ("\n");
  printf ("    vodata <flags> [[ <resource> [<obj> | <ra> <dec>] [<sr>] | [<url>]]\n");
  printf ("\n");
}



/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
    Task *task = &self;

    vo_taskTest (task, "--help", NULL);

    vo_taskTestFile ("m31\nm51\nm99\n", "pos.txt");
    vo_taskTestFile ("hst\nchandra\ngsc2.3\n", "svcs.txt");

    vo_taskTest (task, "gsc2.3", "ngc1234", NULL);
    vo_taskTest (task, "gsc2.3", "pos.txt", NULL);
    vo_taskTest (task, "gsc2.3", "m31,m51,m93", NULL);
    vo_taskTest (task, "svcs.txt", "pos.txt", NULL);
    vo_taskTest (task, "hst,chandra,gsc2.3", "pos.txt", NULL);

    vo_taskTest (task, "-c", "-t", "image", "any", "IC10", NULL);
    vo_taskTest (task, "--count", "--type=image", "any", "IC10", NULL);
    vo_taskTest (task, "-c", "-t", "catalog", "-b", "x-ray", 
	"any", "abell2712", NULL);
    vo_taskTest (task, "--count", "--type=image", "--bandpass=x-ray", 
	"any", "abell2712", NULL);

    vo_taskTest (task, "--meta", "rc3", NULL);

    vo_taskTest (task, "-O", "white97", "-all", "J/MNRAS/292/419", NULL);

    vo_taskTest (task, "-rv", "-t", "image", "xmm", NULL);
    vo_taskTest (task, "-cq", "xmm-newton", "3c273", NULL);
    vo_taskTest (task, "--count", "--quiet", "xmm-newton", "3c273", NULL);
    vo_taskTest (task, "--get", "xmm-newton", "3c273", NULL);

    vo_taskTest (task, "-e", "-O", "2mass", "-t", "image", "2mass", 
	"12:34:56.7", "-23:12:45.2", NULL);
    vo_taskTest (task, "-e", "--output=2mass", "--type=image", "2mass", 
	"12:34:56.7", "-23:12:45.2", NULL);

    vo_taskTest (task, "-e", "ivo://nasa.heasarc/abell", 
	"0.0", "0.0", "180.0", NULL);

    vo_taskTest (task, "-a", "galex", "m51", NULL);
    vo_taskTest (task, "--all", "galex", "m51", NULL);


    if (access ("pos.txt", F_OK) == 0)    unlink ("pos.txt");
    if (access ("svcs.txt", F_OK) == 0)   unlink ("svcs.txt");

    vo_taskTestReport (self);
}

