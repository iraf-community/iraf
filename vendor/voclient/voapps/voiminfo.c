/*
 *  VOIMINFO -- Get the WCS information for a FITS image.
 *
 *    Usage:
 *		voiminfo [<otps>] image.fits
 *
 *  @file       voiminfo.c
 *  @author     Mike Fitzpatrick
 *  @date       11/03/12
 *
 *  @brief      Get the WCS information for a FITS image.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "votParse.h"			/* keep these in order!		*/
#include "voApps.h"
#include "fitsio.h"



#define	MAX_IMAGES	20480		/* max images to process	*/
#define SZ_RESBUF	819200

#define	OPT_ALL		0001
#define	OPT_BOX		0002
#define	OPT_CORNERS	0004
#define	OPT_EXTNS	0010
#define	OPT_FRAME	0020
#define	OPT_NAXES	0040


static int  do_return   = 0;		/* return result?		*/

/*  Global task declarations.  These should all be defined as 'static' to
 *  avoid namespace collisions.
 */
static int do_all	= 0;		/* print global values		*/
static int do_sex	= 0;		/* print sexagesimal values	*/

static int do_box	= 0;		/* print box coordinates	*/
static int do_corners	= 0;		/* print image corners		*/
static int do_extns	= 0;		/* print extension values	*/
static int do_info	= 0;		/* print image info		*/
static int do_naxis	= 0;		/* print NAXIS values		*/

static int debug	= 0;		/* debug flag			*/
static int verbose	= 1;		/* verbose flag			*/


/*  Task specific option declarations.  Task options are declared using the
 *  getopt_long(3) syntax.
 */
int  voiminfo (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "voiminfo",  voiminfo,  0,  0,  0  };
static char  *opts 	= "%:habcdvnseio:r";
static struct option long_opts[] = {
        { "test",         1, 0,   '%'},		/* --test is std	*/
        { "help",         2, 0,   'h'},		/* --help is std	*/
        { "return",       2, 0,   'r'},		/* --return is std	*/
        { "debug",        2, 0,   'd'},		/* debug flag 		*/
        { "verbose",      2, 0,   'v'},		/* verbose flag 	*/

        { "all",          2, 0,   'a'},		/* print global value	*/
        { "box",     	  2, 0,   'b'},		/* print box values 	*/
        { "corners",      2, 0,   'c'},		/* print image corner 	*/
        { "extns",        2, 0,   'e'},		/* print each extn	*/
        { "info",         2, 0,   'i'},		/* print image info	*/
        { "naxes",     	  2, 0,   'n'},		/* print NEXIS values 	*/
        { "output",       1, 0,   'o'},		/* output filename	*/
        { "sex",          2, 0,   's'},		/* sexagesimal values	*/
        { NULL,           0, 0,    0 }
};


/**
 *  Private procedures.
 */
static void  Usage (void);
static void  Tests (char *input);
static char *fmt (double pos, int is_ra);
static char *fmt_naxis (char *imname, ImInfo *im, int do_all);
static char *fmt_box (char *imname, ImInfo *im, int do_all);
static char *fmt_corners (char *imname, ImInfo *im, int do_all);

extern int   vos_getURL (char *url, char *name);



/**
 *  Application entry point.
 */
int
voiminfo (int argc, char **argv, size_t *reslen, void **result)
{
    /*  These declarations are required for the VOApps param interface.
     */
    char **pargv, optval[SZ_FNAME], resbuf[SZ_RESBUF];
    char   imname[SZ_LINE];


    /*  These declarations are specific to the task.
     */
    char   *oname = NULL, *imlist[MAX_IMAGES], *nimlist[MAX_IMAGES];
    int     i, ch = 0, status = OK, pos = 0, nfiles = 0, narg = 0;;
    size_t  maxlen = SZ_RESBUF;
    FILE   *fd = (FILE *) NULL;
    ImInfo *im;


    /* Initialize result object	whether we return an object or not.
     */
    *reslen = 0;	
    *result = NULL;
    memset (imlist,  0, MAX_IMAGES);
    memset (nimlist, 0, MAX_IMAGES);


    /*  Parse the argument list.  The use of vo_paramInit() is required to
     *  rewrite the argv[] strings in a way vo_paramNext() can be used to
     *  parse them.  The programmatic interface allows "param=value" to
     *  be passed in, but the getopt_long() interface requires these to
     *  be written as "--param=value" so they are not confused with 
     *  positional parameters (i.e. any param w/out a leading '-').
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext(opts,long_opts,argc,pargv,optval,&pos)) != 0) {
        if (ch > 0) {
	    /*  If the 'ch' value is > 0 we are parsing a single letter
	     *  flag as defined in the 'opts string.
	     */
	    switch (ch) {
	    case '%':  Tests (optval);			return (self.nfail);
	    case 'h':  Usage ();			return (OK);
	    case 'r':  do_return=1;	    	    	break;
	    case 'd':  debug++;				break;
	    case 'v':  verbose++;			break;

	    case 'a':  do_all++;			break;
	    case 'b':  do_box++;			break;
	    case 'c':  do_corners++;			break;
	    case 'e':  do_extns++;			break;
	    case 'i':  do_info++;			break;
	    case 's':  do_sex++;			break;
	    case 'n':  do_naxis++;			break;
	    case 'o':  oname = strdup (optval);		break;
	    default:
		fprintf (stderr, "Invalid option '%s'\n", optval);
		return (1);
	    }

        } else if (ch == PARG_ERR) {
            return (ERR);

	} else {
	    /*  This code processes the positional arguments.  The 'optval'
	     *  string contains the value but since this string is
	     *  overwritten w/ each arch we need to make a copy (and must
	     *  remember to free it later.
	     */
	    imlist[nfiles++] = strdup (optval);
	    narg++;
	}

	if (narg > MAX_IMAGES) {
            fprintf (stderr, "ERROR: Too many images to process\n");
            return (1);
	}
    }


    /*  Sanity checks.
     */
    if (imlist[0] == NULL || strcmp (imlist[0], "-") == 0) { 
	free (imlist[0]);
	imlist[0] = strdup ("stdin");  
	nfiles = 1;
    }
    if (do_all && strcasecmp (imlist[0], "stdin") == 0) {
	fprintf (stderr, "Error: Option not supported with standard input\n");
	return (ERR);
    }
    if (oname == NULL) oname = strdup ("stdout");
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }


    /*  Open the output file.
     */
    if (strcmp (oname, "stdout") != 0) {
	if ((fd = fopen (oname, "w+")) == (FILE *) NULL) {
	    fprintf (stderr, "Error: Cannot open output file '%s'\n", oname);
	    return (ERR);
	}
    } else
	fd = stdout;

    /* Default to printing all info if we're not given a specific option.
    if (!do_box && !do_corners && !do_info && !do_naxis)
	do_info = 1;
     */


    /**
     *  Main body of task
     */
    for (i=0; i < nfiles; i++) {

	memset (imname, 0, SZ_LINE);
	if (strncmp ("http://", imlist[i], 7) == 0) {
	    if (access ("/tmp/voiminfo.fits", F_OK) == 0)
		unlink ("/tmp/voiminfo.fits");
	    strcpy (imname, "/tmp/voiminfo.fits");
	    if (vos_getURL (imlist[i], imname) <= 0)
		continue;
	} else
	    strcpy (imname, imlist[i]);

	if ((im = vot_imageInfo (imname, do_all)) ) {
    	    memset (resbuf,  0, SZ_RESBUF);
	    if (do_info) {
                vot_printImageInfo (fd, im);
	    } else if (do_naxis) {
		strcpy (resbuf, fmt_naxis (imname, im, do_all));
	    } else if (do_box) {
		strcpy (resbuf, fmt_box (imname, im, do_all));
	    } else if (do_corners) {
		strcpy (resbuf, fmt_corners (imname, im, do_all));
	    } else {
                sprintf (resbuf, "%s\t%s\t%s\t%s\n", imlist[i], 
		    fmt (im->frame.cx,1), fmt (im->frame.cy,0), 
		    fmt (im->frame.radius,0));
	    }

	    if (do_return)
		vo_appendResultFromString (resbuf, reslen, result, &maxlen);
	    else
                fprintf (fd, "%s", resbuf);

            vot_freeImageInfo (im);

	    if (strncmp ("http://", imlist[i], 7) == 0)
	        unlink (imname);
	}
    }


    /*  Clean up.  Rememebr to free whatever pointers were created when
     *  parsing arguments.
     */
    for (i=0; i < MAX_IMAGES; i++) {
        if (imlist[i])  
	    free (imlist[i]);
        if (nimlist[i]) 
	    free (nimlist[i]);
	else
	    break;
    }
    if (oname) free (oname);
    if (fd != stdout)
	fclose (fd);

    vo_paramFree (argc, pargv);

    return (status);	/* status must be OK or ERR (i.e. 0 or 1)     	*/
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "voiminfo [<opts>] votable.xml\n\n"
        "  where\n"
        "       -%%,--test		run unit tests\n"
        "       -h,--help		this message\n"
	"\n"
        "	-a,--all   		print all extns\n"
        "	-i,--info     		print image info\n"
        "	-b,--box     		print box values\n"
        "	-c,--corners   		print image corners\n"
        "	-n,--naxes     		print NAXIS values\n"
        "	-o,--output    		output filename\n"
        "	-s,--sex       		sexagesimal values\n"
	"\n"
        "       -o,--output=<file>	output file\n"
	"\n"
	"\n"
 	"  Examples:\n\n"
	"    1) Print the size of each image extension in an MEF file:\n\n"
	"	    %% voiminfo -n mef.fits\n"
	"\n"
	"    2) Print the LL and UR positions of an image as sexigesimal:\n\n"
	"	    %% voiminfo -b -s image.fits\n"
	"\n"
	"    3) Print the corner positions of an image as decimal degrees:\n\n"
	"	    %% voiminfo -c image.fits\n"
	"\n"
	"    4) Print the box values for an entire mosaic MEF file:\n\n"
	"	    %% voiminfo -b mef.fits\n"
	"\n"
    );
}


/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
    Task  *task = &self;
    char  *mef = "data/mef.fits";
    char  *sif = "data/sif.fits";
    char  *opts[] = { "-a", "-i", "-b", "-c", "-n", "-s", NULL };
    register int i;


    vo_taskTest (task, "--help", NULL);

    if (access (input, F_OK) == 0) {
        vo_taskTest (task, "-n", input, NULL);				// Ex 1
        vo_taskTest (task, "-b", "-s", input, NULL);			// Ex 2
        vo_taskTest (task, "-c", input, NULL);				// Ex 3
        vo_taskTest (task, "-b", input, NULL);				// Ex 4
    } else 
	fprintf (stderr, "Warning: input file '%s' not found...\n", input);

    if (access (sif, F_OK) == 0)
	for (i=0; opts[i]; i++)
	    vo_taskTest (task, opts[i], sif, NULL);

    if (access (mef, F_OK) == 0)
	for (i=0; opts[i]; i++)
	    vo_taskTest (task, opts[i], mef, NULL);

    vo_taskTestReport (self);
}



/***************************************************************************/
/****  			    Private Procedures				****/
/***************************************************************************/

/**
 *  FMT -- Output formatter.
 */
static char *
fmt (double pos, int is_ra)
{
    static char strbuf[10][SZ_LINE];
    static int  bufnum = 0;


    bufnum = (bufnum + 1) % 10;
    memset (strbuf[bufnum], 0, SZ_LINE);

    pos = (pos < 0.0 ? -pos : pos);

    if (do_sex) {
        int   d, m;
        float s, frac;
        char  sign = (pos < 0.0 ? '-' : ' ');

        pos = (is_ra ? (pos / 15.0) : pos);
        d = (int) pos;
        frac = (pos - d);
        m = frac * 60.0;
        s = ((frac * 60.0) - m) * 60.0;

        sprintf (strbuf[bufnum], "%c%02d:%02d:%04.1f", sign, d, m, s);

    } else
        sprintf (strbuf[bufnum], "%f", pos);

    return (strbuf[bufnum]);
}


/**
 *  FMT_NAXIS -- Print the size of pixels of an image.
 */
static char *
fmt_naxis (char *imname, ImInfo *im, int do_all)
{
    static char line[SZ_RESBUF];

    memset (line, 0, SZ_RESBUF);
    if (im->nextend && !do_all) {
	register int i = 0;
	char buf[SZ_RESBUF];

	for (i=0; i < im->nextend; i++) {
	    memset (buf, 0, SZ_RESBUF);
            if (im->frame.naxis == 3)
                sprintf (buf, "%s[%d]\t%d %d %d\n", imname, i,
	            im->extns[i].naxes[0], 
		    im->extns[i].naxes[1],
		    im->extns[i].naxes[2]);
            else
                sprintf (buf, "%s[%d]\t%d %d\n", imname, i,
	            im->extns[i].naxes[0], im->extns[i].naxes[1]);
	    strcat (line, buf);
	}

    } else {
        if (im->frame.naxis == 3)
            sprintf (line, "%s\t%d %d %d\n", imname,
	        im->frame.naxes[0], im->frame.naxes[1], im->frame.naxes[2]);
        else
            sprintf (line, "%s\t%d %d\n", imname,
	        im->frame.naxes[0], im->frame.naxes[1]);
    }

    return (line);
}


/**
 *  FMT_BOX -- Print the corners of a box and rotation angle.
 */
static char *
fmt_box (char *imname, ImInfo *im, int do_all)
{
    static char line[SZ_RESBUF];

    memset (line, 0, SZ_RESBUF);
    if (im->nextend && !do_all) {
	register int i = 0;
	char buf[SZ_RESBUF];

	for (i=0; i < im->nextend; i++) {
	    memset (buf, 0, SZ_RESBUF);
            sprintf (buf, "%s[%d]\t%s %s\t%s %s\t%s\n", imname, i,
	        fmt (im->extns[i].lx,1), fmt (im->extns[i].ly,0), 
	        fmt (im->extns[i].ux,1), fmt (im->extns[i].uy,0), 
	        fmt (im->extns[i].rotang,0));
	    strcat (line, buf);
	}

    } else {
        sprintf (line, "%s\t%s %s\t%s %s\t%s\n", imname, 
	    fmt (im->frame.lx,1), fmt (im->frame.ly,0), 
	    fmt (im->frame.ux,1), fmt (im->frame.uy,0), 
	    fmt (im->frame.rotang,0));
    }

    return (line);
}


/**
 *  FMT_CORNERS -- Print the explicit corners of an image footprint.
 */
static char *
fmt_corners (char *imname, ImInfo *im, int do_all)
{
    static char line[SZ_RESBUF];

    memset (line, 0, SZ_RESBUF);
    if (im->nextend && !do_all) {
	register int i = 0;
	char buf[SZ_RESBUF];

	for (i=0; i < im->nextend; i++) {
	    memset (buf, 0, SZ_RESBUF);

            sprintf (buf, "%s\t%s %s\t%s %s\t%s %s\t%s %s\n", imname, 
	        fmt (im->extns[0].xc[0],1), fmt (im->extns[0].yc[0],0),
	        fmt (im->extns[0].xc[1],1), fmt (im->extns[0].yc[1],0),
	        fmt (im->extns[0].xc[2],1), fmt (im->extns[0].yc[2],0),
	        fmt (im->extns[0].xc[3],1), fmt (im->extns[0].yc[3],0));
	    strcat (line, buf);
	}

    } else {
        sprintf (line, "%s\t%s %s\t%s %s\t%s %s\t%s %s\n", imname, 
	    fmt (im->frame.xc[0],1), fmt (im->frame.yc[0],0),
	    fmt (im->frame.xc[1],1), fmt (im->frame.yc[1],0),
	    fmt (im->frame.xc[2],1), fmt (im->frame.yc[2],0),
	    fmt (im->frame.xc[3],1), fmt (im->frame.yc[3],0));
    }
    return (line);
}
