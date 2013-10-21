/**
 *  VOATLAS -- Query the SkyView Image service for an all-sky image.
 *
 *  Usage:   voatlas [<opts>] <name> | <ra dec>
 *
 *  Where
 *       -%%,--test             run unit tests
 *       -h,--nelp              this message
 *       -d,--debug             enable debug messages
 *       -r,--return            return result from method
 *
 * 	 -b,--band <bpass>	Bandpass
 * 	 -l,--list 		List available surveys for position
 * 	 -p,--survey <survey>	Survey program name
 * 	 -g,--graphic 		Get a graphic image (i.e. JPEG)
 * 	 -n,--naxis <npix>	Set returned image size
 *
 * 	 -s,--size <size>	Field size
 *	      <size>s		Field size (arcsec)
 *	      <size>m		Field size (arcmin)
 *	      <size>d		Field size (degrees, default)
 *	 -F,--field <field>	Resolve query field name
 *	 -R,--ra <ra>		Set query RA position
 *	 -D,--dec <dec>		Set query Dec position
 *	 -P,--pos <ra,dec>	Set query as a POS string
 *	 -S,--samp 		Broadcast as SAMP message
 *	 -o <name>		Save image to named file
 *
 *	 <name>			Target name to be resolved to position
 *	 <ra> <dec>		Position to query (dec. deg or Eq. sexagesimal)
 *
 *
 *  @file       voatlas.c
 *  @author     Mike Fitzpatrick
 *  @date       7/03/12
 *
 *  @brief      Query the SkyView Image service for an all-sky image.
 */


#define  _GNU_SOURCE			/* for strcasestr() on linux	*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include "VOClient.h"
#include "voApps.h"
#include "samp.h"


static double  ra      	   = 0.0;	/* default values		*/
static double  dec     	   = 0.0;
static double  size    	   = 0.25;

static  int   debug	   = FALSE;	/* debug output			*/
static  int   do_samp	   = FALSE;	/* broadcast SAMP msg?		*/
static  int   graphic	   = FALSE;	/* get graphics format?		*/
static  int   list_surveys = FALSE;	/* list results?		*/
static  int   verbose      = FALSE;	/* verbose output?		*/
static  char *field	   = NULL;	/* Input field name		*/
static  char *pos	   = NULL;	/* Input position		*/
static  char *bpass	   = NULL;	/* Bandpass			*/
    
static  char  svc_url[SZ_URL], survey[SZ_FNAME];


static char *base_url = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?";

static  int 	voa_resolveField (char *field, double *ra, double *dec);
static  int 	voa_resolvePos (char *pos, double *ra, double *dec);
static  double 	voa_getSize (char *arg);
static  int     voa_callService (char *url, double ra, double dec, double size,
			char *ofname, char *format, int maximages);

extern	double	sexa (char *s);
extern  char   *strcasestr ();
extern  int     vot_atoi (char *v);
extern  double  vot_atof (char *v);


#ifdef USE_RESBUF
static char *resbuf;                    /* result buffer                */
#endif


/*  Task specific option declarations.
 */
int  voatlas (int argc, char **argv, size_t *len, void **result);

static int   do_return	= 0;
static Task  self       = {  "voatlas",  voatlas,  0,  0,  0  };
static char  *opts      = "%hF:R:D:P:b:dgs:o:Srlp:n:v";
static struct option long_opts[] = {
        { "field",        1, 0,   'F'},         /* query field name	*/
        { "ra",           1, 0,   'R'},         /* RA of position	*/
        { "dec",          1, 0,   'D'},         /* Dec of position	*/
        { "pos",          1, 0,   'P'},         /* POS position		*/
        { "graphics",     2, 0,   'g'},         /* graphics format?	*/
        { "band",         1, 0,   'b'},         /* bandpass		*/
        { "list",         2, 0,   'l'},         /* list surveys		*/
        { "survey",       1, 0,   'p'},         /* survey name		*/
        { "naxis",        1, 0,   'n'},         /* image size		*/
        { "size",         1, 0,   's'},         /* query size		*/
        { "output",       1, 0,   'o'},         /* set output name	*/
        { "samp",         2, 0,   'S'},         /* broadcast SAMP	*/
        { "verbose",      2, 0,   'v'},         /* required             */
        { "help",         2, 0,   'h'},         /* required             */
        { "debug",        2, 0,   'd'},         /* required (debug)     */
        { "test",         2, 0,   '%'},         /* required             */
        { "return",       1, 0,   'r'},         /* required             */
        { NULL,           0, 0,    0 }
};

static void Usage (void);
static void Tests (char *input);



/**
 *  Application entry point.
 */
int 
voatlas (int argc, char **argv, size_t *reslen, void **result)
{
    char **pargv, optval[SZ_FNAME], ch;
    char  *iname = NULL, *oname = NULL, *dlname = NULL;
    char   tmp[SZ_FNAME], buf[SZ_FNAME];
    int    i=1, status = OK, apos = 0, samp = -1, naxis = 512;


    /*  Initialize.
     */
    memset (buf, 0, SZ_FNAME);
    memset (tmp, 0, SZ_FNAME);
    memset (survey, 0, SZ_FNAME);
    memset (svc_url, 0, SZ_URL);

    *reslen = 0;
    *result = NULL;


    /*  Parse the argument list.
     */
    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&apos)) != 0) {
	i++;
        if (ch > 0) {
            switch (ch) {
	    case '%':  Tests (optval);			return (self.nfail);
	    case 'h':  Usage ();			return (OK);
	    case 'd':  debug++;				break;
	    case 'r':  do_return++;			break;

	    case 'b':  bpass  = strdup (optval); i++;  	break;
	    case 'g':  graphic++;			break;
	    case 'l':  list_surveys++;
	               strcpy (survey, "list");		break;
	    case 'p':  strcpy (survey, optval); i++;	break;
	    case 'n':  naxis = vot_atoi (optval); i++;	break;

	    case 'F':  field  = strdup (optval); i++;  	break;
	    case 'R':  if (strchr (optval, (int)':'))
	    		  ra = (15. * (double) sexa (optval));
		       else
	    		  ra = (double) vot_atof (optval);
		       i++;
		       break;
	    case 'D':  if (strchr (optval, (int)':'))
	    		  dec = (double) sexa (optval);
		       else
	    		  dec = (double) vot_atof (optval);
		       i++;
		       break;
	    case 'P':  sscanf (optval, "%lf,%lf", &ra, &dec);
		       i++;
		       break;

	    case 's':  size = voa_getSize(optval); i++; break;
	    case 'o':  oname = strdup (optval); i++;  	break;

	    case 'S':  do_samp = 1;			break;
	    case 'v':  verbose = 1;			break;

	    default:
		fprintf (stderr, "Invalid argument '%c'\n", ch);
		return (ERR);
	    }

        } else if (ch == PARG_ERR) {
            return (ERR);

	} else {
	    if (i == (argc-2)) {
		sprintf (buf, "%s %s", optval, argv[i+1]);
		pos = strdup (buf);
	        sscanf (pos, "%lf %lf", &ra, &dec);
	    } else
		field = strdup (optval);
	    break;
	}
    }


    /*  Sanity checks.
     */
    if (!field && !pos && (ra == 0.0 && dec == 0.0)) {
	fprintf (stderr, "Error: no field/position specified.\n");
	return (ERR);
    }

    if (!survey[0]) {
	/*  No survey specified so go by the bandpass.
	 */
        if (!bpass)
	    strcpy (survey, "dss2b");
        else {
	    if (strncasecmp (bpass, "optical", 3) == 0)
	        strcpy (survey, "dss");
	    else if (strncasecmp (bpass, "infrared", 3) == 0 ||
		   strncasecmp (bpass, "ir", 2) == 0)
	        strcpy (survey, "2massk");
	    else if (strncasecmp (bpass, "x-ray", 1) == 0)
	        strcpy (survey, "pspc2int");
	    else if (strncasecmp (bpass, "euv", 3) == 0)
	        strcpy (survey, "euve83");
	    else if (strncasecmp (bpass, "gamma-ray", 5) == 0)
	        strcpy (survey, "egret1000");
	    else if (strncasecmp (bpass, "radio", 3) == 0)
	        strcpy (survey, "4850mhz");
        }
    } else if (strcasecmp (survey, "list") == 0) {
	list_surveys = 1;
	memset (survey, 0, SZ_FNAME);
    }


    /*  Setup the output name.
     */
    if (oname) {
	dlname = oname;			    /* output name specified	*/
    } else if (do_samp) {
	strcpy (tmp, "/tmp/voatlasXXXXXX");    /* temp download name	*/
	mktemp (tmp);
	dlname = tmp;
    } else {
	if (field)
	    sprintf (buf, "%s.%s", field, (graphic ? "jpg" : "fits"));

	else if (pos) {
	    char *ip, *op;

	    memset (buf, 0, SZ_FNAME);
	    for (op=buf, ip=pos; *ip; ip++)
		*op++ = (*ip == ' ' ? '_' : *ip);
	    strcat (buf, ".");
	    strcat (buf, (graphic ? "jpg" : "fits"));

	} else if (list_surveys) {
	    if (verbose)
	        fprintf (stderr, 
		    "Warning: no field/position specified, using (0.,0.).\n");

	} else {
	    fprintf (stderr, "Error: no output name specified.\n");
	    status = ERR;
	    goto done_;
	}
	dlname = oname = strdup (buf);
    }

    
    if (debug) {
	fprintf (stderr, "field='%s' pos='%s' ra=%g dec=%g\n", 
	    field, pos, ra, dec);
	fprintf (stderr, "bpass='%s' survey='%s'\n", bpass, survey);
	fprintf (stderr, "oname='%s'\n", oname);
    }

    /*  Initialize the VOClient code.  Error messages are printed by the
     *  interface so we just quit if there is a problem.
     */
    if (voc_initVOClient ("runid=voc.voatlas") == ERR) 
        return (ERR);

    /* Sanity checks
     */
    if (iname == NULL) iname = strdup ("stdin");
    if (oname == NULL) oname = strdup ("stdout");
    if (strcmp (iname, "-") == 0) { free (iname), iname = strdup ("stdin");  }
    if (strcmp (oname, "-") == 0) { free (oname), oname = strdup ("stdout"); }

    if (field && pos) {
	fprintf (stderr, 
	    "Error: only one of 'field' or 'pos' may be specified.\n");
	    status = ERR;
	    goto done_;
    } else if (field) {
	if (voa_resolveField (field, &ra, &dec) == OK) {
	    fprintf (stderr, "Error: cannot resolve object '%s'\n", field);
	    status = ERR;
	    goto done_;
	}
    } else if (pos) {
	if (voa_resolvePos (pos, &ra, &dec) != OK) {
	    fprintf (stderr, "Error: cannot convert position '%s'\n", pos);
	    status = ERR;
	    goto done_;
	}
    }


    /*  Form the service URL.
     */
    memset (svc_url, 0, SZ_URL);
    sprintf (svc_url, "%snaxis=%d&", base_url, naxis);
    if (survey[0]) {
	strcat (svc_url, "survey=");
	strcat (svc_url, survey);
	strcat (svc_url, "&");
    }
    if (debug)
	fprintf (stderr, "ra = %g  dec = %g\nurl='%s'\n", ra, dec, svc_url);


    /*  Call the SkyView SIA service.
     */
    if (voa_callService (svc_url, ra, dec, size, dlname, 
	(graphic ? "jpeg" : "fits"), 1) == ERR) {
	    fprintf (stderr, "Error: cannot contact SkyView service\n");
	    status = ERR;
	    goto done_;
    }

    /*  Broadcast the image as a message if requested.
     */
    if (do_samp) {
	if ((samp = sampInit ("voatlas", "VOClient Task")) >= 0) {
	    char url[SZ_LINE], cwd[SZ_LINE];

	    samp_setSyncMode (samp);	/* use asynchronous mode	*/
	    sampStartup (samp);		/* register w/ Hub		*/

	    memset (url, 0, SZ_LINE);
	    memset (cwd, 0, SZ_LINE);
	    getcwd (cwd, SZ_LINE);
	    if (tmp[0])
	        sprintf (url, "file://%s", dlname);
	    else
	        sprintf (url, "file://%s/%s", cwd, oname);

	    if (verbose)
		printf ("Displaying image %s ...\n", url);
            (void) samp_imageLoadFITS (samp, "all", url, "", field);

	    if (tmp[0])
		unlink (dlname);
	    sampShutdown (samp);
	}
    }


    /*  See if we're returning the image.
     */
    if (do_return) {
	vo_setResultFromFile (dlname, reslen, result);
        unlink (dlname);
    }


    /*  Clean up and shutdown.
     */
done_:
    if (pos)    free (pos);
    if (field)  free (field);
    if (iname)  free (iname);
    if (oname)  free (oname);

    voc_closeVOClient (1);
    vo_paramFree (argc, pargv);

    sleep (2);		/*  FIXME -- SkyView seems to have a reset issue...  */

    return (status);
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
    fprintf (stderr, "\n  Usage:\n\t"
        "voatlas [<opts>] [<field> | <pos>]\n\n"
        "  where\n"
	"       -%%,--test              run unit test\n"
	"       -d,--debug              enable debugging\n"
	"       -h,--nelp               this messag\n"
	"       -r,--return             return result from metho\n"
	"\n"
	" 	-b,--band <bpass>	Bandpas\n"
	" 	-p,--survey <survey>	Survey program nam\n"
	" 	-g,--graphic 		Get a graphic image (i.e. JPEG\n"
	" 	-n,--naxis <npix>	Set returned image size\n"
	"\n"
	" 	-s,--size <size>	Field siz\n"
	"	      <size>s		Field size (arcsec\n"
	"	      <size>m		Field size (arcmin\n"
	"	      <size>d		Field size (degrees, default\n"
	"	-F,--field <field>	Resolve query field nam\n"
	"	-R,--ra <ra>		Set query RA positio\n"
	"	-D,--dec <dec>		Set query Dec positio\n"
	"	-P,--pos <ra,dec>	Set query as a POS strin\n"
	"	-S,--samp 		Broadcast as SAMP messag\n"
	"	-v,--verbose 		Verbose output\n"
	"	-o <name>		Save image to named fil\n"
	"\n"
	"	 <name>			Target name to be resolved\n"
	"	 <ra> <dec>		Position to query\n"
        "\n"
        "Examples:\n\n"
	"    1)  Display an image of M83 on Aladin using SAMP\n\n"
	"	    %% voatlas --samp m83\n"
	"\n"
	"    2)  Get a 256x256 JPEG image of the Sombrero galaxy\n\n"
	"	    %% voatlas -o gal.jpg -n 256 --graphic sombrero\n"
	"\n"
	"    3)  Get a 20 arcmin Wise 2.2micron survey image of m101\n\n"
	"	    %% voatlas -s 20m --survey=wise22 m101\n"
	"\n"
	"    4)  Get a radio image of 3c273, image will be '3c273.fits'\n\n"
	"	    %% voatlas --band=radio 3c273\n"
	"\n"
	"    5)  List (verbose) the survey images available for ngc1234\n\n"
	"	    %% voatlas --survey=list -v ngc1234\n"
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

    vo_taskTest (task, "--help", NULL);
    vo_taskTest (task, "-S", "m83", NULL);				// Ex 1
    vo_taskTest (task, "--samp", "m83", NULL);

    vo_taskTest (task, "-o", "gal.jpg", "-n", "256", 			// Ex 2
	"--graphic", "sombrero", NULL);

    vo_taskTest (task, "-s", "20m", "--survey=wise22", "m101", NULL);	// Ex 3

    vo_taskTest (task, "--band=radio", "3c273", NULL);			// Ex 4

    vo_taskTest (task, "--survey=list", "-v", "ngc1234", NULL);		// Ex 5

    vo_taskTest (task, "m51", NULL);
    vo_taskTest (task, "-o", "m51.fits", "m51", NULL);	
    vo_taskTest (task, "--ra=202.47", "--dec=47.195", "--samp", NULL);

    /*  Clean up any files we created.
     */
    if (access ("m51.fits",   F_OK) == 0) unlink ("m51.fits");
    if (access ("m101.fits",  F_OK) == 0) unlink ("m101.fits");
    if (access ("3c273.fits", F_OK) == 0) unlink ("3c273.fits");
    if (access ("gal.jpg",    F_OK) == 0) unlink ("gal.jpg");

    vo_taskTestReport (self);
}



/**
 *  Simple test routine to call a Siap search service and summarize results.
 */
static int
voa_callService (char *svc_url, double ra, double dec, double size,
    char *ofname, char *format, int maximages)
{
    char *acref    = NULL, *fmt = NULL, *program = NULL;
    double  dra, ddec;
    int   i, nrec = 0, recnum = 0;
    FILE *fd = (FILE *) NULL;


    DAL	      siap;				/* DAL Connection	 */
    Query     query;				/* query handle		 */
    QResponse qr;                               /* query response handle */
    QRecord   rec;				/* result record handle	 */
    QRAttribute v;				/* dataset attribute	 */
	

    /*  Get a new connection to the named service.
     */
    siap = voc_openSiapConnection (svc_url);    /* open a connection    */

    /*  Form a query.  Here we'll use the one search size we're given for
     *  both the RA,DEC sizes, and specify a null format.
     */
    query = voc_getSiapQuery (siap, ra, dec, size, size, 
	(list_surveys ? NULL : (graphic ? "image/jpeg" : "image/fits")));


    if (VOAPP_DEBUG) {
        fprintf (stderr, "Executing Query:\n  %s\n\n", 
            voc_getQueryString (query, SIAP_CONN, 0));
    }
        

    /* Execute the query.
     */
    qr = voc_executeQuery (query);                  /* execute the query    */
    if ((nrec = voc_getRecordCount (qr)) <= 0)
        return (ERR);

    if (do_return && list_surveys)
	fd = fopen (ofname, "w+");
    else
	fd = stdout;


    /*  Download the first 'maximages' images.
     */
    for (i=0; i < nrec && i < nrec; i++) {
	rec = voc_getRecord (qr, i);            /* get a row in the table   */

	v = voc_getAttribute (rec, "Format"); 	/* get the right format	    */
	if (!(fmt = voc_stringValue(v)) || strcasestr (fmt, format) == NULL) 
	    if (!list_surveys)
	 	continue;

	v = voc_getAttribute (rec, "Title"); /* get the survey   	    */
	program = voc_stringValue(v);
	if (list_surveys) {
	    v = voc_getAttribute (rec, "Ra");   dra = voc_floatValue (v);
	    v = voc_getAttribute (rec, "Dec");  ddec = voc_floatValue (v);
	    if (verbose)
	        fprintf (fd, "%3d  %12.12s  %g %g  %s\n",
		    (i+1), program, dra, ddec, fmt);
	    else
	        fprintf (fd, "%3d  %12.12s  %s\n", (i+1), program, fmt);

	} else if (program) {
	    int  plen = strlen (program);
	    int  slen = strlen (survey);

	    if (strncasecmp (program, survey, min(plen,slen)) == 0) {
	        if ((v = voc_getAttribute (rec, "AccessReference"))) {
	            acref = voc_stringValue (v);
		    if (verbose) {
			printf ("Downloading %d of %d: '%s' ....", 
			    i, nrec, ofname);
			fflush (stdout);
		    }
	            if ( voc_getDataset (rec, acref, ofname) != OK ) {
		        if (verbose)
			    printf ("error downloading file\n");
                        return (ERR);
	            }
		    if (verbose)
			printf ("done\n");

	            if ( ++recnum >= maximages )
	                break;
	        }
	    }
	}
    }

    if (fd != stdout)
	fclose (fd);

    voc_closeConnection (siap);			/* close the siap connection */
    return (OK);
}


/**
 *  VOA_RESOLVEPOS -- Resolve a position string to decimal degrees.
 */
static int
voa_resolvePos (char *pos, double *ra, double *dec)
{
    char *ip = NULL, *s1 = NULL, *s2 = NULL;


    if ( (ip = strchr (pos, (int)' '))) {
	*ip = '\0';
	s1 = pos;
	s2 = ip + 1;

	if (strchr (s1, (int)':'))
	    *ra = (15. * (double) sexa (s1));
	else
	    *ra =  (double) vot_atof (s1);
	*dec = (strchr (s2,(int)':') ? 
	    (double) sexa (s2) : (double) vot_atof (s2));

	return (OK);
    } else
	return (ERR);

}


/**
 *  VOA_RESOLVEFIELD -- Resolve an object field name to a position in decimal
 *  degrees.
 */
static int
voa_resolveField (char *field, double *ra, double *dec)
{
    Sesame sr;
    extern  char *vo_urlEncode();


    if ((sr = voc_nameResolver (vo_urlEncode (field))) != 0) {
        *ra = (double) voc_resolverRA (sr);
        *dec = (double) voc_resolverDEC (sr);
    }
    return ((sr > 0) ? OK : ERR);
}


/**
 *  VOA_GETSIZE -- Convert an argument size spec into a decimal degree value.
 */
static double
voa_getSize (char *arg)
{
    int  len = strlen (arg);
    int  unit = 'd';
    double  size = (double) 0.0;


    if (isalpha ((int) arg[len-1]) && arg[len-1] != '.') {
	unit = tolower (arg[len-1]);
	if (strchr ("mdsMDS", unit) == NULL) {
	    fprintf (stderr, "Error: Invalid size unit '%c'\n", unit);
	    return (size);
	}
	arg[len-1] = '\0';
    } 

    /*  Convert to decimal degrees for the query.
     */
    switch (unit) {
    case 'd': 	return ( vot_atof (arg)         );
    case 'm': 	return ( vot_atof (arg) / 60.   );
    case 's': 	return ( vot_atof (arg) / 3600. );
    }

    return ((double) 0.0);
}
