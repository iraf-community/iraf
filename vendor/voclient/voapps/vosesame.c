/**
 *  VOSESAME -- Call the Sesame name resolver service on the specified target.
 *
 *  Usage:    vosesame [-adehntsv] [<objfile> | <target> [<target> ...]]
 *
 *     -a,--all		   print all information about the object
 *     -d,--decimal	   print position in decimal degrees (default)     
 *     -e,--errors	   print position errors
 *     -n,--name	   print object name
 *     -s,--sex		   print position as sexagesimal coordinates
 *     -t,--type	   print object type
 *
 *				INPUT PARAMETERS
 *     -f,--force	   force new query (i.e. no cache)
 *     -o,--output=<file>  specify output file
 *     -p,--pos		   comma-separated input position
 *
 *				OUTPUT PARAMETERS
 *     -h,--help	   print help summary
 *     -q,--quiet	   quiet (no) output
 *     -v,--verbose	   verbose output
 *     -A,--ascii	   ASCII output (i.e. space-delimited)
 *     -C,--comma	   comma-delimited output
 *     -F,--format	   format output
 *     -H,--header	   print header
 *     -T,--tab		   tab-delimited output
 *     -I,--init_cache	   initialize the resolver cache directory
 *
 *  Output is printed as whitespace delimited values in the same order
 *  in which the arguments appear.
 *
 *  M. Fitzpatrick, NOAO, June 2007
 *
 *  @file       vosesame.c
 *  @author     Mike Fitzpatrick
 *  @date       6/23/07
 *  @returns    0 if no error, 1 if an error occurs
 *
 *  @brief      Call the Sesame name resolver service on the specified target. 
 */


#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include "VOClient.h"
#include "voApps.h"


/* Print field flags	*/
#define	F_ALL		0001		    /* Print all fields		    */
#define	F_DEC		0002		    /* Print decimal position	    */
#define	F_ERR		0004		    /* Print position errors	    */
#define	F_NAM		0010		    /* Print object name	    */
#define	F_TYP		0020		    /* Print object type	    */
#define	F_SEX		0040		    /* Print sexagesimal position   */

#define MAX_FLAGS  	16
#define SZ_TARGET       64                  /* size of target name          */


static int   flags[MAX_FLAGS];	    	    /* argv flags array		    */
static int   nflags		= 0; 	    /* number of flags		    */
static int   ntargets		= 0; 	    /* number of input targets	    */
static int   all_flags		= 0; 	    /* print all fields?	    */

static int   format		= TRUE;	    /* format the output?	    */
static int   header		= FALSE;    /* print a header?		    */
static int   invert		= FALSE;    /* invert to print non-matches  */
static int   debug		= FALSE;    /* std debug flag		    */
static int   verbose		= FALSE;    /* std verbose flag		    */
static int   quiet		= FALSE;    /* suppress output flag	    */
static int   user_pos		= TRUE;	    /* using user position?	    */
static int   status		= OK;	    /* return status		    */

static char  delim		= ' ';	    /* output table delimiter	    */
static char *output		= (char *) NULL;

static char   *u_ra		= (char *) NULL,  /* user-defined position  */
       	      *u_dec		= (char *) NULL,
       	      *sep		= (char *) NULL;

static FILE   *out;


/*  For getopt_long() option parsing.
*/
/*  Task specific option declarations.
 */
int  vosesame (int argc, char **argv, size_t *len, void **result);

static Task  self       = {  "vosesame",  vosesame,  0,  0,  0  };

static 	char *opts = "rh%aACdefFHno:p:qsTtvI";
static struct option long_opts[] = {
    { "return",    0, 0, 'r' },     /* task option          		*/
    { "help",      2, 0, 'h' },     /* required             		*/
    { "test",      2, 0, '%' },     /* required             		*/

    { "all",       2, 0, 'a' },	    /* print all info about an object 	*/
    { "ascii",     2, 0, 'A' },	    /* ASCII (space-del) output		*/
    { "comma",     2, 0, 'C' },	    /* comma-delimited output		*/
    { "decimal",   2, 0, 'd' },	    /* print pos in dec. deg (def)	*/
    { "errors",    2, 0, 'e' },	    /* print position errors		*/
    { "force",     2, 0, 'f' },	    /* force new query (i.e. no cache)	*/
    { "format",    2, 0, 'F' },	    /* format the output		*/
    { "header",    2, 0, 'H' },	    /* print output header		*/
    { "name",      2, 0, 'n' },	    /* print object name		*/
    { "output",    1, 0, 'o' },	    /* specify output file		*/
    { "pos",       1, 0, 'p' },	    /* comma-separated input position	*/
    { "quiet",     2, 0, 'q' },	    /* quiet (i.e. no) output		*/
    { "sex",       2, 0, 's' },	    /* print sexagesimal coordinates	*/
    { "tab",       2, 0, 'T' },	    /* tab-delimited output		*/
    { "type",      2, 0, 't' },	    /* print object type		*/
    { "verbose",   2, 0, 'v' },	    /* verbose output			*/
    { "init_cache",2, 0, 'I' },	    /* init resolver cache		*/
    { NULL,        0, 0, 0   }
};

static void Usage (void);
static void Tests (char *input);


extern  int     isDecimal(), isSexagesimal();
extern  float   sexa();
extern  char   *toSexa();
extern  double  vot_atof (char *v);

static  int  process_target (char *target);
static  int  print_result (char *target, Sesame sr);
static  void print_header (void);
static  void procUserCoord (char *u_ra, char *u_dec);


/**
 *  Application entry point.
 */
int
vosesame (int argc, char *argv[], size_t *reslen, void **result)
{
    char **pargv, optval[SZ_FNAME];
    register int  ch=0, arg_index=1, narg=0;
    int    pos=0;


    /*  Process command line arguments and initialize.
     */
    nflags   = 0;
    out	     = stdout;
    *reslen  = 0;
    *result  = NULL;
    memset (flags, 0, MAX_FLAGS);

    pargv = vo_paramInit (argc, argv, opts, long_opts);
    while ((ch = vo_paramNext (opts,long_opts,argc,pargv,optval,&pos)) != 0) {
	if (ch > 0) {
	    switch (ch) {
	    case '%':    Tests (optval);  		return (self.nfail);
	    case 'h':    Usage();     			return (OK);
	    case 'v':    verbose++; quiet=0;		break;
	    case 'q':    quiet++;   verbose=0;		break;

	    case 'a':    all_flags++; 			break;
	    case 'i':    invert++; 			break;
	    case 'd':    flags[nflags++] = F_DEC; 	break;
	    case 'e':    flags[nflags++] = F_ERR; 	break;
	    case 'n':    flags[nflags++] = F_NAM; 	break;
	    case 's':    flags[nflags++] = F_SEX; 	break;
	    case 't':    flags[nflags++] = F_TYP; 	break;

	    case 'p':
		/* Check for a comma-separate position, break it up
		** if necessary.
		*/
		user_pos++;
		if ((sep = (char *) strchr (optval, (int)','))) {
		    *sep = '\0';
		    u_dec = strdup (++sep);
		    u_ra  = strdup (optval);
		}
		if (debug) fprintf (stderr, "ra='%s' dec='%s'\n",  u_ra,u_dec);
		if (u_ra == NULL || u_dec == NULL) {
		    fprintf (stderr, "ERROR: Invalid '-p' syntax\n");
		    return (ERR);
		}

		/* Process a 'fake record' based on the position given.
		** Note this is done using the other options given up to
		** this point.
		*/
		procUserCoord (u_ra, u_dec);
		break;

	    case 'f':
		if (voc_initVOClient("runid=voc.vosesame,use_cache=no") != OK) {
		    fprintf (stderr, "ERROR: cannot open VOClient\n");
		    return (ERR);
		}
		break;

	    case 'o':    
		output = strdup (optval);
		if ((out = fopen (output, "w+")) == (FILE *)NULL) {
		    fprintf (stderr, "Cannot open file '%s'\n", output);
		    return (ERR);
		}
		break;

	    case 'A':    delim = ' ';			break;
	    case 'C':    delim = ',',  format = 0;	break;
	    case 'F':    format++;			break;
	    case 'H':    header++;			break;
	    case 'T':    delim = '\t', format = 0;	break;

	    case 'I':    /*  FIXME  */
			 system ("/bin/rm -f ~/.voclient/cache/sesame/*");
			 exit (0);;
	    }

        } else if (ch == PARG_ERR) {
            return (ERR);

	} else {
	    /* Arguments without a leading '-' are assumed to be either
	    ** target names or @-files.  We let the processing routine
	    ** figure out how to handle it.
	    */
	    if ((status = process_target (optval)) != OK) {
		if (!quiet) {
		    fprintf (stderr, "Warning: Cannot resolve target '%s'\n",
		        optval);
		}
	    }
	    narg++;
	}
	arg_index++;

	/* Final error check for argument overflow -- should never happen.
	if (arg_index > argc)
	    break;
	*/
        if (narg > MAX_FLAGS) {
	    fprintf (stderr, "ERROR: Too many arguments specified\n");
	    return (1);
        }
    }


    /* Process the arguments from the standard input. 
    */
    if (!ntargets) {
        char *name = calloc (1, SZ_FNAME);

        while (fgets (name, SZ_FNAME, stdin)) {
	    name[strlen(name)-1] = '\0';		/* kill newline	*/
	    if ((status += process_target (name)) != OK) {
		if (!quiet) {
		    fprintf (stderr, 
			"Warning: cannot resolve target/access file '%s'\n",
			name);
		}
	    } else
		ntargets++;
	}
        free (name);
    }

    if (!ntargets)
	fprintf (stderr, "Warning: No target name specified.\n");

    if (u_ra)   free ((void *) u_ra);
    if (u_dec)  free ((void *) u_dec);
    if (output) free ((void *) output);

    if (out != stdout)
	fclose (out);

    return ( status );
}


/**
 *  USAGE -- Print task help summary.
 */
static void
Usage (void)
{
  printf ("\n\
    Usage:      sesame [-adehntsv] [<objfile> | <target> [<target> ...]]\n\
  \n\
      -a,--all            print all information about the object\n\
      -d,--decimal        print position in decimal degrees (default)\n\
      -e,--errors         print position errors\n\
      -n,--name           print object name\n\
      -s,--sex            print position as sexagesimal coordinates\n\
      -t,--type           print object type\n\
  \n\
                                INPUT PARAMETERS\n\
      -f,--force          force new query (i.e. no cache)\n\
      -o,--output=<file>  specify output file\n\
      -p,--pos            comma-separated input position\n\
  \n\
                                OUTPUT PARAMETERS\n\
      -h,--help           print help summary\n\
      -q,--quiet          quiet (no) output\n\
      -v,--verbose        verbose output\n\
      -A,--ascii          ASCII output (i.e. space-delimited)\n\
      -C,--comma          comma-delimited output\n\
      -F,--format         format output\n\
      -H,--header         print header\n\
      -T,--tab            tab-delimited output\n\
  \n\
    Output is printed as whitespace delimited values in the same order\n\
    in which the arguments appear.\n\
  \n\n");

  printf ("\n\
  Examples:\n  ---------\n\n\
  \n\
  1) Print the coordinates of NGC4456 decimal degrees\n\
  \n\
        %% vosesame ngc4456\n\
        186.968458 -30.097514\n\
  \n\
  2) Print the sexagesimal coordinates of multiple objects\n\
     include the type:\n\
  \n\
        %% vosesame -st m31 m51 m99\n\
	00:42:44.32 +41:16:07.5 LIN\n\
	13:29:52.69 +47:11:42.9 Sy2\n\
	12:18:49.62 +14:24:59.3 H2G\n\
  \n\
  3) Print the decimal coordinates of those same objects listed\n\
     in the file 'myobjs.txt', output as CSV, include a header,\n\
     and print the id, coords, and type:\n\
  \n\
        %% vosesame -CHndt myobjs.txt\n\
	#Name,DRA,DDEC,Type,\n\
	m31,10.684708,41.268750,LIN\n\
	m51,202.469575,47.195258,Sy2\n\
	m99,184.706771,14.416489,H2G\n\
           :      :        :      :\n\
  \n\
  4) Print the sexagesimal and decimal values for multiple user coords:\n\
  \n\
        %% vosesame -sd -p 12:30:0.0,-45:00:0.0 -p 127.5,2.05\n\
        12:30:00.0 -45:00:00.0  12.500000 -45.000000\n\
        12:30:00.0  02:03:00.0 187.500000   2.050000\n\
  \n\
  ");

  printf ("\n\n");
}


/**
 *  Tests -- Task unit tests.
 */
static void
Tests (char *input)
{
   Task *task = &self;

   vo_taskTest (task, "--help", NULL);

   vo_taskTestFile ("m31\nm51\nm99\n", "objs.txt");

   vo_taskTest (task, "ngc4456", NULL);					// Ex 1
   vo_taskTest (task, "-s", "ngc4456", NULL);				// Ex 2
   vo_taskTest (task, "-st", "m31", "m51", "m99", NULL);		// Ex 3
   vo_taskTest (task, "-CHndt", "objs.txt", NULL);			// Ex 4
   vo_taskTest (task, "-sd", "-p", "12:30:00.0,-45:00:00.0",
	"-p", "187.5,2.05", NULL);					// Ex 5

   if (access ("objs.txt", F_OK) == 0) 	unlink ("objs.txt");

   vo_taskTestReport (self);
}




/************************************************************************
**  PROCUSERCOORD --  Process a user-specified coordinate.  Generate a 
**  fake resolution record and output the requested values.
*/
static void
procUserCoord (char *ra, char *dec)
{
    register int i;
    char *fmt, sp;
    char  s_ra[SZ_FNAME], s_dec[SZ_FNAME], scoords[SZ_FNAME];
    double  dra = 0.0, ddec = 0.0;


    memset (s_ra, 0, SZ_FNAME);		/* initialize		*/
    memset (s_dec, 0, SZ_FNAME);
    memset (scoords, 0, SZ_FNAME);

    if (isDecimal (u_ra)) {
	dra = vot_atof (u_ra);
	sprintf (s_ra, "%s", toSexa (dra / 15.0));
    } else if (isSexagesimal (u_ra)) {
	dra = sexa (u_ra);
	strcpy (s_ra, u_ra);
    } else {
	fprintf (stderr, "Error in RA specification: '%s'\n", u_ra);
	return;
    }

    if (isDecimal (u_dec)) {
	ddec = vot_atof (u_dec);
	sprintf (s_dec, "%s", toSexa (ddec));
    } else if (isSexagesimal (u_dec)) {
	ddec = sexa (u_dec);
	strcpy (s_dec, u_dec);
    } else {
	fprintf (stderr, "Error in Dec specification: '%s'\n", u_dec);
	return;
    }

    sprintf (scoords, "%s %s", s_ra, s_dec);

    /*  Set the default output if we got no commandline flags so far.
    */
    ntargets++;
    if (nflags == 0) 
	flags[nflags++] = F_DEC;


    /*  Print the output in the order specified by the flags.  The exception
    **  is the '-a' flag to print all information we have, but we do so in a
    **  fixed format
    */
    if (all_flags) {
        if (format)
	    fmt = "%12.12s%c%23.23s%c%9.5f%c%9.5f%c0.0%c0.0%c%s\n"; 
	else
	    fmt = "%s%c%s%c%f%c%f%c0.0%c0.0%c%s\n"; 

        fprintf (out, fmt, "UserCoord", delim, scoords, delim,
		dra, delim, ddec, delim, delim, delim, "Unknown");

    } else {
	for (i=0; i < nflags; i++) {
	    sp = (i < (nflags - 1) ? delim : (char) 0);
	    switch (flags[i]) {
	    case F_DEC:
    		fprintf (out, "%f%c%f", (float)dra, delim, (float)ddec);
		break;
	    case F_ERR:
    		fprintf (out, "0.0%c0.0", delim);
		break;
	    case F_NAM:
    		fprintf (out, "UserCoord");
		break;
	    case F_TYP:
    		fprintf (out, "Unknown");
		break;
	    case F_SEX:
    		fprintf (out, "%s", scoords);
		break;
	    }
	    if (sp)
    		fprintf (out, "%c", sp);
	}
    	fprintf (out, "\n");
    }
}


/************************************************************************
**  PROCESS_TARGET --  Process a target name.
*/
static int
process_target (char *target)
{
    int     status;
    char    name[SZ_FNAME];
    extern  char *vo_urlEncode();


    /*  Do some error checking before we move on.
    */
    if (target == (char *)NULL) {
	fprintf (stderr, "ERROR: No target name or file specified.\n");
	return (ERR);
    }

    /*  Set the default output if we got no commandline flags so far.
    */
    ntargets++;
    if (nflags == 0) 
	flags[nflags++] = F_DEC;

    /*  Now call the Resolver Service and summarize the results.
    */
    if (target[0] == '@' || access (target, R_OK) == 0) {
	char  *fname = (target[0] == '@' ? &target[1] : &target[0]);
	FILE  *fd;


	/* Open the @-file and process the contents.  We assume there is
	** one target per line in the file, a target may contain spaces.
	*/
	if (access (fname, R_OK) != OK) {
	    fprintf (stderr, "Cannot access target file '%s'\n", fname);
	    return (ERR);
	} else {
	    if ((fd = fopen (fname, "r")) == (FILE *)NULL) {
	        fprintf (stderr, "Cannot open target file '%s'\n", fname);
	        return (ERR);
	    }
	}

	while (fgets (name, SZ_FNAME, fd)) {
	    name[strlen(name)-1] = '\0';		/* kill newline	*/
            status = print_result (name, voc_nameResolver (vo_urlEncode(name)));
	}
	fclose (fd); 			/* close the file and clean up	 */

    } else {
        /*  Print the result for a single resolved target.
        */
        status = print_result (target, voc_nameResolver (vo_urlEncode(target)));
    }

    return (status);
}


/************************************************************************
**  PRINT_RESULT --  Print the result table in the requested format.
*/
static int
print_result (char *target, Sesame sr)
{
    register int i, found;
    char     *type = NULL, *ip, *pos, sp;
    double   ra, dec, Era, Edec;


    if (sr == 0)			/* check for no match found	*/
	return (ERR);

    /* Fix the target name so spaces become underscores.
    */
    for (ip=target; *ip; ip++) {
        if (isspace (*ip) || *ip == '+')
    	    *ip = ' ';
    }

    /* First time through, check that the formatting makes sense and print
    ** the header if requested.
    */
    if (delim != ' ')
	format = 0;
    if (header && !invert) 
	print_header ();


    /* Get the information for the object.
    */
    pos = voc_resolverPos(sr);
    if (delim != ' ' && ip) {
        for (ip=pos; *ip; ip++)
            *ip = (isspace(*ip) ? delim : *ip);
    }
    ra   = voc_resolverRA(sr);
    dec  = voc_resolverDEC(sr);
    Era  = voc_resolverRAErr(sr);
    Edec = voc_resolverDECErr(sr);
    type = voc_resolverOtype(sr);
    pos  = voc_resolverPos(sr);

    found = 1;
    if (ra == 0.0 && dec == 0.0 && Era == 0.0 && Edec == 0.0)
	found = 0;


    /* Return if we didn't get any results.
    */
    if (!invert && !found)
	return (ERR);

    /*  Print the output in the order specified by the flags.  The exception
    **  is the '-a' flag to print all information we have, but we do so in a
    **  fixed format
    */
    if (all_flags) {
	char *fmt;


        if (format)
	    fmt = "%12.12s%c%23.23s%c%9.5f%c%9.5f%c%6.1f%c%6.1f%c%s\n"; 
	else
	    fmt = "%s%c%s%c%f%c%f%c%f%c%f%c%s\n"; 

	if (!invert || (!found && invert)) {
            fprintf (out, fmt, target, delim, pos, delim,
		ra, delim, dec, delim,
		Era, delim, Edec, delim,
		type);
	}

    } else if ((found && !invert) || (!found && invert)) {
	for (i=0; i < nflags; i++) {
	    sp = (i < (nflags - 1) ? delim : (char) 0);
	    switch (flags[i]) {
	    case F_DEC:
    		fprintf (out, "%f%c%f", ra, delim, dec);
		break;
	    case F_ERR:
    		fprintf (out, "%f%c%f", Era, delim, Edec);
		break;
	    case F_NAM:
    		fprintf (out, "%s", target);
		break;
	    case F_TYP:
    		fprintf (out, "%s", (type ? type : "Unknown"));
		break;
	    case F_SEX:
                for (ip=pos; *ip; ip++) {
                    if (isspace (*ip))
    	                *ip = delim;
                }
    		fprintf (out, "%s", pos);
		break;
	    }
	    if (sp)
    		fprintf (out, "%c", sp);
	}
    	fprintf (out, "\n");
    }


    return (OK);
}


/************************************************************************
**  PRINT_HEADER --  Put a header on the table.
*/
static void
print_header ()
{
    register int i;
    char     *fmt, sp;

	    
    if (all_flags) {
        if (format)
	    fmt = "# %10.10s%c%-11.11s%c%-10.10s%c%-9.9s%c%-9.9s%c%-6.6s%c%-6.6s%c%s\n"; 
        else
	    fmt = "# %s%c%s%c%s%c%s%c%s%c%s%c%s%c%s\n"; 

        fprintf (out, fmt, 
	    "Name", delim, 
	    (format ? "    RA" : "RA"), delim, 
	    (format ? "    DEC" : "DEC"), delim, 
	    (format ? "   DRA" : "DRA"), delim, 
	    (format ? "   DDEC" : "DDEC"), delim, 
	    "RAerr", delim, "DECerr", delim, 
	    "Type", delim);
    } else {
        fprintf (out, "#");
	for (i=0; i < nflags; i++) {
	    sp = (((nflags-i) > 1) || nflags >= 1 ? delim : (char) 0);
	    switch (flags[i]) {
	    case F_DEC:    fprintf (out, "DRA%cDDEC%c", sp, sp); 	break;
	    case F_ERR:    fprintf (out, "RAerr%cDECerr%c", sp, sp); 	break;
	    case F_NAM:    fprintf (out, "Name%c", sp); 		break;
	    case F_TYP:    fprintf (out, "Type%c", sp); 		break;
	    case F_SEX:    fprintf (out, "RA%cDEC%c", sp, sp); 		break;
	    }
	}
    	fprintf (out, "\n");
    }
    header = 0;			/* disable the header		*/
}
