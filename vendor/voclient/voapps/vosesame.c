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


/* Print field flags	*/
#define	F_ALL		0001		    /* Print all fields		    */
#define	F_DEC		0002		    /* Print decimal position	    */
#define	F_ERR		0004		    /* Print position errors	    */
#define	F_NAM		0010		    /* Print object name	    */
#define	F_TYP		0020		    /* Print object type	    */
#define	F_SEX		0040		    /* Print sexagesimal position   */

#define MAX_FLAGS  	16
#define SZ_TARGET       64                  /* size of target name          */

#define NEXTARG(argc,argv,i) {if(i+1>=argc||(strlen(argv[i+1])>1&&argv[i+1][0]=='-'&&(!isdigit(argv[i+1][1])))){fprintf(stderr,"Error: Option '%s' requires an argument\n",argv[i]);break;}}


static int     flags[MAX_FLAGS];	    /* argv flags array		    */
static int	nflags		= 0; 	    /* number of flags		    */
static int	ntargets	= 0; 	    /* number of input targets	    */
static int	all_flags	= 0; 	    /* print all fields?	    */

static int	format		= TRUE;	    /* format the output?	    */
static int	header		= FALSE;    /* print a header?		    */
static int	invert		= FALSE;    /* invert to print non-matches  */
static int	debug		= FALSE;    /* std debug flag		    */
static int	verbose		= FALSE;    /* std verbose flag		    */
static int	quiet		= FALSE;    /* suppress output flag	    */
static int	user_pos	= TRUE;	    /* using user position?	    */
static int	status		= OK;	    /* return status		    */

static char	delim		= ' ';	    /* output table delimiter	    */
static char    *output		= (char *) NULL;

static char   *u_ra		= (char *) NULL,  /* user-defined position  */
       	      *u_dec		= (char *) NULL,
       	      *sep		= (char *) NULL;
static char   *next_arg	= (char *) NULL;

static FILE   *out;


/*  For getopt_long() option parsing.
*/
static  int   opt_index;
static 	char *opt_string = "aACdefFHhno:p:qsTtv";

static struct option long_options[] = {
    { "all",     0, 0, 'a' },	/* print all info about an object 	*/
    { "ascii",   0, 0, 'A' },	/* ASCII (space-delimited) output	*/
    { "comma",   0, 0, 'C' },	/* comma-delimited output		*/
    { "decimal", 0, 0, 'd' },	/* print pos in decimal degrees (def)	*/
    { "errors",  0, 0, 'e' },	/* print position errors		*/
    { "force",   0, 0, 'f' },	/* force new query (i.e. no cache)	*/
    { "format",  0, 0, 'F' },	/* format the output			*/
    { "header",  0, 0, 'H' },	/* print output header			*/
    { "help",    0, 0, 'h' },	/* print help summary			*/
    { "name",    0, 0, 'n' },	/* print object name			*/
    { "output",  1, 0, 'o' },	/* specify output file			*/
    { "pos",     1, 0, 'p' },	/* comma-separated input position	*/
    { "quiet",   0, 0, 'q' },	/* quiet (i.e. no) output		*/
    { "sex",     0, 0, 's' },	/* print sexagesimal coordinates	*/
    { "tab",     0, 0, 'T' },	/* tab-delimited output			*/
    { "type",    0, 0, 't' },	/* print object type			*/
    { "verbose", 0, 0, 'v' },	/* verbose output			*/
    { NULL,      0, 0, 0   }
};


extern  int     isDecimal(), isSexagesimal();
extern  float   sexa();
extern  char   *toSexa();

static  int  process_target (char *target);
static  int  print_result (char *target, Sesame sr);
static  void print_header (void);
static  void print_help (void);
static  void procUserCoord (char *u_ra, char *u_dec);


/**
 *  Application main().
 */
int
vosesame (int argc, char *argv[])
{
    register int  i=0, ch=0, arg_index=0, narg=0;


    /*  Process command line arguments and initialize.
     */
    nflags   = 0;
    out	     = stdout;
    memset (flags, 0, MAX_FLAGS);


    while (1) {
	ch = getopt_long (argc, argv, opt_string, long_options, &opt_index);

	if (ch > 0) {
	    switch (ch) {
	    case 'h':    
		print_help();     		
		return (0);
	    case 'v':    verbose++; quiet=0;	break;
	    case 'q':    quiet++;   verbose=0;	break;

	    case 'a':    all_flags++; 		break;
	    case 'd':    flags[nflags++] = F_DEC; 	break;
	    case 'e':    flags[nflags++] = F_ERR; 	break;
	    case 'n':    flags[nflags++] = F_NAM; 	break;
	    case 's':    flags[nflags++] = F_SEX; 	break;
	    case 't':    flags[nflags++] = F_TYP; 	break;

	    case 'i':    invert++; 			break;

	    case 'p':
		/* Check for a comma-separate position, break it up
		** if necessary.
		*/
		i = optind;
		NEXTARG(argc,argv,i);

		user_pos++;
		next_arg = optarg;
		if ((sep = (char *) strchr (next_arg, (int)','))) {
		    *sep = '\0';
		    u_dec = ++sep;
		    u_ra  = argv[++i];
		} else {
		    u_ra  = argv[++i]; 			/* FIXME */
		    u_dec = argv[++i]; 			/* FIXME */
		}
		if (debug)
		    fprintf (stderr, "ra='%s' dec='%s'\n",  u_ra,u_dec);
		if (u_ra == NULL || u_dec == NULL) {
		    fprintf (stderr, "ERROR: Invalid '-c' syntax\n");
		    exit (1);
		}

		/* Process a 'fake record' based on the position given.
		** Note this is done using the other options given up to
		** this point.
		*/
		procUserCoord (u_ra, u_dec);

		break;

	    case 'f':
		if (voc_initVOClient ("use_cache=no") == ERR)  {
		    fprintf (stderr, "ERROR: cannot open VOClient\n");
		    exit (1);
		}
		break;

	    case 'o':    
		NEXTARG(argc,argv,i);
		output = argv[++i]; 	
		if ((out = fopen (output, "w+")) == (FILE *)NULL) {
		    fprintf (stderr, "Cannot open file '%s'\n", output);
		    exit (1);
		}
		break;

	    case 'A':    delim = ' ';			break;
	    case 'C':    delim = ',',  format = 0;	break;
	    case 'F':    format++;			break;
	    case 'H':    header++;			break;
	    case 'T':    delim = '\t', format = 0;	break;
	    }

	} else {
	    /* Arguments without a leading '-' are assumed to be either
	    ** target names or @-files.  We let the processing routine
	    ** figure out how to handle it.
	    */
	    arg_index = optind + narg;
	    if ((status += process_target (argv[arg_index])) != OK) {
		if (!quiet) {
		    fprintf (stderr, "Warning: Cannot resolve target '%s'\n",
		        argv[arg_index]);
		}
	    }
	    narg++;
	}

	/* Final error check for argument overflow -- should never happen.
	*/
        if (narg > MAX_FLAGS) {
	    fprintf (stderr, "ERROR: Too many arguments specified\n");
	    return (1);
        }

	/* When we're done processing the arguments, break....
	*/
	if (arg_index == (argc - 1))
	    break;
    }


    /* Process the arguments from the standard input. 
    */
    if (!ntargets) {
        char *name = calloc (1, SZ_FNAME);

        while (fgets (name, SZ_FNAME, stdin)) {
	    name[strlen(name)-1] = '\0';
	    if ((status += process_target (name)) != OK) {
		if (!quiet) {
		    fprintf (stderr, "Warning: cannot resolve target '%s'\n",
			name);
		}
	    } else
		ntargets++;
	}
        free (name);
    }

    if (!ntargets)
	fprintf (stderr, "Warning: No target name specified.\n");

    if (out != stdout)
	fclose (out);


    return ( status );
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


    bzero (s_ra, SZ_FNAME);		/* initialize		*/
    bzero (s_dec, SZ_FNAME);
    bzero (scoords, SZ_FNAME);

    if (isDecimal (u_ra)) {
	dra = atof (u_ra);
	sprintf (s_ra, "%s", toSexa (dra / 15.0));
    } else if (isSexagesimal (u_ra)) {
	dra = sexa (u_ra);
	strcpy (s_ra, u_ra);
    } else {
	fprintf (stderr, "Error in RA specification: '%s'\n", u_ra);
	return;
    }

    if (isDecimal (u_dec)) {
	ddec = atof (u_dec);
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
    char    *ip, name[SZ_FNAME];


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
	    /* Clobber the newline and do a poor-man's URL encoding of
	    ** embedded spaces.
	    */
	    for (ip=name; *ip; ip++) {
		/*  preserve spaces.....  */
		if (*ip == ' ') *ip = '+';
		if (*ip == '\n') *ip = '\0';
	    }
            status = print_result (name, voc_nameResolver (name) );
	}
	fclose (fd); 			/* close the file and clean up	 */

    } else {
        /*  Print the result for a single resolved target.
        */
	for (ip=target; *ip; ip++)
	    if (*ip == ' ') *ip = '+'; 	/*  preserve spaces.....  */
        status = print_result (target, voc_nameResolver (target) );
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
    ra = voc_resolverRA(sr);
    dec = voc_resolverDEC(sr);
    Era = voc_resolverRAErr(sr);
    Edec = voc_resolverDECErr(sr);
    type = voc_resolverOtype(sr);
    pos = voc_resolverPos(sr);

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


/************************************************************************
**  PRINT_HELP --
*/
static void
print_help ()
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
        186.960000 -30.120000\n\
  \n\
  2) Print the sexagesimal coordinates of multiple objects\n\
     include the type:\n\
  \n\
        %% vosesame -st m31 m51 m99\n\
        00:42:44.31 +41:16:09.4 LINER\n\
        13:29:52.36 +47:11:40.8 Seyfert_2\n\
        12:18:49.51 +14:25:00.4 HII_G\n\
  \n\
  3) Print the decimal coordinates of those same objects listed\n\
     in the file 'myobjs.txt', output as CSV, include a header,\n\
     and print the id, coords, and type:\n\
  \n\
        %% vosesame -CHndt myobjs.txt\n\
        #Name,DRA,DDEC,Type,\n\
        m31,10.684625,41.269278,LINER\n\
        m51,202.468208,47.194667,Seyfert_2\n\
        m99,184.706333,14.416778,HII_G\n\
           :      :         :        :\n\
  ");

  printf ("\n\n");
}
