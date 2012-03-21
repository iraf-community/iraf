/**
 *  SEND - Example task to send a single SAMP message for the cmdline.
 *
 *  Usage:
 *
 *	% send [-hvd] [-r recipient] [-p pattern] [-f file] <mtype> [args ...]
 *
 *  	where	<mtype>			mtype of message to send
 *  	     	-r <recipient>		send to specified application (or all)
 *  	     	-p <pattern>		message pattern:  sync|async|notify
 *  	     	-f <file>		send all commands in the file
 *  	     	-h			print help summary
 *  	     	-v			verbose output
 *  	     	-d			debug output
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "samp.h"				/* LIBSAMP interface	*/

#define	SZ_MTYPE	64
#define	MAX_ARGS	8
#define MATCH(s)        (strcasecmp(mtype,s)==0)

int	samp		= 0;			/* samp struct handle	*/

int	verbose		= 0;			/* task options		*/
int	debug		= 0;
char   *recip		= "all";
char   *pattern		= NULL;
char   *cmdfile		= NULL;
char    mtype[SZ_MTYPE];

FILE   *fd		= (FILE *) NULL;


static char *name	= "send";		/* metadata		*/
static char *descr	= "Example App";
    
static void help_summary (void);
static void procCmd (int samp, char *recip, char *mtype, char *args[],
			int numargs);


/****************************************************************************
 *  Program entry point.
 */
int
main (int argc, char **argv)
{
    char   cmd[SZ_CMD], *args[MAX_ARGS], mtype[SZ_CMD];
    int	   i, j, len, numargs = 0;


    memset (mtype, 0, SZ_MTYPE);		/* initialize */
    for (i=0; i < MAX_ARGS; i++) 
	args[i] = calloc (1, SZ_LINE);

    /* Process commandline arguments.
    */
    for (i=1; i < argc; i++) {
        if (argv[i][0] == '-' && !(isdigit(argv[i][1]))) {
            len = strlen (argv[i]);
            for (j=1; j < len; j++) {
                switch (argv[i][j]) {
                case 'h':  help_summary ();  		return (0);
                case 'd':  debug++;   			break;
                case 'v':  verbose++;			break;

                case 'f':  cmdfile = argv[++i];  	j = len; break;
                case 'p':  pattern = argv[++i];  	j = len; break;
                case 'r':  recip   = argv[++i];		j = len; break;

                default:
                    fprintf (stderr, "Unknown option '%c'\n\n", argv[i][j]);
		    help_summary ();
                    break;
                }
            }
        } else { 
	    /*  Remainder of argv is the mtype and its arguments.
	     */
	    if (!mtype[0])
                strcpy (mtype, argv[i]);
	    else {
		if (strcasecmp (mtype, "client.cmd.exec") == 0) {
		    strcat (args[0], argv[i]);
		    strcat (args[0], " ");
		} else
                    strcpy (args[numargs++], argv[i]);
	    }
	}
    }


    /*  Initialize the SAMP interface.
    */
    samp = sampInit (name, descr);

    /*  Use Synchronous mode by default so we don't exit before receiving
     *  the reply.  Otherwise, we could cause an error in the recipient.
     */
    samp_setSyncMode (samp);

    /*  Set alternative messaging pattern if requested. Valid values are
     *  'synch', 'asynch' or 'notify'.  This could just as well be done 
     *  after the startup and would take effect for subsequent messages.
     */
    if (pattern) {
	switch (tolower(pattern[0])) {
	case 's':    break;			    /* default mode	*/
	case 'a':    samp_setASyncMode (samp);	    break;
	case 'n':    samp_setNotifyMode (samp);	    break;
	default:
	    if (verbose)
		fprintf (stderr, "Warning: Invalid pattern '%s'\n", pattern);
	}
    }

    /*  Register with the Hub and begin messaging.  Since we're a single-shot
     *  command we won't bother to register metadata or subscribe to
     *  messages.  The startup will run the server thread to handle the
     *  responses
     */
    sampStartup (samp);


    /*  Process the messages in the named file, or from the cmdline.  Since
     *  there is some overhead in connecting to the hub, this is an efficient
     *  way to send multiple messages from a single connection.
     */
    if (cmdfile) {
	fd = (cmdfile[0] == '-' ? stdin : fopen (cmdfile, "r"));
	if (fd) {
	    memset (cmd, 0, SZ_CMD);
	    while (fgets (cmd, SZ_CMD, fd)) {
		cmd[strlen(cmd)-1] = '\0';	/* kill newline		*/
	    	memset (mtype, 0, SZ_CMD);
    	        memset (&args[0][0], 0, (SZ_MTYPE * MAX_ARGS));

		if (strncasecmp (cmd, "client.cmd.exec", 15) == 0) {
		    /* special-case for exec command to create a single arg  */
		    strcpy (mtype, "client.cmd.exec");
		    sprintf (args[0], "%s", &cmd[16]);
		    numargs = 1;

		} else {
        	    numargs = sscanf (cmd, "%s %s %s %s %s %s %s %s %s", 
		        mtype, args[0], args[1], args[2], args[3], 
		           args[4], args[5], args[6], args[7]);
		}

		procCmd (samp, recip, mtype, args, numargs);
	    }
	    if (fd != stdin)
		fclose (fd);
	} else 
	    fprintf (stderr, "Cannot open input file '%s'\n", cmdfile);

    } else
	procCmd (samp, recip, mtype, args, numargs);


    if (sampShutdown (samp) < 0) 			/*  clean up 	*/
	fprintf (stderr, "SAMP shutdown fails\n");
    sampClose (samp);

    return (0);
}


/**
 *
 */
static void
procCmd (int samp, char *recip, char *mtype, char *args[], int numargs)
{
    int   stat = 0;


    /*  Format the message and send it.
     */
    if (MATCH ("samp.app.ping")) {
        stat = samp_Ping (samp, recip);

    } else if (MATCH ("table.load.fits")) {
        stat = samp_tableLoadFITS (samp, recip, args[0], args[1], args[2]);

    } else if (MATCH ("table.load.votable")) {
        stat = samp_tableLoadVOTable (samp, recip, args[0], args[1], 
			args[2]);

    } else if (MATCH ("table.highlight.row")) {
        stat = samp_tableHighlightRow (samp, recip, args[0], args[1],
			atoi(args[2]));
	
    } else if (MATCH ("image.load.fits")) {
        stat = samp_imageLoadFITS (samp, recip, args[0], args[1], args[2]);

    } else if (MATCH ("coord.pointAt.sky")) {
        stat = samp_coordPointAtSky (samp, recip, 
			atof(args[0]), atof(args[1]));

    } else if (MATCH ("client.cmd.exec")) {
	if (verbose)
	    printf ("Sending: '%s'\n", args[0]);
        samp_cmdExec (samp, recip, args[0]);

    } else if (MATCH ("client.env.get")) {
        char *v = samp_envGet (samp, recip, args[0]);
	printf ("%s\n", v);
  	free ((void *) v);

    } else if (MATCH ("client.env.set")) {
        stat = samp_envSet (samp, recip, args[0], args[1]);

    } else if (MATCH ("client.param.get")) {
        char *v = samp_paramGet (samp, recip, args[0]);
	printf ("%s\n", v);
  	free ((void *) v);

    } else if (MATCH ("client.param.set")) {
        stat = samp_paramSet (samp, recip, args[0], args[1]);

    } else if (MATCH ("bibcode.load")) {
        stat = samp_bibLoad (samp, recip, args[1]);

    } else {
        stat = samp_sendGeneric (samp, recip, mtype, args);
    }

    if (verbose)
        fprintf (stderr, "%s\n", (stat < 0 ? "Error" : "OK"));
}


static void
help_summary (void)
{ 
 fprintf (stderr,
 "  Usage:\n"
 "	%% send [-hvd] [-f file] [-r recip] [-p pattern] <mtype> [args....]\n"
 "\n"
 "  	where	<mtype>			mtype of message to send\n"
 "  	     	-r <recipient>		send to specified app (or 'all')\n"
 "  	     	-p <pattern>		message pattern: sync|async|notify\n"
 "  	     	-f <file>		send messages in the named file\n"
 "  	     	-h			print help summary\n"
 "  	     	-v			verbose output\n"
 "  	     	-d			debug output\n"
 "\n" 
 "      Valid mtypes and required params are:\n"
 "\n" 
 "    	    samp.app.ping\n"
 "    	    table.load.votable  url [table-id] [name]\n"
 "    	    table.load.fits  	url [table-id] [name]\n"
 "    	    image.load.fits  	url [image-id] [name]\n"
 "    	    client.cmd.exec  	cmd_str\n"
 "    	    client.env.set  	keyw value\n"
 "    	    client.env.get  	keyw\n"
 "    	    client.param.set  	keyw value\n"
 "    	    client.param.get  	keyw\n"
 "\n" 
 );
}
