/**
 *  SAMP - Example task to send a single SAMP message for the cmdline.
 *
 *  Usage:
 *
 *	% samp [-hvd] [-t to] [-p pattern] [-f file] <cmd> [args ...]
 *
 *  	where	<cmd>			command to process
 *  	     	-h			print help summary
 *  	     	-d			debug output
 *  	     	-v			verbose output
 *
 *  	     	-m			handle multiple messages
 *  	     	-s <sender>		handle only messages from <sender>
 *
 *  	     	-t <to>			send to specified application (or all)
 *  	     	-p <pattern>		message pattern:  sync|async|notify
 *  	     	-f <file>		send all commands in the file
 *
 *  Subcommands:
 *
 *    snoop 				    print all received messages
 *
 *    status 				    print Hub availability
 *    list 				    list all registered clients
 *    access <appName>			    print <appName> availability
 *    handle <mtype>			    wait for <mtype> message
 *
 *    send <mtype> [<args> ...]		    generalized <mtype> message send
 *    exec <cmd>			    execute a client command
 *    pointAt <ra> <dec>		    point at given coords
 *    setenv  <name> <value>		    set an environment value
 *    getenv  <name>			    get an environment value
 *    setparam <name> <value>		    set a parameter value
 *    getparam <name>			    get a parameter value
 *
 *    loadImage <url>			    load the named image
 *    loadVOTable <url>			    load the named VOTable
 *    loadFITS <url>			    load the named FITS bintable
 *    showRow [<tblId>] [<url>] <row>	    highlight specified row
 *    selectRows [<tblId>] [<url>] <rows>   select specified rows
 *    bibcode <bibcode>			    load the named bibcode
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "samp.h"				/* LIBSAMP interface	*/

#define	SZ_MTYPE	64
#define	MAX_ARGS	8
#define MATCH(s)        (strcasecmp(cmd,s)==0)

int	samp		= 0;			/* samp struct handle	*/

int	verbose		= 0;			/* task options		*/
int	debug		= 0;
int	multiple	= 0;
int	interact	= 0;
char   *to		= "all";
char   *pattern		= NULL;
char   *cmdfile		= NULL;
char   *filt_mtype      = NULL;
char   *filt_sender     = NULL;
char    cmd[SZ_CMD];

FILE   *fd		= (FILE *) NULL;


static char *name	= "samp";		/* metadata		*/
static char *descr	= "CLI App";
    
static void msg_handler (char *sender, char *msg_id, int params);
static void procCmd (int samp, char *to, char *cmd, char *args[], int numargs);
static void help_summary (void);



/****************************************************************************
 *  Program entry point.
 */
int
main (int argc, char **argv)
{
    char   line[SZ_CMD], *args[MAX_ARGS];
    int	   i, j, len, numargs = 0;


    memset (cmd, 0, SZ_MTYPE);				/* initialize */
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

                case 'm':  multiple++;     		break;
                case 's':  filt_sender = argv[++i];     j = len; break;

                case 'i':  interact++; cmdfile = "-";  	j = len; break;
                case 'f':  cmdfile     = argv[++i];  	j = len; break;
                case 'p':  pattern     = argv[++i];  	j = len; break;
                case 't':  to          = argv[++i];	j = len; break;

                default:
                    fprintf (stderr, "Unknown option '%c'\n\n", argv[i][j]);
		    help_summary ();
                    break;
                }
            }
        } else { 
	    /*  Remainder of argv is the subcommand and its arguments.
	     */
	    if (!cmd[0])
                strcpy (cmd, argv[i]);
	    else {
		if (strcasecmp (cmd, "exec") == 0) {
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

    /*  Set alternative messaging pattern if requested. Valid values are
     *  'synch', 'asynch' or 'notify'.
     */
    if (pattern) {
	switch (tolower(pattern[0])) {
	case 's':  samp_setSyncMode (samp);	break;	    /* default 	*/
	case 'a':  samp_setASyncMode (samp);	break;
	case 'n':  samp_setNotifyMode (samp);	break;
	default:
	    if (verbose)
		fprintf (stderr, "Warning: Invalid pattern '%s'\n", pattern);
	}
    } else {
        /*  Use Synchronous mode by default so we don't exit before receiving
         *  the reply.  Otherwise, we could cause an error in the recipient.
         */
        samp_setSyncMode (samp);
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
	    memset (line, 0, SZ_CMD);

	    if (interact || fd == stdin)
		printf ("samp> ");

	    while (fgets (line, SZ_CMD, fd)) {
		line[strlen(line)-1] = '\0';	/* kill newline		*/
	    	memset (cmd, 0, SZ_CMD);
    	        memset (&args[0][0], 0, (SZ_MTYPE * MAX_ARGS));

		if (strncasecmp (line, "exec", 4) == 0) {
		    /* special-case for exec command to create a single arg  */
		    strcpy (cmd, "exec");
		    sprintf (args[0], "%s", &line[5]);
		    numargs = 1;

		} else {
        	    numargs = sscanf (line, "%s %s %s %s %s %s %s %s %s", 
		        cmd, args[0], args[1], args[2], args[3], 
		           args[4], args[5], args[6], args[7]);
		}

		procCmd (samp, to, cmd, args, numargs);
	    }
	    if (fd != stdin)
		fclose (fd);
	} else 
	    fprintf (stderr, "Cannot open input file '%s'\n", cmdfile);

    } else
	procCmd (samp, to, cmd, args, numargs);


    if (sampShutdown (samp) < 0) 			/*  clean up 	*/
	fprintf (stderr, "SAMP shutdown fails\n");
    sampClose (samp);

    return (0);
}


/**
 *  PROCCMD -- Process the command and its arguments.
 */
static void
procCmd (int samp, char *to, char *cmd, char *args[], int numargs)
{
    int   stat = 0;


    /*  Format the message and send it.
     */
    if (MATCH ("status")) {
	stat = (samp >= 0);

    } else if (MATCH ("access")) {
        stat = samp_Ping (samp, to);

    } else if (MATCH ("handle")) {
	int timeout = (atoi (args[1]) == 0 ? 999999 : atoi (args[1]));

	if (verbose)
	    fprintf (stderr, "Waiting for '%s' ....\n", args[0]);
	if (*args[1])
	    filt_sender = args[1];

	samp_Subscribe (samp, (filt_mtype = args[0]), msg_handler);
    	samp_DeclareSubscriptions (samp);
	sleep (timeout);

    } else if (MATCH ("snoop")) {
        /*  Subscribe to all message types and install the snoop handler.
         *  Sleep forever so the handler can print anything it receives.
         */
	multiple = 1;
        samp_Subscribe (samp, "*",  msg_handler);
    	samp_DeclareSubscriptions (samp);
	sleep (9999999);

    } else if (MATCH ("send")) {
	if (strcasecmp (to, "all"))	/* no recipient, use broadcast */
	    samp_setASyncMode (samp);
        stat = samp_sendGeneric (samp, to, args[0], &args[1]);

    } else if (MATCH ("ping")) {
        stat = samp_Ping (samp, to);

    } else if (MATCH ("loadImage")) {
        stat = samp_imageLoadFITS (samp, to, args[0], args[1], args[2]);

    } else if (MATCH ("loadFITS")) {
        stat = samp_tableLoadFITS (samp, to, args[0], args[1], args[2]);

    } else if (MATCH ("loadVOTable")) {
        stat = samp_tableLoadVOTable (samp, to, args[0], args[1], 
			args[2]);

    } else if (MATCH ("loadSpec")) {
	/*  NYI  */

    } else if (MATCH ("loadResource")) {
	/*  NYI  */

    } else if (MATCH ("showRow")) {
        stat = samp_tableHighlightRow (samp, to, args[0], args[1],
			atoi(args[2]));
	
    } else if (MATCH ("selectRows")) {
	/*  NYI  */

    } else if (MATCH ("pointAt")) {
        stat = samp_coordPointAtSky (samp, to, 
			atof(args[0]), atof(args[1]));


    } else if (MATCH ("exec")) {
	if (verbose)
	    printf ("Sending: '%s'\n", args[0]);
        samp_cmdExec (samp, to, args[0]);

    } else if (MATCH ("getenv")) {
        char *v = samp_envGet (samp, to, args[0]);
	printf ("%s\n", v);
  	free ((void *) v);
	return;

    } else if (MATCH ("setenv")) {
        stat = samp_envSet (samp, to, args[0], args[1]);

    } else if (MATCH ("getparam")) {
        char *v = samp_paramGet (samp, to, args[0]);
	printf ("%s\n", v);
  	free ((void *) v);
	return;

    } else if (MATCH ("setparam")) {
        stat = samp_paramSet (samp, to, args[0], args[1]);

    } else if (MATCH ("bibcode")) {
        stat = samp_bibLoad (samp, to, args[1]);

    } else {
	fprintf (stderr, "Error: unknown command '%s'\n", cmd);
    }

    if (verbose)
        fprintf (stderr, "%s\n", (stat < 0 ? "Error" : "OK"));
}


static void 
msg_handler (char *sender, char *msg_id, int params)
{
    extern int samp;

    if (filt_sender && strcasecmp (filt_sender, sender))
        return;

    /*  Either no filters were set, or the message is of the requested type,
     *  print the contents.
     */
    samp_printMessage (filt_mtype, samp_id2app (samp, sender), msg_id, params);

    if (!interact && !multiple) { 	/*  Do a clean disconnect ....	*/
        if (sampShutdown (samp) < 0)
	    fprintf (stderr, "SAMP shutdown fails\n");
        sampClose (samp);
        exit (0);
    }
}


static void
help_summary (void)
{ 
 fprintf (stderr,
 "  Usage:\n"
 "\n" 
 "	%% samp [-hvd] [-t to] [-p pattern] [-f file] <cmd> [args ...]\n" 
 "\n" 
 "  	where	<cmd>			command to process\n" 
 "  	     	-h			print help summary\n" 
 "  	     	-v			verbose output\n" 
 "  	     	-d			debug output\n" 
 "\n" 
 "  	     	-m			handle multiple messages\n"
 "  	     	-s <sender>		handly only msgs from <sender>\n"
 "\n" 
 "  	     	-t <to>			send to specified app (or all)\n" 
 "  	     	-p <pattern>		message pattern:  sync|async|notify\n" 
 "  	     	-f <file>		send all commands in the file\n" 
 "\n" 
 "  Commands:\n" 
 "\n" 
 "    snoop 				    print all received messages\n"
 "    send <mtype> [<args> ...]		    generalized <mtype> message send\n" 
 "\n" 
 "    status 				    print Hub availability\n" 
 "    list 				    list all registered clients\n"
 "    access <appName>			    print <appName> availability\n" 
 "    handle <mtype>			    wait for <mtype> message\n" 
 "\n" 
 "    exec <cmd>			    execute a client command\n" 
 "    setenv  <name> <value>		    set an environment value\n" 
 "    getenv  <name>			    get an environment value\n" 
 "    setparam <name> <value>		    set a parameter value\n" 
 "    getparam <name>			    get a parameter value\n" 
 "\n" 
 "    loadImage <url>			    load the named image\n" 
 "    loadVOTable <url>			    load the named VOTable\n" 
 "    loadFITS <url>			    load the named FITS bintable\n" 
 "    loadSpec <url>			    load the named spectrum\n" 
 "    loadResource <ivorn>		    load the named VO Resource\n" 
 "\n" 
 "    pointAt <ra> <dec>		    point at given coords\n" 
 "    showRow [<tblId>] [<url>] <row>	    highlight specified row\n" 
 "    selectRows [<tblId>] [<url>] <rows>   select specified rows\n" 
 "    bibcode <bibcode> 		    load the bibcode\n"
 "\n" 
 );
}
