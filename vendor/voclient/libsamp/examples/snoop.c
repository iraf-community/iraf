/**
 *  SNOOP - Example task to subscribe to all message and print them out.
 *
 *  Usage:
 *		% snoop [-m mtype] [-s sender] [-v] [-d]
 *
 *  	where	-m <mtype>		print only the given mtype
 *  	     	-s <sender>		print messages from named sender
 *  	     	-v			verbose output
 *  	     	-d			debug output
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>

#include "samp.h"				/* LIBSAMP interface	*/


    
int	samp		= 0;			/* samp struct handle	*/

int	verbose		= 0;			/* task options		*/
int	debug		= 0;
char   *mtype		= NULL;
char   *filt_mtype	= NULL;
char   *filt_sender	= NULL;

static char *name	= "snoop";		/* metadata		*/
static char *descr	= "Example App";
    

static void   help_summary (void);



/****************************************************************************
 * Simple user-methods to install as message handlers.  In this case
 *  we have a simple method to filter message by type or sender so we
 *  only print the desired messages.
 */

void msg_handler (char *sender, char *msg_id, int params)
{
    if (filt_sender && strcasecmp (filt_sender, sender))
	return;
    if (filt_mtype && strcasecmp (filt_mtype, mtype))
	return;

    /*  Either no filters were set, or the message is of the requested type,
     *  print the contents.
     */
    samp_printMessage (mtype, sender, msg_id, params);
}



/****************************************************************************
 *  Program entry point.
 */
int
main (int argc, char **argv)
{
    int	   i, j, len;
    char   cmd[SZ_CMD];


    /* Process commandline arguments.
    */
    memset (cmd, 0, SZ_CMD);
    for (i=1; i < argc; i++) {
        if (argv[i][0] == '-' && !(isdigit(argv[i][1]))) {
            len = strlen (argv[i]);
            for (j=1; j < len; j++) {
                switch (argv[i][j]) {
                case 'm':  filt_mtype = argv[++i];   	j = len; break;
                case 's':  filt_sender = argv[++i];  	j = len; break;
                case 'd':  debug++;   				 break;
                case 'v':  verbose++;				 break;
                default:
                    fprintf (stderr, "Unknown option '%c'\n\n", argv[i][j]);
		    help_summary ();
                    break;
                }
            }
        } else
	    break;
    }


    /* Initialize the SAMP interface.
    */
    samp = sampInit (name, descr);

    /*  Set up some local application metadata values.  These tell the
     *  Hub and other applications who we are.
     */
    samp_Metadata (samp, "author.email", "Will E Coyote");
    samp_Metadata (samp, "author.name", "rascal@acme.com");

    /*  Subscribe to all message types and install the message handler.
     */
    samp_Subscribe (samp, "*",  msg_handler);

    /*  Register with the Hub and begin messaging .....
     */
    sampStartup (samp);

    /*  Loop until we're told to quit.
     */
    memset (cmd, 0, SZ_CMD);
    do {
	if (cmd[0] == 'q')
	    break;
	printf ("Type 'q' to quit ......\n");
    } while (fgets (cmd, SZ_CMD, stdin));


    /*  Clean up.  The Shutdownunregisters us from the Hub, we then need
     *  to close the interface separately to free any allocated memory.
     */
    if (sampShutdown (samp) < 0)
	fprintf (stderr, "Shutdown fails\n");
    sampClose (samp);

    return (0);
}



/********************************
 **   Private methods.
 *******************************/
static void
help_summary (void)
{ 
 fprintf (stderr,
     "  Usage:\n"
     "		%% snoop [-m mtype] [-s sender] [-v] [-d]\n"
     "\n"
     "  	where	-m <mtype>	print only the given mtype\n"
     "  	     	-s <sender>	print messages from named sender\n"
     "  	     	-v		verbose output\n"
     "  	     	-d		debug output\n"
     "\n" 
 );
}
