#include <stdio.h>
#include <stdlib.h>
#include <sys/errno.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Tcl/tcl.h>
#include "ximtool.h"

/*
 * ISM_WCSPIX.C -- Client callbacks for the WCS/Pixel value ISM.
 *
 *	        wcspix_connect  (xim)
 *	     wcspix_disconnect  (xim)
 *	        wcspix_command  (xim, argc, argv)
 */


static int wcspix_debug = 0;
extern int errno;


/* WCS/Pixel ISM client callbacks. */
void  wcspix_connect(), wcspix_disconnect(), wcspix_command(), wcspix_message();



/* WCSPIX_CONNECT --  Called when the WCSPIX ISM first connects to the server.
 * Used to update with the current cache of mappings as well as initialize
 * the GUI that we are alive.
 */
void
wcspix_connect (xim, ism)
register XimDataPtr xim;
register IsmModule ism;

{
        MappingPtr mp;
	FrameBufPtr fr;
        register int i, j;
	char buf[SZ_LINE];

	if (wcspix_debug) printf ("ConnectCB for '%s' ....\n", ism->name);

	wcspix_message (xim, "startup");

	/* Update the ISM with the current set of mappings. */
        for (j=0; j < xim->nframes; j++) {
	    fr = &xim->frames[j];
            for (i=0; i < fr->nmaps; i++) {
                mp = &fr->mapping[i];
	        sprintf (buf, "cache %s %d", mp->ref, mp->id);
	        ism_message (xim, ism->name, buf);
	        sprintf (buf, "wcslist %d", mp->id);
	        ism_message (xim, ism->name, buf);

       		/* Send the object ref to the GUI. */
		sprintf (buf, "cache %s %d %d", mp->ref, fr->frameno, mp->id);
        	wcspix_message (xim, buf);

		if (wcspix_debug) printf ("connectCB:  '%s'\n", buf);
            }
        }
}


/* WCSPIX_DISCONNECT --  Called to shut down the WCSPIX ISM.  Send a "quit"
 * command to the module and notify the GUI.
 */
void
wcspix_disconnect (xim, ism)
register XimDataPtr xim;
register IsmModule ism;
{
        MappingPtr mp;
	FrameBufPtr fr;
        register int i, j;
	char buf[SZ_LINE];

	if (wcspix_debug) printf ("DisconnectCB for '%s' ....\n", ism->name);

	/* Uncache all the mapped images in the GUI. */
        for (j=0; j < xim->nframes; j++) {
	    fr = &xim->frames[j];
            for (i=0; i < fr->nmaps; i++) {
                mp = &fr->mapping[i];
		sprintf (buf, "uncache %d", mp->id);
        	wcspix_message (xim, buf);
	    }
	}

	/* If we got here from a GUI command send a quit command to the
	 * ISM, if the ISM shut down itself it will already be disconnected
	 * so check to see if we're still alive.
	 */
	if (ism->connected)
	    ism_message (xim, ism->name, "quit");

	/* Notify the GUI that we're done.  */
	wcspix_message (xim, "shutdown");
}


/* WCSPIX_COMMAND --  Handle all WCSPIX ISM specific commands.
 *
 *	set	wcspix_cmd   <path>
 *		pixtab_size  <size>
 *
 */
void
wcspix_command (xim, ism, argc, argv)
register XimDataPtr xim;
register IsmModule ism;
int	argc;
char	**argv;
{
	register int i;
	char	cmd[SZ_FNAME];

	if (wcspix_debug) printf ("CommandCB....\n");

	for (i=0; i < argc; i = i + 1) {
	    printf ("ism_cmd:  %d  '%s'\n", i, argv[i]);
	}
}


/* WCSPIX_MESSAGE -- Send a message to the user interface ism_msg callback,
 * but format it so it's delivered to the WCSPIX callback procedures.
 */
void
wcspix_message (xim, message)
register XimDataPtr xim;
char *message;
{
        char msgbuf[SZ_MSGBUF];

        sprintf (msgbuf, "setValue { deliver wcspix { %s } }", message);
        ObmDeliverMsg (xim->obm, "ism_msg", msgbuf);
}

