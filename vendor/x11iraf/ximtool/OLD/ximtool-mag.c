#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Obm.h>

#define XIMTOOL_MAIN
#include "ximtool.h"

/*
 * XIMTOOL -- X11 based image display server and standalone image display
 * client.  This program is based on the IRAF prototype widget server (object
 * manager library or OBM) and the gterm-image widget.
 */

/* Compatibility hacks. */
#ifdef AUX
void *memmove(a,b,n) void *a; const void *b; size_t n; { bcopy(b,a,n); }
#else
#if defined(sun) && !defined(SYSV)
void *memmove(a,b,n) void *a, *b; int n; { bcopy(b,a,n); }
#endif
#endif

/* Data. */
XtAppContext app_context;
static char server[] = "server";

/* The builtin default GUI. */
char *defgui_text[] = {
#   include "ximtool.gui.h"
    NULL
};



/* MAIN -- XImtool main program.  This is the only ximtool routine containing
 * window system specific code.
 */
main (argc, argv)
int argc;
char *argv[];
{
	register XimDataPtr xim = &ximtool_data;
	register int i;
	XtPointer obm;
	char **sv_argv, *init_file = NULL, *str;
	int sv_argc, ncolors, base;


	/* Process the command line arguments.  Scan the arglist first to see
	 * if we're requesting help or a printout of the default GUI, if so 
	 * print these out and exit.
	 */
	if (argc > 1) {
	    if (strcmp (argv[1], "-help") == 0) {
      	        Usage ();
      	        exit (-1);

    	    } else if (strcmp (argv[1], "-defgui") == 0) {
	        register int i;
	        for (i=0;  defgui_text[i];  i++)
		    printf ("%s\n", defgui_text[i]);
	        exit (0);
	    }
	}


	/* Loop over the command line options and preprocess the ones that
	 * are widget/GUI resources we want to make available more easily.
	 * To do this we'll tweak the argument list so it appears to be a
 	 * "-xrm" resource setting, this means the X initialization code
	 * below will do all the real work.
	 */
	for (i=1; i < argc; i++) {

	    if (strcmp (argv[i], "-cmapName") == 0) {
		str = argv[++i];
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*cmapName:%s\0", str);

	    } else if (strcmp (argv[i], "-maxColors") == 0) {
		ncolors = atoi (argv[++i]);
		ncolors = max (32, min (201, ncolors));
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*maxColors:%d\0", ncolors);

	    } else if (strcmp (argv[i], "-basePixel") == 0) {
		base = atoi (argv[++i]);
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*basePixel:%d", base);

	    } else if (strcmp (argv[i], "-cmapInitialize") == 0) {
		str = argv[++i];
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*cmapInitialize:%s", str);

	    } else if (strcmp (argv[i], "-displayPanner") == 0) {
		str = argv[++i];
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*displayPanner:%s", str);

	    } else if (strcmp (argv[i], "-displayMagnifier") == 0) {
		str = argv[++i];
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*displayMagnifier:%s", str);

	    } else if (strcmp (argv[i], "-displayCoords") == 0) {
		str = argv[++i];
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*displayCoords:%s", str);

	    } else if (strcmp (argv[i], "-printConfig") == 0) {
		str = argv[++i];
		strcpy (argv[i-1], "-xrm\0");
		argv[i] = (char *) malloc (256);
		sprintf (argv[i], "XImtool*printConfig:%s", str);
	    }
	}

        /* Get local copy of argc and argv.  */
        if ((sv_argc = argc) > 0) {
            sv_argv = (char **) XtMalloc (argc * sizeof(char *));
 	    memmove (sv_argv, argv, argc * sizeof(char *)); 
        } else
            sv_argv = argv;

	/* Initialize applications context.  We don't use the top level
	 * shell created here, but we need to deal with it in order to
	 * get the application resources.  The object manager opens its own
	 * connection to the display server and manages a separate window
	 * hierarchy with its own top level shell.
	 */
	xim->toplevel = XtAppInitialize (&app_context, "XImtool",
	    (XrmOptionDescList) NULL, 0, &sv_argc, sv_argv,
	    (String *) NULL, (ArgList) NULL, 0);

	/* Free saved arglist. */
	free ((char *)sv_argv);

	/* Get application resources. */
	XtVaGetApplicationResources (xim->toplevel, xim,
	    resources, XtNumber(resources),
	    /* Add any resource overrides here */
	    NULL);

	/* Initialize the object manager. */
	xim->obm = obm = (XtPointer) ObmOpen (app_context, argc, argv);
	ObmAddCallback (obm, OBMCB_setGterm|OBMCB_preserve, xim_reset,
	    (XtPointer)xim);


	/* Loop over the command line options. The default xim structure
	 * should be defined at this point so the command options can be
	 * used to override them.
	 */
	for (i=1; i < argc; i++) {

	    /* Anything without a '-' is a file name to load at startup.
	     * Only use the first name defined.
	     */
	    if (argv[i][0] != '-') { 			    /* File name */
		if (!init_file) {
		    init_file = argv[i];
		    if (access (init_file, R_OK) < 0) {
			fprintf (stderr, "%s: File does not exist: '%s'\n", 
			    argv[0], init_file);
			exit (-1);
		    }
		}

	    } else if (strcmp (argv[i], "-gui") == 0) {
		xim->gui = argv[++i];

	    } else if (strcmp (argv[i], "-cmap1") == 0) {
		xim->userCMap1 = argv[++i];

	    } else if (strcmp (argv[i], "-cmap2") == 0) {
		xim->userCMap2 = argv[++i];

	    } else if (strcmp (argv[i], "-cmapDir1") == 0) {
		xim->userCMapDir1 = argv[++i];

	    } else if (strcmp (argv[i], "-cmapDir1") == 0) {
		xim->userCMapDir2 = argv[++i];

	    } else if (strcmp (argv[i], "-imtoolrc") == 0) {
		xim->imtoolrc = argv[++i];

	    } else if (strcmp (argv[i], "-memModel") == 0) {
		xim->memModel = argv[++i];

	    } else if (strcmp (argv[i], "-config") == 0) {
		xim->def_config = atoi (argv[++i]);

	    } else if (strcmp (argv[i], "-nframes") == 0) {
		i++;
		xim->def_nframes = min (MAX_FRAMES, atoi (argv[i]));

	    } else if (strcmp (argv[i], "-tile") == 0) {
		xim->tileFrames++;

	    } else if (strcmp (argv[i], "-invert") == 0) {
		xim->invert++;

	    } else if (strcmp (argv[i], "-fifo") == 0) {
                xim->input_fifo = malloc (strlen (argv[++i]+2));
                xim->output_fifo = malloc (strlen (argv[i]+2));
		sprintf (xim->input_fifo, "%si", argv[i]);
		sprintf (xim->output_fifo, "%so", argv[i]);

	    } else if (strcmp (argv[i], "-port") == 0) {
		if (xim->port != 0 )
		    xim->port = atoi (argv[++i]);
		else 
		    i++;

	    } else if (strcmp (argv[i], "-unix") == 0) {
		if (strcmp ("none", xim->input_fifo) )
		    xim->unixaddr = argv[++i];
		else 
		    i++;

	    } else if (strcmp (argv[i], "-fifo_only") == 0) {
		xim->unixaddr = "none";
		xim->port = 0;

	    } else if (strcmp (argv[i], "-inet_only") == 0 ||
	        strcmp (argv[i], "-port_only") == 0 ) {
		    xim->input_fifo = "";
		    xim->unixaddr = "none";

	    } else if (strcmp (argv[i], "-unix_only") == 0) {
		xim->input_fifo = "";
		xim->port = 0;


	    /* Skip any standard X toolkit flags, they're handled above. 
	     */
	    } else if (strcmp (argv[i], "-bg") == 0) {
		i++;
	    } else if (strcmp (argv[i], "-fg") == 0) {
		i++;
	    } else if (strcmp (argv[i], "-iconic") == 0) {
		   ;
	    } else if (strcmp (argv[i], "-display") == 0) {
		i++;
	    } else if (strcmp (argv[i], "-geometry") == 0) {
		i++;
	    } else if (strcmp (argv[i], "-title") == 0) {
		i++;
	    } else if (strcmp (argv[i], "-xrm") == 0) {
		i++;

	    } else {
		fprintf (stderr, "Unrecognized flag '%s'\n", argv[i]);
      	        Usage();
      	        exit (-1);
	    }
	}


	/* Initialize the ximtool/obm client code. */
	xim_clientOpen (xim);

	/* Load the Ximtool GUI.  If the GUI name is "default" the builtin
	 * default GUI is used.  This is stored as an array of text lines,
	 * which we must append newlines to and concatenate together to
	 * form the GUI message.
	 */
	if (strcmp (xim->gui, "default") == 0 ||
	    (ObmDeliverMsgFromFile (obm, server, xim->gui) != 0)) {

	    register char *ip, *op;
	    char *message;
	    int i;

	    message = (char *) malloc (204800);
	    for (i=0, op=message;  ip = defgui_text[i];  i++) {
		while (*ip)
		    *op++ = *ip++;
		*op++ = '\n';
	    }
	    *op++ = '\0';

	    ObmDeliverMsg (obm, server, message);
	    free ((char *)message);

	}

	/* Activate the GUI. */
	ObmActivate (obm);

	/* Initialize the frame buffers and graphics pipeline. */
	xim_initialize (xim, xim->def_config, xim->def_nframes, 1);

	/* Listen for a client connection. */
	xim_iisopen (xim);

	/* Initialize the hardcopy option and printer configuration. */
	xim_initPrinterOps (xim);

	/* Open the file Load/Save structures. */
	xim_initLoad (xim);
	xim_initSave (xim);

	/* Load a file at startup if it was defined. */
	if ( init_file != NULL )
	    xim_loadFile (xim, init_file, 1);

	/* EXECUTE */
	XtAppMainLoop (app_context);

	xim_shutdown (xim);
}


/* XIM_SHUTDOWN -- Terminate ximtool.
 */
xim_shutdown (xim)
register XimDataPtr xim;
{
	eps_close (xim->psim);
	xim_loadClose (xim);
	xim_saveClose (xim);
	xim_clientClose (xim);
	xim_iisclose (xim);
	xim_close (xim);
	ObmClose (xim->obm);
	exit (0);
}


/* XIM_ADDINPUT -- Register a procedure to be called when there is input
 * to be processed on the given input source.  The ximtool code doesn't
 * talk to X directly so we need to provide this interface routine.
 */
XtPointer
xim_addInput (xim, input, proc, client_data)
register XimDataPtr xim;
int input;
void (*proc)();
XtPointer client_data;
{
	return ((XtPointer) XtAppAddInput (app_context, input,
	    (XtPointer)XtInputReadMask, *proc, client_data));
}


/* XIM_REMOVEINPUT -- Remove a callback previously posted with xim_addInput.
 */
void
xim_removeInput (xim, id)
register XimDataPtr xim;
XtPointer id;
{
	XtRemoveInput ((XtInputId)id);
}


/* USAGE -- Print a list of command-line options.
 */
Usage ()
{
    fprintf (stderr, "Usage:\n\n");
    printoption ("    ximtool");
    printoption ("[-basePixel <num>]");   	/* base cmap pixel 	*/
    printoption ("[-cmap1 <file>]");	   	/* User cmap 1 		*/
    printoption ("[-cmap2 <file>]");    	/* User cmap 2 		*/
    printoption ("[-cmapDir1 <dir>]");   	/* User cmapDir 1 	*/
    printoption ("[-cmapDir1 <dir>]");   	/* User cmapDir 2 	*/
    printoption ("[-cmapInitialize <bool>]");   /* initialize colormap 	*/
    printoption ("[-cmapName <name>]");   	/* colormap name 	*/
    printoption ("[-config <num>]");	   	/* initial config 	*/
    printoption ("[-defgui]");		   	/* Print default GUI 	*/
    printoption ("[-displayPanner <bool>]");   	/* display panner box 	*/
    printoption ("[-displayMagnifier <bool>]");	/* display magnifier    */
    printoption ("[-displayCoords <bool>]");   	/* display coords box 	*/
    printoption ("[-fifo <pipe>]");	    	/* fifo pipe 		*/
    printoption ("[-fifo_only]");  		/* use fifo only 	*/
    printoption ("[-gui <file>]");	    	/* GUI file 		*/
    printoption ("[-help]");		   	/* Print help 		*/
    printoption ("[-imtoolrc <file>]");   	/* fbconfig file 	*/
    printoption ("[-inet_only | -port_only]"); 	/* use inet only 	*/
    printoption ("[-invert]");       	   	/* invert colormap 	*/
    printoption ("[-maxColors <num>]");   	/* # of colors 		*/
    printoption ("[-memModel <type>]");   	/* memory model	 	*/
    printoption ("[-nframes <num>]");    	/* # of frames 		*/
    printoption ("[-port <num>]");	    	/* inet port 		*/
    printoption ("[-printConfig <name>]");     	/* printer config file 	*/
    printoption ("[-tile]");       		/* tile frames 		*/
    printoption ("[-unix <name>]");	    	/* unix socket 		*/
    printoption ("[-unix_only]");  		/* use unix only 	*/
    printoption ("[<file>]");  		   	/* file to load 	*/
    fprintf (stderr,"\n");
}


/* PRINTOPTION -- Pretty-print an option string.
 */
static int cpos = 0;
printoption(st)
char 	*st;
{
  	if (strlen(st) + cpos > 78) {
    	    fprintf (stderr,"\n\t");
    	    cpos = 8;
  	}
  	fprintf (stderr,"%s ",st);
  	cpos = cpos + strlen(st) + 1;
}
