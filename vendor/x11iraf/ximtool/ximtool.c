#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/Error.h>
#include <Obm.h>

#define XIMTOOL_MAIN
#include "ximtool.h"

/* The X11IRAF version. */
char *ximtool_version[] = {
#   include "../version.h"
    NULL
};


/*
 * XIMTOOL -- X11 based image display server and standalone image display
 * client.  This program is based on the IRAF prototype widget server (object
 * manager library or OBM) and the gterm-image widget.
 */

/* Compatibility hacks. */
#ifdef AUX
void *memmove(a,b,n) void *a; const void *b; size_t n; { bcopy(b,a,n); }
#else
#if defined(sun) && !defined(SVR4)
void *memmove(a,b,n) void *a; void *b; size_t n; { bcopy(b,a,n); }
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


#ifdef AUX
#define SIGFUNC sigfunc_t
#else
typedef void  (*SIGFUNC)();
#endif

void	xim_onsig();


/* MAIN -- XImtool main program.  This is the only ximtool routine containing
 * window system specific code.
 */
main (argc, argv)
int argc;
char *argv[];
{
	register XimDataPtr xim = &ximtool_data;
	register int i;
        register IsmModule ism;

	Screen *screen;
	Visual *visual;
	Widget toplevel;
	XtPointer obm;
	char **sv_argv, *init_file = NULL, *str;
	int sv_argc, ncolors, base;
	int depth, tile = 0;

	extern IsmModule ismNameToPtr();
	int 	xerror(), xioerror();


	/* Process the command line arguments.  Scan the arglist first to see
	 * if we're requesting help or a printout of the default GUI, if so 
	 * print these out and exit.
	 */
	if (argc > 1) {
	    if (strcmp (argv[1], "-help") == 0) {
      	        Usage ();
      	        exit (1);

    	    } else if (strcmp (argv[1], "-version") == 0) {
		printf ("Version:  %s\n", ximtool_version[0]);
      	        exit (1);

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
	xim->toplevel = toplevel = XtAppInitialize (&app_context, "XImtool",
	    (XrmOptionDescList) NULL, 0, &sv_argc, sv_argv,
	    (String *) NULL, (ArgList) NULL, 0);

	/* Free saved arglist. */
	free ((char *)sv_argv);

	/* Get application resources. */
	XtVaGetApplicationResources (xim->toplevel, xim,
	    resources, XtNumber(resources),
	    /* Add any resource overrides here */
	    NULL);

	/* Check to see if we have the correct visual to proceed, if not
	 * shut down more gracefully and informatively than the BadMatch
	 * error from X that awaits us.
	 */

#ifdef PSEUDOCOLOR_ONLY
	screen = XtScreen (toplevel);
    	visual = DefaultVisualOfScreen(screen);
    	depth = DefaultDepthOfScreen(screen);
    	if (depth != 8 || visual->class != PseudoColor)
	    xim_badVisual (depth, visual->class);
#endif



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
		    if (access (init_file, F_OK) < 0) {
			fprintf (stderr, "%s: File does not exist: '%s'\n", 
			    argv[0], init_file);
			exit (1);
		    } else if (access (init_file, R_OK) < 0) {
			fprintf (stderr, 
			    "%s: File doesn't have read permission: '%s'\n", 
			    argv[0], init_file);
			exit (1);
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

	    } else if (strcmp (argv[i], "-cmapDir2") == 0) {
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
                tile = ++xim->tileFrames;

	    } else if (strcmp (argv[i], "-invert") == 0) {
		xim->invert++;

	    } else if (strcmp (argv[i], "-fifo") == 0) {
		if (strcmp ("none", argv[i+1]) == 0) {
		    xim->input_fifo = "none";
		} else {
                    xim->input_fifo = malloc (strlen(argv[++i])+2);
                    xim->output_fifo = malloc (strlen(argv[i])+2);
		    sprintf (xim->input_fifo, "%si", argv[i]);
		    sprintf (xim->output_fifo, "%so", argv[i]);
		}

	    } else if (strcmp (argv[i], "-port") == 0) {
		xim->port = atoi (argv[++i]);

	    } else if (strcmp (argv[i], "-nports") == 0) {
		xim->nports = atoi (argv[++i]);

	    } else if (strcmp (argv[i], "-unix") == 0) {
		xim->unixaddr = argv[++i];

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

	    } else if (strcmp (argv[i], "-ismdev") == 0) {
		if (strcmp ("none", xim->ism_addr) )
		    xim->ism_addr = argv[++i];
		else 
		    i++;


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
      	        exit (1);
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

	    message = (char *) malloc (409600);
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
	xim->tileFrames = tile;
	xim_initialize (xim, xim->def_config, xim->def_nframes, 1);

	/* Listen for a client connection. */
	xim_iisOpen (xim);
	xim_ismOpen (xim);

	/* Display a pretty logo. */
	xim_displayLogo (xim);

	/* Initialize the hardcopy option and printer configuration. */
	xim_initPrinterOps (xim);

	/* Open the file Load/Save structures. */
	xim_initLoad (xim);
	xim_initSave (xim);

	/* Load a file at startup if it was defined. */
	if ( init_file != NULL )
	    xim_loadFile (xim, init_file, 1);

        /* Lookup the ISM command for the WCSPIX task and start it.
         */
        if ((ism = ismNameToPtr ("wcspix"))) {
            system (ism->command);
	    ism_message (ism, "wcspix", "initialize");
	}

        XSetErrorHandler(xerror);
        XSetIOErrorHandler(xioerror);


/*	signal (SIGINT, (SIGFUNC)xim_onsig);*/

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
	xim_iisClose (xim);
	xim_ismClose (xim);
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
    printoption ("[-basePixel <num>]");   	/* base cmap pixel        */
    printoption ("[-cmap1 <file>]");	   	/* User cmap 1            */
    printoption ("[-cmap2 <file>]");    	/* User cmap 2            */
    printoption ("[-cmapDir1 <dir>]");   	/* User cmapDir 1         */
    printoption ("[-cmapDir2 <dir>]");   	/* User cmapDir 2         */
    printoption ("[-cmapInitialize <bool>]");  	/* initialize colormap    */
    printoption ("[-cmapName <name>]");   	/* colormap name          */
    printoption ("[-config <num>]");	   	/* initial config         */
    printoption ("[-defgui]");		   	/* Print default GUI      */
    printoption ("[-displayPanner <bool>]");   	/* display panner box     */
    printoption ("[-displayMagnifier <bool>]");	/* display magnifier box  */
    printoption ("[-displayCoords <bool>]");   	/* display wcs coords box */
    printoption ("[-fifo <pipe>]");	    	/* fifo pipe              */
    printoption ("[-fifo_only]");  		/* use fifo only          */
    printoption ("[-gui <file>]");	    	/* GUI file               */
    printoption ("[-help]");		   	/* Print help             */
    printoption ("[-imtoolrc <file>]");   	/* fbconfig file          */
    printoption ("[-inet_only | -port_only]"); 	/* use inet only          */
    printoption ("[-invert]");       	   	/* invert colormap        */
    printoption ("[-ismdev <dev>]");      	/* ISM device template    */
    printoption ("[-maxColors <num>]");   	/* # of colors            */
    printoption ("[-memModel <type>]");   	/* memory model           */
    printoption ("[-nframes <num>]");    	/* # of frames            */
    printoption ("[-port <num>]");	    	/* inet port              */
    printoption ("[-printConfig <name>]");     	/* printer config file    */
    printoption ("[-tile]");       		/* tile frames            */
    printoption ("[-unix <name>]");	    	/* unix socket            */
    printoption ("[-unix_only]");  		/* use unix only          */
    printoption ("[<file>]");  		   	/* file to load           */
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


/* xim_badVisual --  A bad X visual has been detected for the screen which
 * will cause us to crash with a BadMatch error.  Instead, abort with a
 * more informative message so the user can correct the visual.
 */
xim_badVisual (depth, class)
int	depth;
int	class;
{
    fprintf (stderr, 
	"\nERROR: Detected incorrect X visual:  depth=%d class=", depth);
        switch (class) {
        case StaticGray:    fprintf (stderr, "StaticGray\n")  ; break;
        case GrayScale:     fprintf (stderr, "GrayScale\n")   ; break;
        case StaticColor:   fprintf (stderr, "StaticColor\n") ; break;
        case PseudoColor:   fprintf (stderr, "PseudoColor\n") ; break;
        case TrueColor:     fprintf (stderr, "TrueColor\n")   ; break;
        case DirectColor:   fprintf (stderr, "DirectColor\n") ; break;
        default: 	    fprintf (stderr, "DirectColor\n") ; break;
        }

    fprintf (stderr,  "\n");
    fprintf (stderr, 
	"XImtool currently requires an 8-bit PseudoColor visual in order\n");
    fprintf (stderr, 
	"to operate properly. Unfortunately the only workaround for this\n");
    fprintf (stderr, 
	"at present is to start an 8-bit server using commands such as\n\n");
    fprintf (stderr, 
        "    %% startx -- -bpp 8\t\t\t\t\t    # XFree86 V3.x\n");
    fprintf (stderr, 
        "    %% startx -- -depth 8\t\t\t\t    # XFree86 V4.x\n");
    fprintf (stderr, 
        "    %% Xsun :0 -dev /dev/fb defclass PseudoColor defdepth 8  # Sun\n");
    fprintf (stderr, 
        "    %% Xdec -vclass0 PseudoColor\t\t\t\t    # Digital Unix\n");
    fprintf (stderr, 
	"\nThe actual commands used will vary depending on the platform,\n");
    fprintf (stderr, 
	"window manager/desktop used, and in some cases video hardware.\n");
    fprintf (stderr, 
	"See the Xserver(1) and xinit(1) man page for details. Users should\n");
    fprintf (stderr, 
	"also contact IRAF site support (iraf@noao.edu) with any questions\n");
    fprintf (stderr, 
	"or problems.\n\n");

    exit (1);
}



#define ERROR_XERROR    83      /* xerror: XError event */
#define ERROR_XIOERROR  84      /* xioerror: X I/O error */

/* XERROR -- Handle an XLIB server error.  A standard X error message is
 * printed and then the program either dumps core, exits, or ignores the error,
 * depending upon the value of the environment variable XGXERROR, if defined.
*/
/*ARGSUSED*/
xerror (display, event)
Display *display;
register XErrorEvent *event;
{
        static char *envvar = "XGXERROR";
        static int nerrs = 0;
        extern char *getenv();
        char fname[128];
        char *action;
        int pid;

        fprintf (stderr, "ximtool: warning, error event received:\n");
        (void) XmuPrintDefaultErrorMessage (display, event, stderr);
        if (nerrs++ > 50)
            exit (ERROR_XERROR);

        if (action = getenv (envvar)) {
            if (strcmp (action, "dumpcore") == 0) {
                if ((pid = fork()) >= 0) {
                    if (pid) {
                        fprintf (stderr, "dumping core... ");
                        fflush (stderr);
                        sprintf (fname, "core.%d", pid);
                        wait(NULL); rename ("core", fname);
                        fprintf (stderr, "core file core.%d written\n", pid);
                       fflush (stderr);
                    } else
                        kill (getpid(), 6);
                } else
                    fprintf (stderr, "fork failed, no core dump produced\n");
           } else if (strcmp (action, "exit") == 0) {
                fprintf (stderr, "program terminated\n");
                exit (ERROR_XERROR);
            } else
                fprintf (stderr, "%s: unknown action %s\n", envvar, action);
        }
	fflush (stderr);

        return (0);
}


/*ARGSUSED*/
xioerror(dpy)
Display *dpy;
{
    char *SysErrorMsg();

    (void) fprintf (stderr,
        "ximtool: fatal IO error %d (%s) or KillClient on X server \"%s\"\r\n",
        errno, SysErrorMsg (errno),
        DisplayString (dpy));

    exit (ERROR_XIOERROR);
}

void xt_error(message)
    String message;
{
    (void) fprintf (stderr, "ximtool Xt error: %s\n", message);
    exit (1);
}


char *SysErrorMsg (n)
    int n;
{
    return((n >= 0) ? (char *)strerror(n) : "unknown error");
}


/* XIM_ONSIG -- Catch interrupt and shutdown gracefully.
 */
void
xim_onsig (sig, code, scp)
int     sig;                    /* signal which was trapped     */
int     *code;                  /* not used */
int     *scp;                   /* not used */
{
        xim_shutdown (&ximtool_data);
}

