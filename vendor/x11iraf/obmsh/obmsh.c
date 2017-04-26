#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Obm.h>

/*
 * OBMSH -- GUI shell using the OBM (Object Manager) library.  The GUI script
 * to be executed may be read from either the standard input or a file.
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

static void deactivate();
static void output();


/* MAIN -- OBMSH main program.
 */
main (argc, argv)
int argc;
char *argv[];
{
	Widget toplevel;
	XtPointer obm;
	char **sv_argv;
	int sv_argc;
	char *fname;

        /* Get local copy of argc and argv. */
        if ((sv_argc = argc) > 0) {
            sv_argv = (char **) XtMalloc (argc * sizeof(char *));
            memmove (sv_argv, argv, argc * sizeof(char *));
        } else
            sv_argv = argv;

	/* Initialize applications context. */
	toplevel = XtAppInitialize (&app_context, "OBMsh",
	    (XrmOptionDescList) NULL, 0, &sv_argc, sv_argv,
	    (String *) NULL, (ArgList) NULL, 0);

	/* Free saved arglist. */
	free ((char *)sv_argv);
	if (!toplevel)
	    exit (1);

	/* Initialize the object manager. */
	if (!(obm = (XtPointer) ObmOpen (app_context, argc, argv)))
	    exit (2);
	ObmAddCallback (obm, OBMCB_deactivate|OBMCB_preserve, deactivate, obm);
	ObmAddCallback (obm, OBMCB_clientOutput|OBMCB_preserve, output, obm);

	/* Open and execute the GUI file if there was one, otherwise read
	 * the GUI from the standard input.
	 */
	if (argc == 2) {
	    if (access (fname=argv[1],0) != 0) {
		fprintf (stderr, "cannot open %s\n", fname);
		exit (3);
	    }
	    if (ObmDeliverMsgFromFile (obm, server, fname) != 0) {
		fprintf (stderr, "error executing GUI %s\n", fname);
		exit (4);
	    }
	} else {
	    register int ch;
	    register char *op;
	    char *message = (char *) XtMalloc (1024000);

	    for (op=message;  (ch = getc(stdin)) != EOF;  )
		*op++ = ch;
	    *op++ = '\0';

	    ObmDeliverMsg (obm, server, message);
	    free (message);
	}

	/* Activate the GUI. */
	ObmActivate (obm);

	/* EXECUTE */
	XtAppMainLoop (app_context);
}


/* DEACTIVATE -- The deactivate callback is called when deactivate is executed
 * in the GUI.
 */
static void
deactivate (obm, toplevel)
XtPointer obm;
Widget toplevel;
{
	ObmClose (obm);
	exit (0);
}


/* OUTPUT -- The output callback is called when the GUI sends data or requests
 * to the "client" object, which is this routine in the case of obmsh.
 */
static void
output (obm, tcl, objname, key, string)
XtPointer obm;
XtPointer tcl;			/* not used */
char *objname;			/* not used */
int key;
char *string;
{
	if (key == 'q' || key == 'Q') {
	    ObmClose (obm);
	    exit (0);
	} else {
	    printf ("key=%c(%03o) command = `%s'\n", key, key, string);
	    fflush (stdout);
	}
}
