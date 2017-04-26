#include <stdio.h>
#include <sys/errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

/* #ifndef AUX */
#include <fcntl.h>
/* #endif */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>	
#include <X11/Xaw/Paned.h>	
#include <X11/Xaw/Viewport.h>	
#include <X11/Xaw/List.h>	
#include <X11/Xaw/Form.h>	
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>	
#include <X11/Xaw/AsciiText.h>	
#include <X11/Xaw/Command.h>	
#include <X11/Xaw/Cardinals.h>	

#include "appres.h"
#include "xtapemon.h"
#include "classnames.h"
#include "patchlevel.h"

/*
 * XTAPEMON -- X window system based tape monitoring utility.
 *
 * Monitors the raw ascii status output of the iraf tape driver and
 * maintains a graphical display of the tape status in a window.
 *
 */

/* Local definitions.
 */
#define	MAXCONN		0
#define	MAXPORTS	2
#define	ERR		(-1)
#define	EOS		'\0'
#define SZ_FNAME	256
#define SZ_LINE		512
#define SZ_MSGBUF	2048

extern	errno;

/* Functions defined in this file.
 */
int main();
void doPendingEvents();
void initWidgetsFromString();
void setText(), setLabel();
void status0(), status1(), status2();
void fail0(), fail1();

static int portSetup(), portOpen();
static void initGraphics(), initWidgets();
static void selectItem(), portClose();
static void syntax(), appendText();
static void Quit();
static XtInputCallbackProc connectToClient(), readClientData();

/* Global graphics data.
 */
Display *display;
Screen *screen;
Window root;
int server_socket;
int server_port;

/* Global widget data.
 */
XtAppContext appContext;
Widget toplevel;

static Widget statusText, messages, quitButton, recordLabel;
static Widget devtypeText, tapetypeText, tapesizeText, tapeusedText;
static Widget acmodeText, densityText, blksizeText, fileText, recordText;
static Widget recsizeText, nfilesText;

/* Other global data.
 */
char *progname;
AppResources appResources;

/* Non-widget resources obtained from resource manager.
 */
static XtResource resources[] = {
    { "widgets", "Widgets", XtRString, sizeof(String),
      XtOffset(AppResources *,widgets), XtRImmediate, "" },
    { "port", "Port", XtRInt, sizeof(int),
      XtOffset(AppResources *,port), XtRImmediate, (XtPointer)5138 },
    { "alternate", "Alternate", XtRInt, sizeof(int),
      XtOffset(AppResources *,alternate), XtRImmediate, (XtPointer)0 },
    { "debug", "Debug", XtRInt, sizeof(int),
      XtOffset(AppResources *,debug), XtRImmediate, (XtPointer)0 },
};

/* Non-widget resources set on command line.
 */
static XrmOptionDescRec options[] = {
    { "-port",  ".port",	 XrmoptionSepArg, (XtPointer)"5138" },
    { "-a",     ".alternate",	 XrmoptionNoArg,  (XtPointer)"1" },
    { "-d",     ".debug",	 XrmoptionNoArg,  (XtPointer)"1" },
};

/* Widget and non-widget resources if the application defaults file can't
 * be found.  Generated automatically from XTapemon.ad by "ad2c".
 */
static String fallbackResources[] = {
#include "XTapemon.ad.h"
    NULL
};

/* Compatibility hacks. */
#ifdef AUX
void *memmove(a,b,n) void *a; const void *b; size_t n; { bcopy(b,a,n); }
#else
#if defined(sun) && !defined(SYSV)
void *memmove(a,b,n) void *a, *b; int n; { bcopy(b,a,n); }
#endif
#endif

/*
 * The main program.
 */
main(argc,argv)
int argc;
char **argv;
{
    char buf[80];
    int port;

    progname = argv[0];
    initGraphics(&argc,argv);
    if (argc > 1) {
	syntax(argc,argv);
	XtDestroyApplicationContext(appContext);
	exit(1);
    }
    initWidgets();
    XtRealizeWidget(toplevel);
    /* Set window title to indicate version */
    sprintf (buf, "xtapemon %d.%d - IRAF Tape Monitor Utility",
	xtapemonMajorVersion, xtapemonMinorVersion);
    XStoreName(display,XtWindow(toplevel),buf);
    sprintf(buf, "Welcome to xtapemon %d.%d",
	xtapemonMajorVersion, xtapemonMinorVersion);
    status0(buf);

    /* Prepare to receive a connection, set up Xt callback to accept a
     * client connection when one arrives.
     */
    port = appResources.port + appResources.alternate;
    if ((server_socket = portSetup(port)) >= 0)
	XtAppAddInput (appContext, server_socket, (XtPointer)XtInputReadMask,
	    (XtInputCallbackProc)connectToClient, (XtPointer)server_socket);

    /* Identify port in use. */
    sprintf (buf, "ready on port %s (%d)", 
	(server_port == appResources.port) ? "A" : "B", server_port);
    setText (messages, buf);

    /* do it */
    XtAppMainLoop(appContext);
    /*NOTREACHED*/
}


void
doPendingEvents()
{
    while (XtAppPending(appContext))
	XtAppProcessEvent(appContext,XtIMAll);
}

static void
initGraphics(argcp,argv)
int *argcp;
char **argv;
{
    toplevel = XtAppInitialize(&appContext, "XTapemon",
			       options, XtNumber(options),
			       argcp,argv,fallbackResources,NULL,ZERO);
    initConverters(appContext);
    /* XawSimpleMenuAddGlobalActions(appContext); */
    /* XtAppAddActions(appContext,actionTable,XtNumber(actionTable)); */
    XtGetApplicationResources(toplevel,(XtPointer)&appResources,
                              resources,XtNumber(resources),NULL,ZERO);
    display = XtDisplay(toplevel);
    screen = XtScreen(toplevel);
    root = RootWindowOfScreen(screen);
}


/* initWidgets -- Initialize the widgets given in the .widgets resource,
 * check for required widgets, and set globals vars.
 */
static void
initWidgets()
{
    initWidgetsFromString(appResources.widgets,".widgets");

    /* set globals for optional widgets */
    statusText = XtNameToWidget (toplevel, "*statusText");
    messages = XtNameToWidget (toplevel, "*messages");
    quitButton = XtNameToWidget (toplevel, "*quitButton");
    devtypeText = XtNameToWidget (toplevel, "*devtypeText");
    tapetypeText = XtNameToWidget (toplevel, "*tapetypeText");
    tapesizeText = XtNameToWidget (toplevel, "*tapesizeText");
    tapeusedText = XtNameToWidget (toplevel, "*tapeusedText");
    acmodeText = XtNameToWidget (toplevel, "*acmodeText");
    densityText = XtNameToWidget (toplevel, "*densityText");
    blksizeText = XtNameToWidget (toplevel, "*blksizeText");
    fileText = XtNameToWidget (toplevel, "*fileText");
    recordText = XtNameToWidget (toplevel, "*recordText");
    recordLabel = XtNameToWidget (toplevel, "*recordLabel");
    recsizeText = XtNameToWidget (toplevel, "*recsizeText");
    nfilesText = XtNameToWidget (toplevel, "*nfilesText");

    /* Set up Quit button callback. */
    XtAddCallback (quitButton, XtNcallback, Quit, (XtPointer)NULL);
}

static void
Quit(w, call_data, client_data)
Widget w;
XtPointer call_data, client_data;
{
    XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
    exit(0);
}

#define ISSPACE(c)	((c) == ' ' || (c) == '\t' || (c) == '\n')
#define NUL		'\0'

/* initWidgetsFromString -- Create the widgets specified in resourceStr as
 * "parent class name" triples. The resourceName is used for error messages.
 */
void
initWidgetsFromString(resourceStr,resourceName)
char *resourceStr,*resourceName;
{
    char name[32],class[32],parent[256];
    char *s,*t;
    Boolean isShell;
    WidgetClass wc;
    Widget pw;

    if ((s=resourceStr) == NULL)
	fail1("no widgets specified in %s resource!",resourceName);
    while (*s) {
	/* skip leading whitespace */
        while (ISSPACE(*s))
            s += 1;
	if (!*s)
	    break;
	/* Gather the parent widget name */
        t = parent;
        while (*s && !ISSPACE(*s))
            *t++ = *s++;
        *t = NUL;
	/* skip whitespace */
        while (ISSPACE(*s))
            s += 1;
	if (!*s)
	    fail1("missing widget class and name in %s resource",resourceName);
	/* Gather the class name */
        t = class;
        while (*s && !ISSPACE(*s))
            *t++ = *s++;
        *t = NUL;
	/* skip whitespace */
        while (ISSPACE(*s))
            s += 1;
	if (!*s)
	    fail1("missing widget name in %s resource",resourceName);
	/* Gather the widget's name */
        t = name;
        while (*s && !ISSPACE(*s))
            *t++ = *s++;
        *t = NUL;
	/* convert class name to WidgetClass */
        if ((wc=classNameToWidgetClass(class,&isShell)) == NULL)
	    fail1("can't convert string \"%s\" to widgetClass",class);
	/* convert parent name to Widget */
	if (strcmp(parent,"toplevel") == 0)
	    pw = toplevel;
	else if ((pw=XtNameToWidget(toplevel,parent)) == NULL)
	    fail1("can't convert string \"%s\" to widget",parent);
	/* finally create the widget */
	if (isShell)
            (void)XtCreatePopupShell(name,wc,pw,NULL,ZERO);
        else
            (void)XtCreateManagedWidget(name,wc,pw,NULL,ZERO);
    }
}


/*
 * Client actions.
 * --------------------------
 */

/* connectToClient -- Called when a client has attempted a connection on
 * the xtapemon socket.  Accept the connection and open a file pointer on
 * the status output stream of the client.
 */
static XtInputCallbackProc
connectToClient (client_data, source, id)
XtPointer client_data;
int *source;
XtInputId *id;
{
    int fd;

    setText (statusText, "connecting to client...");
    if ((fd = portOpen ((int)*source)) < 0)
	setText (statusText, "connection failed");
    else {
	setText (statusText, "connection established");
	if (fcntl (fd, F_SETFD, O_RDWR|O_NDELAY) < 0) {
	    char buf[80];
	    sprintf (buf, "fcntl failed, errno=%d", errno);
	    setText (statusText, buf);
	    portClose (fd);
	} else {
	    /* Enable the following to prohibit multiple clients.
	     * XtRemoveInput (*id);
	     */
	    XtAppAddInput (appContext, fd, (XtPointer)XtInputReadMask,
		(XtInputCallbackProc)readClientData, (XtPointer)NULL);
	}
    }

    doPendingEvents();
}


/* readClientData -- Called when there is client data to be read and
 * displayed.
 */
static XtInputCallbackProc
readClientData (client_data, source, id)
XtPointer client_data;
int *source;
XtInputId *id;
{
    register int fd = *source;
    register char *ip, *op;
    static char msg[SZ_MSGBUF];
    static int nleft, tapesize, blksize;
    char word[SZ_LINE], value[SZ_LINE];
    char obuf[SZ_LINE], iodev[SZ_FNAME];
    int nchars, newline, maxch, ival;
    char *start;

    start = msg + nleft;
    maxch = SZ_MSGBUF - nleft;
    nleft = 0;

    /* Read a block of text from the input socket and process each line to
     * the monitor window.  Messages may be batched for efficiency, i.e.,
     * the text block may contain several lines.  Since socket i/o is stream
     * based we can't assume that lines are not broken over read transfer
     * boundaries, so it is necessary to save any partial line at the end of
     * a read and join the line when the next data block is read.
     */
    nchars = read (fd, start, maxch);
    if (nchars >= 0) {
	if (appResources.debug)
	    write (2, start, nchars);
	*(start + nchars) = EOS;
    }
    ip = msg;

    while (*ip) {
	start = ip;
	newline = 0;

	/* Get first whitespace delimited word. */
	for (op=word;  *ip && !isspace(*ip);  )
	    *op++ = *ip++;
	*op = EOS;
	while (*ip && *ip == ' ')
	    ip++;

	/* Anything other than "keyword = value" is a message and is sent
	 * to the message window.  Messages are newline delimited.  The
	 * newline is required to delimit records.  To include a newline
	 * in a message one must pass an explicit \n, e.g, "foo\\n\n".
	 */
	if (*ip == '=') {
	    for (ip++, op=value;  *ip;  ) {
		if (*ip == '\n') {
		    ip++;
		    newline++;
		    break;
		} else
		    *op++ = *ip++;
	    }
	    *op = EOS;
	} else {
	    for (ip=start, op=value;  *ip;  ) {
		if (*ip == '\n') {
		    ip++;
		    newline++;
		    break;
		} else if (*ip == '\\' && *(ip+1) == 'n') {
		    *op++ = '\n';
		    ip += 2;
		} else
		    *op++ = *ip++;
	    }
	    *op = EOS;
	    if (newline) {
		appendText (value);
		continue;
	    }
	}

	/* Save any partial line for next time. */
	if (!newline) {
	    strcpy (msg, start);
	    nleft = strlen (msg);
	    break;
	}

	/* Set the value of a keyword. */
	if (word[0] == EOS) {
	    continue;
	} else if (strcmp (word, "iodev") == 0) {
	    strcpy (iodev, value+1);
	} else if (strcmp (word, "host") == 0) {
	    sprintf (obuf, "connected to %s,%s on port %s (%d)",
		value+1, iodev,
		(server_port == appResources.port) ? "A" : "B", server_port);
	    setText (statusText, obuf);

	} else if (strcmp (word, "devtype") == 0) {
	    setText (devtypeText, value);
	} else if (strcmp (word, "tapetype") == 0) {
	    setText (tapetypeText, value);

	} else if (strcmp (word, "tapesize") == 0) {
	    ival = atoi (value);
	    sprintf (obuf, " %d.%02d Mb", ival / 1000, ((ival%1000) + 5) / 10);
	    setText (tapesizeText, obuf);
	    tapesize = ival;

	} else if (strcmp (word, "tapeused") == 0) {
	    ival = atoi (value);
	    sprintf (obuf, " %d.%02d Mb (%02d%%)",
		ival / 1000, ((ival % 1000) + 5) / 10,
		(ival / tapesize) * 100);
	    setText (tapeusedText, obuf);

	} else if (strcmp (word, "blksize") == 0) {
	    ival = atoi (value);
	    if (ival != blksize) {
		setLabel (recordLabel, ival ? " Block:" : "Record:");
		blksize = ival;
	    }
	    setText (blksizeText, !strcmp(value," 0") ? " variable" : value);

	} else if (strcmp (word, "acmode") == 0) {
	    setText (acmodeText, value);
	} else if (strcmp (word, "density") == 0) {
	    setText (densityText, value);
	} else if (strcmp (word, "file") == 0) {
	    setText (fileText, value);
	} else if (strcmp (word, "nfiles") == 0) {
	    setText (nfilesText, !strcmp(value," 0") ? " unknown" : value);
	} else if (strcmp (word, "record") == 0) {
	    setText (recordText, value);
	} else if (strcmp (word, "recsize") == 0) {
	    setText (recsizeText, value);
	} else {
	    sprintf (obuf, "%s = %s", word, value);
	    appendText (obuf);
	}
    }

    /* Close connection, and ready to receive further connection. */
    if (nchars <= 0) {
	close (fd);
	XtRemoveInput (*id);
	setText (statusText, "connection closed");

	/* Enable the following if XtRemoveInput is used in XtInputCallback
	 * XtAppAddInput (appContext, server_socket, XtInputReadMask,
	 *   (XtInputCallbackProc)connectToClient, (XtPointer)server_socket);
	 */
    }

done:

    XFlush (display);
    doPendingEvents();
}


/* appendText -- Append some text to the message buffer.
 */
static void
appendText (text)
char *text;
{
    register char *ip, *op;
    static XawTextPosition pos = 0;
    static int newline = 0;
    XawTextBlock tx;
    char buf[1024];

    if (pos == 0)
	setText (messages, "");

    op = buf;
    if (newline)
	*op++ = '\n';

    for (ip=text;  *ip;  )
	*op++ = *ip++;

    if (newline = (*(ip-1) == '\n'))
	op--;

    *op = EOS;

    tx.ptr = buf;
    tx.length = op - buf;
    tx.format = FMT8BIT;
    tx.firstPos = 0;

    XawTextReplace (messages, pos, pos, &tx);
    XawTextSetInsertionPoint (messages, (pos += (op - buf)));
}


/*
 * Socket i/o code.
 * --------------------------
 */


/* portSetup -- Set up the tape status port, used by remote tape driver client
 * to send messages to xtapemon.
 */
static int
portSetup (first_port)
int	first_port;
{
	struct sockaddr_in sockaddr;
	int bound, reuse=1;
	int s, port, i;

	if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
	    status1 ("socket creation fails, errno=%d", errno);
	    return (ERR);
	}

	/* Try to bind an address to the socket.  If the first try doesn't
	 * succeed it may be because a server is already bound to the port,
	 * so try a higher numbered port.
	 */
	for (bound=i=0;  i < MAXPORTS;  i++) {
	    port = first_port + i;
	    memset ((char *)&sockaddr, 0, sizeof(sockaddr));
	    sockaddr.sin_family = AF_INET;
	    sockaddr.sin_port = htons((short)port);
	    sockaddr.sin_addr.s_addr = INADDR_ANY;

	    if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&reuse,
		    sizeof(reuse)) < 0) {
		close (s);
		return (ERR);
	    }

	    if (bind (s, (struct sockaddr *)&sockaddr, sizeof(sockaddr)) >= 0) {
		server_port = port;
		bound++;
		break;
	    }
	}

	if (!bound) {
	    status1 ("bind on socket fails, errno=%d", errno);
	    close (s);
	    return (ERR);
	}

	if (listen (s, MAXCONN) < 0) {
	    status1 ("listen on socket fails, errno=%d", errno);
	    close (s);
	    return (ERR);
	}

	return (s);
}


/* portOpen -- Called when processing is needed on a socket, either when
 * a client is requesting a connection or when data is ready on a connected
 * socket.
 */
static int
portOpen (s)
int	s;
{
	int fd;
	if ((fd = accept (s, (struct sockaddr *)0, (int *)0)) < 0)
	    return (ERR);
	else
	    return (fd);
}


/* portClose -- Close a file descriptor opened on an active server socket.
 */
static void
portClose (fd)
int	fd;
{
	close (fd);
}


/*
 * Utility routines.
 * --------------------------
 */

/* setText -- Set the given Text item's value to the given string.
 */
void
setText(item,text)
Widget item;
char *text;
{
    Arg args[1];

    if (item != NULL) {
	XtSetArg(args[0],XtNstring,text);
	XtSetValues(item,args,ONE);
    }
}


/* setLabel -- Set the given Label item's value to the given string.
 */
void
setLabel(item,text)
Widget item;
char *text;
{
    Arg args[1];

    if (item != NULL) {
	XtSetArg(args[0],XtNlabel,text);
	XtSetValues(item,args,ONE);
    }
}

void
status0(str)
char *str;
{
    if (statusText != NULL)
	setText(statusText,str);
    doPendingEvents();
}

void
status1(fmt,arg)
char *fmt,*arg;
{
    char buf[256];

    sprintf(buf,fmt,arg);
    status0(buf);
}

void
status2(fmt,arg1,arg2)
char *fmt,*arg1,*arg2;
{
    char buf[256];

    sprintf(buf,fmt,arg1,arg2);
    status0(buf);
}

void
fail0(str)
char *str;
{
    fprintf(stderr,"%s: %s\n",progname,str);
    XtDestroyApplicationContext(appContext);
    exit(1);
}

void
fail1(fmt,arg)
char *fmt,*arg;
{
    char buf[256];

    sprintf(buf,fmt,arg);
    fail0(buf);
}

/* syntax -- Print whatever caused the error and the usage message.
 */
static void
syntax(argc,argv)
int argc;
char **argv;
{
    char *program;

    program = *argv;
    argv += 1;
    if (argc > 2 || (strcmp(*argv,"-help") != 0 && strcmp(*argv,"-?") != 0)) {
	fprintf(stderr,"%s: bad argument(s): ",program);
	while (--argc)
	    fprintf(stderr,"%s ",*argv++);
	fprintf(stderr,"\n");
    }
    fprintf(stderr,"Valid options (in addition to X Toolkit options) are:\n");
    fprintf(stderr,"  -port N\tlisten on port number N\n");
    fprintf(stderr,"  -a\tlisten on alternate port\n");
    fprintf(stderr,"  -d\tpass client messages to standard out\n");
    fprintf(stderr,"Options can be abbreviated to their shortest unique prefix.\n");
}
