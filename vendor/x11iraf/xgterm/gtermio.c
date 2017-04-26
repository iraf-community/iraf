/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/ioctl.h>
#include <ctype.h>
#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Shell.h>
#include <ObmW/Gterm.h>
#include <Obm.h>

#include "gtermio.h"

/*
 * GTERMIO -- XGterm protocol manager for IRAF Gterm graphics emulation.
 * This protocol is an extension of the Tektronix 4012 graphics protocol.
 * The basic extensions are patterned after the Retrographics VT640 graphics
 * terminal, using GS and CAN to switch between vt100 and graphics modes.
 * Additional extensions are defined to support advanced features such as
 * color, area fills, graphics erasure, setting the cursor location under
 * program control, interactive dialog via the "status line", and so on.
 *
 * This is the low level code which filters graphics output out of the ASCII
 * pseudoterminal output stream and decodes the graphics instructions therein,
 * converting these into function calls to the Gterm graphics widget.  The low
 * level pty input code (i/o manager) in XGterm spends most of its time
 * waiting for input on a pty.  When a block of data is physically read from
 * the pty, it is passed to the gio_ptyinput procedure herein.  This routine
 * filters out any graphics data in the input stream, handling text/graphics
 * mode switches, returning any nongraphics input data to be passed on to the
 * vt100 text widget.  The input graphics data is then processed by the
 * protocol manager which decodes the input and makes calls to the drawing
 * functions in the gterm graphics widget.
 */

/* Size limiting definitions. */
#define TEK_XRES	1024		/* tek4012 logical X resolution */
#define TEK_YRES	780		/* tek4012 logical Y resolution */
#define TEK_ROWS	35		/* tek4012 screen size, rows	*/
#define TEK_COLS	80		/* tek4012 screen size, columns	*/
#define	SZ_GBUF		16384		/* max buffered graphics data	*/
#define	GB_MINSPACE	2048		/* high water mark		*/
#define	GB_BIGSPACE	12288		/* low water mark		*/
#define	MAX_PLPTS	4096		/* max points in a polyline	*/
#define	MAX_COLORS	256		/* max colormap cells		*/
#define	SZ_TXBUF	1024		/* max chars in a polytext	*/
#define SZ_STRBUF	128		/* string input buffer		*/
#define	SZ_MSGBUF	16384		/* object message fragments	*/
#define	SZ_ESCAPE	64		/* client escape sequence	*/
#define	INC_MSGBUF	16384		/* increment if overflow	*/
#define	MAX_TEXTCHARS	132		/* max chars in text line	*/
#define	SL_XOFFSET	0		/* x offset to status line	*/
#define	SL_YOFFSET	5		/* y offset to status line	*/
#define	MAX_QUOTA	512		/* limit for one proc. loop	*/
#define	MAXNDC		32767		/* max NDC coordinate value	*/
#define	MAX_GTERM	128		/* max gterm widgets		*/

/* Magic numbers. */
#define	SET_BITS	0		/* draw vectors|points		*/
#define	CLEAR_BITS	1		/* erase vectors|points		*/
#define	TOGGLE_BITS	2		/* toggle data bits		*/
#define	COMMAND_MODE	0		/* initial state		*/
#define	ALPHA_MODE	1		/* tek-alpha character drawing	*/
#define	TEXT_MODE	2		/* output to status line	*/
#define	VECTOR_MODE	3		/* draw vectors or points	*/
#define	MESSAGE_MODE	4		/* accumulate object messages	*/
#define	WIMAGE_MODE	5		/* draw pixels in image		*/
#define	WCMAP_MODE	6		/* write to colormap		*/
#define	WIOMAP_MODE	7		/* write to iomap		*/
#define	CURSOR_MODE	8		/* read crosshair cursor posn	*/
#define	BREAK_LINE	(-2)		/* special automargin code	*/
#define ERR		(-1)		/* error return code		*/

/* ASCII codes. */
#define	ETX		'\003'
#define	ENQ		'\005'
#define	BEL		'\007'
#define	CR		'\015'
#define	CAN		'\030'
#define	EM		'\031'
#define	SUB		'\032'
#define	ESC		'\033'
#define	FS		'\034'
#define	GS		'\035'
#define	RS		'\036'
#define	US		'\037'

/* The following are the xterm equivalents of GS, CAN.   Gtermio recognizes
 * either as graphics start and end sequences.
 */
#define	XGS		"[?38h"		/* xterm GS escape sequence	*/
#define	LEN_XGS		5		/* length excluding ESC		*/
#define	XGE		"\003"		/* graphics terminator (ETX)	*/
#define	LEN_XGE		1		/* length excluding ESC		*/

static	XtAppContext app_con;
static	ObmContext obm;			/* object manager		*/
static	Widget gw;			/* graphics widget		*/

static	int gio_graphicsenabled = 0;	/* switch text/graphics output	*/
static	int gio_enabled = 1;		/* enable graphics window	*/
static	Widget gterms[MAX_GTERM];
static	int actions_registered = 0;

/* Pseudoterminal i/o.
 */
static	int pty_fd;			/* fd of pseudoterminal		*/
static	int pty_stop = 0;		/* set when XOFF is set on pty	*/

/* The graphics data buffer, a circular buffer.  Note that while buffer
 * data is unsigned char, g_getc below returns a signed integer value.
 */
static	unsigned char g_buf[SZ_GBUF];	/* circular buffer		*/
static	unsigned char *g_top= &g_buf[SZ_GBUF];	/* end of buffer + 1	*/
static	unsigned char *g_ip = g_buf;	/* input pointer		*/
static	unsigned char *g_op = g_buf;	/* output pointer		*/

#define g_getc(c)	(g_ip == g_op ? \
	((c)=0, -1) : ((c) = *g_ip++, g_ip >= g_top ? *(char *)(g_ip=g_buf):0))
#define	g_putc(c)\
	(*g_op++ = (c), ((g_op >= g_top) ? g_op = g_buf : g_op))
#define	g_ungetc(c)\
	(g_ip = ((g_ip==g_buf) ? g_top-1 : g_ip-1))
#define g_spaceleft\
	(g_ip <= g_op ? (g_top - g_op + g_ip - g_buf) : (g_ip - g_op))
#define g_havedata	(g_ip != g_op)
#define	g_mark(ip)	((ip)=g_ip)
#define	g_reset(ip)	(g_ip=(ip))
#define	g_equal(ip)	((ip)==g_ip)

/* Message buffer.
 */
static	char	*msgbuf = NULL;
static	int	len_msgbuf = 0;
static	int	msg_op = 0;

/* Polyline (polymarker) output-point buffer.
 */
static	char	pl_text[MAX_PLPTS];	/* encoded [x,y] coord data	*/
static	XPoint	pl_p[MAX_PLPTS];	/* polyline storage		*/
static	int	pl_npts = 0;		/* npoints in polyline		*/
static	int	pl_op = 0;		/* which char in coord pair	*/
static	int	pl_pointmode = 0;	/* point or line mode		*/
static	int	pl_areamode = 0;	/* fill area mode		*/

static	int	ohiy=0, oloy=0;		/* encoded current position	*/
static	int	ohix=0, olox=0;

/* Graphics text variables.
 */
static	char	tx_buf[SZ_TXBUF+1];	/* polytext text buffer		*/
static	int	tx_len = 0;		/* nchars in buffer		*/
static	int	tx_maxlines;		/* nlines of text on a screen	*/
static	int	tx_maxcols;		/* ncols of text on a screen	*/
static	int	tx_charheight;		/* height of a char in pixels	*/
static	int	tx_charwidth;		/* width of a char in pixels	*/
static	int	tx_charbase;		/* topline to baseline distance	*/
static	int	tx_leftmargin;		/* where columns start		*/
static	int	sl_x, sl_y;		/* current pos. in status line	*/
static	int	sl_charwidth;		/* status line char width	*/
static	int	sl_charheight;		/* status line char height	*/
static	int	sl_charbase;		/* topline to baseline distance	*/

/* Miscellaneous variables.
 */
static	int	cur_x, cur_y;		/* current x,y position		*/
static	int	tek_xres, tek_yres;	/* resolution of input data	*/
static	int	win_xres, win_yres;	/* resolution of draw window	*/
static	int	trailer1 = '\r';	/* trailer code, cursor value	*/
static	int	trailer2 = -1;		/* second trailer code (opt)	*/
static	char	s_reset[SZ_ESCAPE];	/* sent to client on reset	*/
static	char	s_resize[SZ_ESCAPE];	/* sent to client on resize	*/
static	int	gio_mode=COMMAND_MODE;	/* graphics drawing mode	*/
static	int	gio_datalevel=SET_BITS;	/* set, clear, or toggle bits	*/
static	int	workstation_open = 0;	/* have issued open workstation	*/
static	int	wait_cursor = 0;	/* waiting for cursor input	*/
static	int	wincursor = 0;		/* return window cursor		*/
static	int	gio_delay = 0;		/* wait for widget to ready	*/
static	int	gio_pending = 0;	/* workproc already posted	*/

/* Imaging variables. */
static	int	wi_encoding, wi_raster, wi_x1, wi_y1, wi_nx, wi_ny, wi_bp;
static	int	wc_map, wc_first, wc_ncolors;

/* Macros to convert between tektronix and window coordinates. */
#define X_TEK2WIN(x)  (              ((x)  * win_xres + tek_xres/2) / tek_xres)
#define Y_TEK2WIN(y)  (win_yres-1 - (((y)  * win_yres + tek_yres/2) / tek_yres))
#define X_WIN2TEK(x)  (((             (x)) * tek_xres + win_xres/2) / win_xres)
#define Y_WIN2TEK(y)  (((win_yres-1 - (y)) * tek_yres + win_yres/2) / win_yres)

#define min(a,b)  ((a)<(b)?(a):(b))
#define max(a,b)  ((a)>(b)?(a):(b))

/* OBM request queue. */
struct request {
	int key;
	int sx, sy;
	int raster;
	int rx, ry;
	int nchars;
	char *strval;
	struct request *next;
};

typedef	struct request Request;
typedef Request	*RequestPtr;
static RequestPtr request_head = NULL;
static RequestPtr request_tail = NULL;

static	int gio_reset(), gio_clear(), gio_setginmodeterm(), gio_output();
static	int gio_retcursor(), gio_queue_output(), gio_queue_request();
static	int gio_hardreset(), gio_activate(), gio_enable(), gio_tekmode();
static	int gio_processdata(), gio_ptyinput(), gio_escape(), gio_status();
static	int gio_activate_cb(), gio_connect_cb();
static	int gio_deactivate_cb();
static	void gio_keyinput(), gio_resize();
static	void pl_decodepts(), gio_retenq();

/* Externally callable routines. */
static struct GT_function gio_functions[] = {
	"reset",		gio_hardreset, NULL,
	"clear",		gio_clear, NULL,
	"input",		gio_ptyinput, NULL,
	"output",		gio_processdata, NULL,
	"activate",		gio_activate, NULL,
	"status",		gio_status, NULL,
	"enable",		gio_enable, NULL,
	"tekmode",		gio_tekmode, NULL,
	"setGinmodeTrailers",	gio_setginmodeterm, NULL,
};


/* Translation to hook Tek menu to gterm widget. */
extern void HandlePopupMenu();
extern void DeleteWindow();
extern char *gtermio_getResource();
static Atom wm_delete_window = 0;   /* for ICCCM delete window */

static char *gio_shellTrans =
	"<ClientMessage>WM_PROTOCOLS: DeleteWindow()\n";
static char *gio_tekMenu =
	"!Ctrl <Btn3Down>: popup-xtmenu(tekMenu)\n";
static XtActionsRec actionsList[] = { 
	{ "popup-xtmenu",       HandlePopupMenu },
	{ "DeleteWindow",	DeleteWindow },
};


/*
 * GTERMIO external procedures.
 * ----------------------------
 */

/* GIO_SETUP -- Called by the high level Gterm window management code during
 * process startup to establish communications between the caller and gtermio.
 */
void
gio_setup (app_context, argc, argv, fd)
XtAppContext app_context;	/* applications context of caller */
int	argc;			/* argument count */
char	*argv[];		/* argument vector */
int	fd;			/* fd of pty for terminal i/o */
{
	app_con = app_context;
	pty_fd = fd;

	/* Register client callable functions. */
	gtermio_register (gio_functions, XtNumber(gio_functions));

	/* Open the object manager. */
	obm = ObmOpen (app_context, argc, argv);
	ObmAddCallback (obm, OBMCB_connect|OBMCB_preserve,
	    gio_connect_cb, NULL);
	ObmAddCallback (obm, OBMCB_activate|OBMCB_preserve,
	    gio_activate_cb, NULL);
	ObmAddCallback (obm, OBMCB_deactivate|OBMCB_preserve,
	    gio_deactivate_cb, NULL);
	ObmAddCallback (obm, OBMCB_clientOutput|OBMCB_preserve,
	    gio_queue_output, NULL);
	ObmAddCallback (obm, OBMCB_setGterm|OBMCB_preserve,
	    gio_reset, NULL);

	/* Register xgterm global actions. */
	if (!actions_registered) {
	    XtAppAddActions (app_context, actionsList, XtNumber(actionsList));
	    actions_registered++;
	}

	gio_hardreset (0);
}


/* GIO_POSTCONNECTCALLBACK -- Called by the client to post a procedure to
 * be called when the display connection is opened or close.
 */
void
gio_postconnectcallback (connect, client_data)
void (*connect)();
int client_data;
{
	if (obm) {
	    ObmAddCallback (obm, OBMCB_connect|OBMCB_preserve,
		connect, client_data);
	}
}


/* GIO_ENABLE -- Enable or disable the graphics window.  If graphics is
 * disabled, all i/o is directed to the text window.
 */
static
gio_enable (dummy, onoff)
int	dummy;
int	onoff;
{
	switch (onoff) {
	case 0:
	    gio_enabled = 0;
	    gio_graphicsenabled = 0;
	    break;
	case 1:
	    gio_enabled = 1;
	    break;
	}

	return (gio_enabled);
}


/* GIO_ACTIVATE -- Callback procedure called by the client application to
 * forcibly activate or deactivate the graphics UI.
 */
static
gio_activate (dummy, state)
int dummy;
int state;
{
	register RequestPtr rp;

	/* Cancel any buffered command output. */
	wait_cursor = 0;
	while (rp = request_head) {
	    request_head = rp->next;
	    free ((char *)rp);
	}
	request_head = request_tail = NULL;

	switch (state) {
	case 0:
	    /* Deactivate.  If the application is currently waiting for
	     * cursor input send it EOF to indicate that the graphics window
	     * is deactivating and that the application should revert to
	     * terminal mode.
	     */
	    if (wait_cursor) {
		gio_retcursor ('\004', 0,0, 0,0,0, 0);   /* ctrl/d */
                if (gw)
                    GtSetCursorType (gw, GtIdleCursor);
	    }

	    ObmDeactivate (obm, 1);
	    gtermio_close_workstation();
	    gio_graphicsenabled = 0;
	    break;

	case 1:
	    ObmActivate (obm);
	    gtermio_open_workstation();
	    break;
	}

	return (ObmActivated (obm));
}


/* GIO_STATUS -- Query the status of the Object Manager, i.e., whether or
 * not a GUI has been loaded.
 */
static
gio_status (dummy, app_name, app_class)
int dummy;
char *app_name;			/* can be NULL */
char *app_class;		/* can be NULL */
{
	return (ObmStatus (obm, app_name, app_class));
}


/* GIO_ACTIVATE_CB -- Activate callback, called by the gterm widget when the
 * user interface is activated.
 */
static
gio_activate_cb (dummy, w, state)
int dummy;
Widget w;
int state;
{
	register RequestPtr rp;

	if (!state)
	    return;

	/* Cancel any buffered command output. */
	wait_cursor = 0;
	while (rp = request_head) {
	    request_head = rp->next;
	    free ((char *)rp);
	}
	request_head = request_tail = NULL;

	if (state)
	    gtermio_open_workstation();
	else
	    gtermio_close_workstation();

	/* Arrange to intercept WM events on toplevel window. */
	wm_delete_window = XInternAtom (XtDisplay(w),
	    "WM_DELETE_WINDOW", False);
	XSetWMProtocols (XtDisplay(w), XtWindow(w), &wm_delete_window, 1);
}


/* GIO_DEACTIVATE_CB -- Deactivate callback, called by the gterm widget when
 * the user interface is deactivated.  In reality we're just a dummy routine
 * to intercept a window close action in a GUI to keep from shutting down 
 * completely.
 */
static
gio_deactivate_cb (dummy, w, state)
int dummy;
Widget w;
int state;
{
}


/* GIO_CONNECT_CB -- Connect callback, called by the gterm widget when a new
 * application GUI is initialized or when the display connection is closed.
 */
static
gio_connect_cb (dummy, display, toplevel, state)
int dummy;
Display *display;
Widget toplevel;
int state;
{
	if (state) {
	    extern Widget term;
	    extern char *mktemp();
	    XrmDatabase db1, db2;
	    char *fname, buf[256];

	    /* Merge XGterm resources into GUI.  There ought to be a way
	     * to do this without writing a temporary file, but there
	     * appears to be no alternative at present.
	     */
	    strcpy (buf, "/tmp/XGdbXXXXXX");
	    if (fname = mktemp (buf)) {
		/* Merge XGterm resources. */
		db1 = XrmGetDatabase (XtDisplay(term));
		XrmPutFileDatabase (db1, fname);
		db1 = XrmGetFileDatabase (fname);
		db2 = XrmGetDatabase (XtDisplay(toplevel));
		XrmMergeDatabases (db1, &db2);
		unlink (fname);

		/* Pass on the default Tek geometry. */
		XtVaSetValues (toplevel, XtNgeometry, 
		    gtermio_getResource ("geometry"), NULL);
	    }

	    XtAugmentTranslations (toplevel,
		XtParseTranslationTable (gio_shellTrans));
	} else
	    memset (gterms, 0, sizeof(gterms));
}


/* GIO_TEKMODE -- Direct input to the graphics window or the text window.
 * Normally this is done by the client via the datastream but this routine
 * can be called to manually switch the input to a window.
 */
static
gio_tekmode (dummy, onoff)
int dummy;
int onoff;
{
	switch (onoff) {
	case 0:
	    gio_graphicsenabled = 0;
	    break;
	case 1:
	    if (gio_enabled)
		gio_graphicsenabled = 1;
	    break;
	}

	return (gio_graphicsenabled);
}


/* GIO_CLEAR -- Clear the graphics window.
 */
static
gio_clear (dummy)
int dummy;
{
	if (gw) {
	    GtClearScreen (gw);
	    GtSetRaster (gw, 0);
	}

	sl_x = 0;
	sl_y = sl_charbase;
	cur_x = tx_leftmargin;
	cur_y = tx_charbase;
}

/* debug routine. */
gio_eventmask (w)
Widget w;
{
	printf ("mask = 0x%x\n", XtBuildEventMask(w));
}


/* GIO_HARDRESET -- Reset everything, including cancelling any cursor read
 * that may be in progress.
 */
static
gio_hardreset (dummy)
int dummy;
{
	register RequestPtr rp;

	/* If a cusor read is currently in progress send the application EOF
	 * to indicate that it should exit graphics mode.
	 */
	if (wait_cursor) {
	    gio_retcursor ('\004', 0,0, 0,0,0, 0);   /* ctrl/d */
	    if (gw)
		GtSetCursorType (gw, GtIdleCursor);
	}

	/* Send a reset message to the client, if the client posted a reset
	 * escape sequence.
	 */
	if (s_reset[0])
	    v_write (pty_fd, s_reset, strlen(s_reset));

	/* The client must reinitialize the client escapes after a reset. */
	s_reset[0] = '\0';
	s_resize[0] = '\0';

	/* Initialize the object manager (destroys any current user
	 * interface).
	 */
	ObmInitialize (obm);
	gtermio_close_workstation();
	if (msgbuf)
	    free (msgbuf);
	msgbuf = (char *) malloc (len_msgbuf = SZ_MSGBUF);
	msg_op = 0;

	memset (gterms, 0, sizeof(gterms));
	actions_registered = 0;

	/* Initialize the input buffer. */
	g_ip = g_op = g_buf;
	g_top = &g_buf[SZ_GBUF];

	/* Initialize the graphics state.  */
	gio_mode = COMMAND_MODE;
	gio_graphicsenabled = 0;
	workstation_open = 0;
	gio_delay = 0;
	pty_stop = 0;

	/* Cancel any buffered command output. */
	wait_cursor = 0;
	while (rp = request_head) {
	    request_head = rp->next;
	    free ((char *)rp);
	}
	request_head = request_tail = NULL;
	return (0);
}


/* GIO_RESET -- Reset the state of the gtermio code.  Should be called
 * whenever any important data structures change, e.g., if the graphics
 * window is resized.
 */
static
gio_reset (notused, w, args)
int notused;
register Widget w;
char *args;
{
	register int i;
	int new_widget;

	/* Make this the active graphics widget. */
	if ((gw = w) == NULL)
	    return (0);

	GtReset (w);
	GtActivate (w);

	/* Having a callback post a callback can result in an infinite
	 * loop, so only post the callbacks once per widget.
	 */
	new_widget = 1;
	for (i=0;  i < MAX_GTERM;  i++)
	    if (gterms[i] == w) {
		new_widget = 0;
		break;
	    }

	if (new_widget) {
	    /* Tell widget how to talk to gtermio. */
	    GtPostResetProc (w, gio_reset, (XtPointer)NULL);
	    GtPostInputProc (w, gio_keyinput, (XtPointer)NULL);
	    GtPostResizeProc (w, gio_resize, (XtPointer)NULL);
	    GtOverrideTranslations (w, gio_tekMenu);
	    GtTimerInhibit (w, False);

	    for (i=0;  i < MAX_GTERM;  i++)
		if (!gterms[i]) {
		    gterms[i] = w;
		    break;
		}
	}

	GtEraseAlphaCursor (w);
	GtSetTextRes (w, TEK_ROWS, TEK_COLS);
	GtGetPhysRes (w, 0, &win_xres, &win_yres);
	GtSetLogRes (w, win_xres, win_yres);
	tek_xres = TEK_XRES;
	tek_yres = TEK_YRES;

	GtGetAlphaTextSize (w, NULL,
	    &tx_charwidth, &tx_charheight, &tx_charbase);
	tx_maxlines = win_yres / tx_charheight;
	tx_maxcols = win_xres / tx_charwidth;
	tx_leftmargin = 0;
	tx_len = 0;

	GtGetDialogTextSize (w, " ",
	    &sl_charwidth, &sl_charheight, &sl_charbase);
	sl_x = 0;
	sl_y = sl_charbase;

	pl_npts = 0;
	pl_op = 0;
	pl_pointmode = 0;
	pl_areamode = 0;
	ohiy = 0; oloy = 0;
	ohix = 0; olox = 0;

	cur_x = tx_leftmargin;
	cur_y = tx_charbase;

	return (0);
}


/* GIO_SETGINMODETERM -- Set the GIN mode (cursor read) trailer codes,
 * expressed as octal constants in the input string argument.
 */
static
gio_setginmodeterm (dummy, str)
int	dummy;
char	*str;
{
	register char	*ip;
	register int	n;

	trailer1 = trailer2 = -1;

	for (ip=str;  isspace(*ip);  ip++)
	    ;
	if (isdigit(*ip)) {
	    for (n=0;  isdigit(*ip);  ip++)
		n = n * 8 + *ip - '0';
	    trailer1 = n;
	}

	while (*ip && isspace(*ip))
	    ip++;
	if (isdigit(*ip)) {
	    for (n=0;  isdigit(*ip);  ip++)
		n = n * 8 + *ip - '0';
	    trailer2 = n;
	}

	return (0);
}


/*
 * Internal procedures.
 * --------------------
 */


/* GIO_RESIZE -- Callback procedure called by the gterm widget when the
 * drawing window is resized.
 */
static void
gio_resize (notused, w)
XtPointer notused;
Widget w;
{
	/* Ignore the resize callback if the widget being resized is not the
	 * active widget.
	 */
	if (w != gw)
	    return;

	/* Always update the window size variables. */
	if (gw) {
	    GtGetPhysRes (gw, GtGetRaster(gw), &win_xres, &win_yres);
	    GtSetLogRes (gw, win_xres, win_yres);

	    GtGetAlphaTextSize (gw, NULL,
		&tx_charwidth, &tx_charheight, &tx_charbase);
	    tx_maxlines = win_yres / tx_charheight;
	    tx_maxcols = win_xres / tx_charwidth;
	    tx_leftmargin = 0;
	    tx_len = 0;

	    GtGetDialogTextSize (gw, " ",
		&sl_charwidth, &sl_charheight, &sl_charbase);
	    sl_x = 0;
	    sl_y = sl_charbase;
	}

	/* Do not do a full reset and redraw if the resize request occurs
	 * while we are processing a buffer full of data.  This happens when
	 * the window is first mapped, while processing the first block of
	 * drawing instructions.
	 */
	if (!g_havedata) {
	    if (gw)
		gio_reset (NULL, gw, NULL);

	    /* If the client posted a resize escape sequence, send this
	     * value to the client as a cursor read to signal the resize
	     * event.  The window size is returned in the RX,RY fields.
	     */
	    if (s_resize[0]) {
		int key = s_resize[0];
		char *strval = s_resize + 1;
		gio_queue_request (0,0,0, win_xres, win_yres, key, strval);
	    }

	    if (wait_cursor && gw)
		GtSetCursorType (gw, GtBusyCursor);
	}
}


/* GIO_QUEUE_OUTPUT -- Queue an OBM client request to be sent to the client
 * in response to the next client cursor read request.  This is a callback
 * procedure called by OBM to queue a request (command) to be sent to the
 * client process.  If the client already has a request for input (cursor
 * read) pending, the request will be passed on immediately.
 */
static int
gio_queue_output (fd, tcl, objname, key, strval)
int fd;				/* pty */
XtPointer tcl;			/* not used */
char *objname;			/* client object name (not used) */
int key;			/* cursor keystroke or NULL */
char *strval;			/* cursor strval or literal command */
{
        int mapping, raster;
	int sx, sy, rx, ry;

	/* Get the coordinates of the last event processed by the gterm
	 * widget.  This is meaningless for many OBM client requests but
	 * will be valid for, e.g., GUI translation events within the
	 * gterm widget.
	 */
	if (gw)
	    GtGetCursorPos (gw, &sx, &sy);
	else
	    sx = sy = 0;

	/* Convert screen (window) coordinates to raster coordinates. */
	if (gw && wincursor) {
	    raster = GtSelectRaster (gw, 0, GtPixel, sx, sy, 
		GtNDC, &rx, &ry, &mapping);
	    ry = MAXNDC - ry;
	} else
	    raster = rx = ry = 0;

	return (gio_queue_request (sx, sy, raster, rx, ry, key, strval));
}


/* GIO_QUEUE_REQUEST -- Queue a request.
 */
static int
gio_queue_request (sx, sy, raster, rx, ry, key, strval)
int sx, sy;
int raster, rx, ry;
int key;
char *strval;
{
	register RequestPtr rp;
	int buflen, nchars;
	char *buf;

	nchars = strlen (strval);
	buflen = sizeof(Request) + nchars + 1;
	if ((buf = (char *) malloc (buflen)) == NULL)
	    return (-1);

	rp = (RequestPtr) buf;
	rp->key = key;
	rp->sx = sx;
	rp->sy = sy;
	rp->raster = raster;
	rp->rx = rx;
	rp->ry = ry;
	rp->nchars = nchars;
	rp->strval = buf + sizeof(Request);
	strcpy (rp->strval, strval);
	rp->next = NULL;

	/* Link request at tail of request list. */
	if (!request_head)
	    request_head = request_tail = rp;
	else {
	    request_tail->next = rp;
	    request_tail = rp;
	}

	/* If a client cursor read is currently pending return the next
	 * available request.
	 */
	if (wait_cursor)
	    gio_output();
}


/* GIO_OUTPUT -- Return the next available OBM request from the request
 * queue to the client, in response to a ready for input (cursor read) from
 * the client.  The data sent to the client consists of a cursor value
 * struct and/or a data string.  If both are sent the cursor value struct
 * contains a field giving the length of the data string which follows.
 */
static int
gio_output()
{
	register RequestPtr rp;

	if (!(rp = request_head))
	    return (-1);

	/* Return a cursor read as if "key" had been typed. */
	if (rp->key) {
	    gio_retcursor (rp->key, rp->sx, rp->sy,
		rp->raster, rp->rx, rp->ry, rp->nchars);
	}

	/* Return the string value, if any.  */
	if (rp->nchars > 0) {
	    v_write (pty_fd, rp->strval, rp->nchars);
	    if (!rp->key)
		v_write (pty_fd, "\r", 1);
	}

	/* Remove the request from the head of the queue. */
	if (!(request_head = rp->next))
	    request_tail = NULL;
	free ((char *)rp);

	return (0);
}


/* GIO_PTYINPUT -- Process pty input packets.  Output directed to the
 * terminal (/dev/tty) by the applications program appears as read-pending
 * events on the pty seen by the XGterm program.  We let the XGterm pty input
 * code monitor the pty and respond to read-pending events.  The low level
 * read code reads the data and then calls us to process the data packet.  We
 * extract any graphics output from the packet and append it to the gio
 * buffer.  If data is added to the gio buffer a gio-data-pending event is
 * queued so that the graphics drawing code will be called to process the new
 * data.  The remaining data, or a null length packet if the packet contained
 * only graphics data, is returned to the caller, completing the read.
 * Sometime later the graphics drawing code will be called to process the data.
 */
static int
gio_ptyinput (notused, ttybuf, nchars)
int	notused;
char	*ttybuf;		/* raw data on input, tty data on output */
int	nchars;			/* nchars of raw data */
{
	register char *itop = ttybuf + nchars;
	register char *op, *ip = ttybuf, ch;

	if (!gio_enabled || nchars <= 0)
	    return (nchars);

	/* If in text mode, make a quick scan for the graphics start sequence
	 * and return the entire data packet if graphics mode is not entered.
	 * Graphics start is indicated either by GS or by the xterm graphics
	 * start sequence XGS.
	 */
	if (!gio_graphicsenabled) {
	    while (ip < itop && *ip != GS &&
		    !(*ip == ESC && strncmp (ip+1, XGS, LEN_XGS) == 0))
		ip++;
	    if (ip >= itop)
		return (nchars);
	    else
		op = ip;
	} else
	    op = ttybuf;

	/* If the gio buffer has reached the high-water mark, call the
	 * output processing routine to dispose of some of the data.
	 */
	if (g_spaceleft < max(GB_MINSPACE,nchars)) {
	    while (g_spaceleft < max(GB_BIGSPACE,nchars))
		if (gio_processdata())
		    break;
	}

	/* Process rest of data in graphics mode.  IP is pointing at the
	 * first char of graphics data, ITOP at the top of the buffer,
	 * and OP at the next tty output char.  Filter out any NULs in
	 * the process of copying the data.
	 */
	while (ip < itop)
	    if (gio_graphicsenabled) {
		while (ip < itop) {
		    if ((ch = *ip++) == CAN) {
gend:			g_putc (ch);
			gio_graphicsenabled = 0;
			break;
		    } else if (ch == ESC && strncmp(ip,XGE,LEN_XGE) == 0) {
			/* Treat xterm graphics terminator the same as CAN. */
			ip += LEN_XGE;
			trailer1 = '\r';
			ch = CAN;
			goto gend;
		    } else if (ch)
			g_putc (ch);
		}
	    } else {
		while (ip < itop) {
		    ch = *ip++;
		    if (ch == GS) {
gstart:			g_putc (GS);
			gio_graphicsenabled = 1;
			break;
		    } else if (ch == ESC && strncmp(ip,XGS,LEN_XGS) == 0) {
			ip += LEN_XGS;
			trailer1 = -1;
			goto gstart;
		    } else if (ch)
			*op++ = ch;
		}
	    }

	return (op - ttybuf);
}


/* GIO_PROCESSDATA -- Called to process graphics instructions and data from
 * the gio buffer.  This is the routine which actually draws lines and text
 * in the graphics window.  May be called repeatedly to process any amount of
 * data at a time.  If there is a great amount of data to be processed the
 * routine should return occasionally to allow the other XGterm event handlers
 * to run (operation is not fully asynchronous).
 *
 * Graphics data is processed as a stream with no record boundaries, so that
 * operation is not dependent on how data is buffered through the system.
 * The graphics engine is a state machine which is by definition always in a
 * legal state; garbage input causes garbage output, just like a real terminal.
 * The states are as follows:
 *
 *	COMMAND_MODE	This is the initial state.  Characters are accumulated
 *			until a known state is recognized.  Receipt of ESC
 *			always causes command mode to be entered, since
 *			additional characters are needed to define the next
 *			instruction.
 *
 *	ALPHA_MODE	Characters are drawn in the graphics window at the
 *			"current" position (normally set beforehand with a
 *			GS/US vector move), using the alpha mode font.
 *			Receipt of any control code causes alpha mode to be
 *			exited.
 *
 *	TEXT_MODE	Text mode is a special mode used to write transient
 *			text in the status line, using the text mode font.
 *			Lines of text are accumulated and displayed on the
 *			status line in reverse video; successive lines of text
 *			overwrite one another.  The status line is cleared
 *			when text mode is entered, even if no text is drawn.
 *			Text mode is terminated by receipt of GS or CAN.
 *
 *	VECTOR_MODE	Vector mode refers to both polyline and polypoint
 *			vector sequences.  The vertices of the points are
 *			accumulated in a buffer and displayed when the buffer
 *			fills or when vector mode is terminated.  Vector
 *			mode is terminated by receipt of any control code;
 *			the tektronix coordinate encoding maps all possible
 *			coordinates into the printable ascii codes.
 *
 *	MESSAGE_MODE	In message mode input text is accumulated in a buffer
 *			and eventually passed to the object manager, which
 *			delivers the message to the referenced object.
 *			Messages are used to download the user interface to
 *			be executed by the object manager, and during
 *			execution messages are used to set the values of
 *			user interface parameters to allow the UI to track
 *			the state of the client application.
 *
 *	WIMAGE_MODE	Pixels are accumulated and written to a gterm widget
 *			image raster.
 *
 *	WCMAP_MODE	Color triplets are accumulated and written to the
 *			gterm widget colormap.
 *
 *	WIOMAP_MODE	Colormap indices are accumulated and written to the
 *			gterm widget iomap.
 *
 *	CURSOR_MODE	The crosshair cursor is turned on, signifying to the
 *			user that the system is waiting on a cursor read.
 *			Output processing ceases until the user types a key
 *			or presses a mouse button to trigger the cursor read.
 *			The cursor value is then encoded and transmitted back
 *			to the pty, and output processing resumes.
 *
 * Clearing the screen causes the mode to be reset to command mode, and all
 * other drawing parameters to be set to their default values, e.g., data level
 * on, solid line type, and so on.
 */
static int
gio_processdata()
{
	register int quota, ch;
	unsigned char *save_ip, *ip_start;
	int textwidth;

	/* If gio_delay is set wait for the Gterm widget to become ready
	 * before processing any further graphics input data.
	 */
	if (gio_delay) {
	    gio_delay = gw ? !GtReady (gw) : 0;
	    if (gio_delay)
		return (1);
	}

	if (!g_havedata)
	    return (1);

	if (gw)
	    GtSetCursorType (gw, GtIdleCursor);
	g_mark (ip_start);

	/* Process data.
	 */
	for (quota=MAX_QUOTA;  --quota >= 0 && g_getc(ch) >= 0;  ) {
	    if (ch == 0 || gio_enabled < 0)
		continue;
again:
	    switch (gio_mode) {
	    case COMMAND_MODE:
		switch (ch) {
		case GS:
		case FS:
		case RS:
		    gio_mode = VECTOR_MODE;
		    pl_pointmode = (ch == FS);
		    pl_areamode = (ch == RS);
		    pl_npts = 0;
		    pl_op = 0;

		    /* Only execute an open workstation if we have not already
		     * done so and if the next command is something other than
		     * close workstation, i.e., no-op sequences GS-CAN are
		     * filtered out, since they would only cause a pointless
		     * switch to the graphics frame and back without drawing.
		     * The open workstation sequence is GS,US.
		     */
		    if (ch == GS && !workstation_open) {
			if (g_getc(ch) < 0) {
			    g_ungetc (GS);
			    gio_mode = COMMAND_MODE;
			    goto exit;
			} else if (ch != CAN) {
			    gtermio_open_workstation();
			    if (ch != EM) {
				/* Create default UI if none has been
				 * downloaded already by client.  Note
				 * that this causes a gio_reset.
				 */
				gio_activate (NULL, 1);
			    }
			    if (gw)
				GtActivate (gw);
			    workstation_open = 1;
			    g_ungetc (ch);
			    goto exit;
			}
		    }
		    break;

		case US:
		case CR:
		    gio_mode = ALPHA_MODE;
		    tx_len = 0;
		    if (ch == CR)
			goto again;
		    break;

		case EM:
		    gio_mode = MESSAGE_MODE;
		    msg_op = 0;
		    break;

		case CAN:
		    if (workstation_open) {
			gtermio_close_workstation();
			if (gw) {
			    GtSetCursorType (gw, GtIdleCursor);
			    GtDeactivate (gw);
			}
			workstation_open = 0;
		    }
		    gio_mode = COMMAND_MODE;
		    goto exit;

		case ESC:
		    g_ungetc (ch);
		    g_mark (save_ip);
		    if (gw)
			GtEraseAlphaCursor (gw);
		    if ((gio_mode = gio_escape()) == -1) {
			gio_mode = COMMAND_MODE;
			g_reset (save_ip);
			goto exit;
		    } else if (gio_mode == CURSOR_MODE)
			goto again;
		    break;

		case BEL:
		    if (gw)
			GtBell (gw);
		    break;

		default:
		    ; /* ignore unknown control chars */
		}
		break;

	    case MESSAGE_MODE:
		if (isprint (ch) || isspace(ch)) {
		    if (msg_op >= len_msgbuf) {
			len_msgbuf += INC_MSGBUF;
			msgbuf = (char *) realloc (msgbuf, len_msgbuf);
		    }
		    /* Map CRLF and LFLF into LF. */
		    if ((ch == '\n' || ch == '\r') && msg_op > 0 && 
			msgbuf[msg_op-1] == '\r')
			    --msg_op;
		    msgbuf[msg_op++] = ch;

		} else {
		    msgbuf[msg_op] = '\0';
		    if (msg_op) {
			char *object, *message;
			char *ip;

			for (object=ip=msgbuf;  *ip && !isspace(*ip);  ip++)
			    ;
			*ip = '\0';
			message = ip + 1;

			ObmDeliverMsg (obm, object, message);
			msg_op = 0;
		    }
		    gio_mode = COMMAND_MODE;
		    goto again;
		}
		break;

	    case WIMAGE_MODE:
		/* Accumulate pixels and write to a gterm widget image
		 * raster.
		 */
		if (ch >= 040) {
		    if (msg_op >= len_msgbuf) {
			len_msgbuf += INC_MSGBUF;
			msgbuf = (char *) realloc (msgbuf, len_msgbuf);
		    }
		    msgbuf[msg_op++] = ch - 040;
		} else {
		    if (gw && wi_nx*wi_ny <= len_msgbuf)
			GtWritePixels (gw, wi_raster, msgbuf, wi_bp,
			    wi_x1, wi_y1, wi_nx, wi_ny);

		    msg_op = 0;
		    gio_mode = COMMAND_MODE;
		    goto again;
		}
		break;

	    case WCMAP_MODE:
		/* Accumulate colormap triplets and write to the gterm widget
		 * colormap.
		 */
		if (ch >= 040) {
		    if (msg_op >= len_msgbuf) {
			len_msgbuf += INC_MSGBUF;
			msgbuf = (char *) realloc (msgbuf, len_msgbuf);
		    }
		    msgbuf[msg_op++] = ch;
		} else {
		    register int i, j, v;
		    register char *ip = msgbuf;
		    unsigned short r[MAX_COLORS], g[MAX_COLORS], b[MAX_COLORS];
		    int b1, b2;

		    msgbuf[msg_op++] = 0;
		    for (i=0;  i < wc_ncolors;  i++) {
			for (j=0;  j < 3;  j++) {
			    b1 = *ip++;  b2 = *ip++;
			    if (b1 < 040 || b2 < 040) {
				wc_ncolors = i;
				break;
			    } else {
				v = ((b1 - 040) << 4) | (b2 - 040);
				if (j == 0)
				    r[i] = (v << 8);
				else if (j == 1)
				    g[i] = (v << 8);
				else
				    b[i] = (v << 8);
			    }
			}
		    }

		    if (gw && wc_ncolors)
			GtWriteColormap (gw, wc_map,
			    wc_first, wc_ncolors, r, g, b);

		    msg_op = 0;
		    gio_mode = COMMAND_MODE;
		    goto again;
		}
		break;

	    case WIOMAP_MODE:
		/* Accumulate colormap indices and write to the gterm widget
		 * iomap.
		 */
		if (ch >= 040) {
		    if (msg_op >= len_msgbuf) {
			len_msgbuf += INC_MSGBUF;
			msgbuf = (char *) realloc (msgbuf, len_msgbuf);
		    }
		    msgbuf[msg_op++] = ch;
		} else {
		    register int b1, b2, i;
		    register char *ip = msgbuf;
		    unsigned short iomap[MAX_COLORS];

		    msgbuf[msg_op++] = 0;
		    for (i=0;  i < wc_ncolors;  i++) {
			b1 = *ip++;  b2 = *ip++;
			if (b1 < 040 || b2 < 040) {
			    wc_ncolors = i;
			    break;
			} else
			    iomap[i] = ((b1 - 040) << 4) | (b2 - 040);
		    }

		    if (gw && wc_ncolors)
			GtWriteIomap (gw, iomap, wc_first, wc_ncolors);

		    msg_op = 0;
		    gio_mode = COMMAND_MODE;
		    goto again;
		}
		break;

	    case ALPHA_MODE:
		/* Tek alpha mode is used to write text to random positions on
		 * the screen, or to write lines of text to the gio window in
		 * "storage scope" mode, where the left and right columns are
		 * alternately written into with an inclusive-or rop.  Alpha
		 * text is graphics output, part of the graphics being drawn.
		 */
		if (ch >= 040) {
		    tx_buf[tx_len++] = ch;
		} else if (ch == '\t') {
		    tx_buf[tx_len++] = 040;
		    if (tx_leftmargin == 0 && tx_charwidth)
			while ((tx_len + (cur_x / tx_charwidth)) % 8 != 0)
			    tx_buf[tx_len++] = 040;
		} else if (ch == '\010' || ch == '\177') {
		    if (tx_len > 0)
			tx_len--;
		    else if (cur_x > tx_leftmargin)
			cur_x -= tx_charwidth;
		} else {
flush_alpha:	    if (tx_len > 0) {
			tx_buf[tx_len] = '\0';
			if (gw) {
			    GtEraseAlphaCursor (gw);
			    GtDrawAlphaText (gw, cur_x, cur_y, tx_buf);
			}
		    }

		    if (gw) {
			GtGetAlphaTextSize (gw, tx_buf,
			    &textwidth, &tx_charheight, &tx_charbase);
			cur_x += textwidth;
		    } else
			cur_x += tx_len * tx_charwidth;
		    tx_len = 0;

		    if (ch == '\n' || ch == BREAK_LINE) {
			cur_y += tx_charheight;
			if (cur_y > win_yres) {
			    if (tx_leftmargin == 0)
				tx_leftmargin = win_xres / 2;
			    else
				tx_leftmargin = 0;
			    cur_y = tx_charbase;
			    if (cur_x < tx_leftmargin)
				cur_x = tx_leftmargin;
			}
			if (ch == BREAK_LINE)
			    cur_x = tx_leftmargin;
		    } else if (ch == '\r') {
			cur_x = tx_leftmargin;
		    } else if (ch != 0) {
			gio_mode = COMMAND_MODE;
			goto again;
		    }
		}

		/* Break long lines at the right margin. */
		if (tx_len && cur_x + (tx_len * tx_charwidth) >= win_xres) {
		    ch = BREAK_LINE;
		    goto flush_alpha;
		}

		break;

	    case TEXT_MODE:
		/* Status or dialog text.  In a graphics application this is
		 * written to the dialog area of the gterm widget.  GUIs can
		 * intercept this text (which is the stdout or stderr of a
		 * task when in graphics mode), e.g. to display it in a
		 * message area.  This is done by defining a UI parameter
		 * "textout" and registering a callback to process the output
		 * text.  If the GUI does not define such a parameter no error
		 * message is printed.  Only multicharacter messages are
		 * passed on in this way.  In graphics applications where the
		 * user is typing into the status line and single characters
		 * are being echoed to the status line, this prevents the
		 * echoed characters from being delivered as messages (it also
		 * prevents any actual single-character messages from being
		 * delivered).
		 */
		if (ch >= 040)
		    tx_buf[tx_len++] = ch;
		else if (ch == '\t')
		    tx_buf[tx_len++] = 040;
		else if (ch == '\010' || ch == '\177') {
		    if (tx_len > 0) {
			char delstr[2];
			delstr[0] = tx_buf[--tx_len];
			delstr[1] = '\0';
			GtGetDialogTextSize (gw, delstr,
			    &textwidth, &sl_charheight, &sl_charbase);
			sl_x -= textwidth;
			if (gw) {
			    GtEraseAlphaCursor (gw);
			    GtDrawDialogText (gw, sl_x, sl_y, " ");
			}
		    }
		} else {
		    if (tx_len > 0) {
			tx_buf[tx_len] = '\0';
			if (tx_len > 1) {

			    if (ch == '\n') {
				tx_buf[tx_len] = ch;
				tx_buf[tx_len+1] = '\0';
			    }
			    tx_buf[tx_len] = '\0';
			    if (tx_len) {
			        char  txtbuf[2048];
			        sprintf (txtbuf, "setValue {%s}\0", tx_buf);
			        ObmDeliverMsg (obm, "textout", txtbuf);
			    }
			}
/*			if (gw && tx_len == 1) {*/
			if (gw) {
			    GtEraseAlphaCursor (gw);
			    GtDrawDialogText (gw, sl_x, sl_y, tx_buf);
			}
		    }

		    if (gw) {
			GtGetDialogTextSize (gw, tx_buf,
			    &textwidth, &sl_charheight, &sl_charbase);
			sl_x += tx_len * sl_charwidth;
		    } else
			sl_x += tx_len * sl_charwidth;

		    if (sl_x > win_xres - sl_charwidth)
			sl_x = win_xres - sl_charwidth;
		    tx_len = 0;

		    if (ch == '\r' || ch == '\n') {
			if (gw)
			    GtEndDialog (gw);
			sl_x = 0;
		    } else if (ch != 0) {
			gio_mode = COMMAND_MODE;
			goto again;
		    }
		}

		/* Truncate long lines. */
		if (sl_charwidth)
		    if (sl_x / sl_charwidth + tx_len >= MAX_TEXTCHARS)
			if (tx_len > 0)
			    --tx_len;
			else
			    sl_x -= sl_charwidth;
		break;

	    case VECTOR_MODE:
		/* Following receipt of GS, accumulate encoded coordinate data
		 * until the buffer fills or a control code is received, then
		 * decode the encoded data to reconstruct the original data
		 * vector, and draw the vector.
		 */
		if (ch >= 040)
		    pl_text[pl_op++] = ch;
		if (ch < 040 || pl_op >= MAX_PLPTS)
		    pl_decodepts();

		if (ch < 040 || pl_npts >= MAX_PLPTS) {
		    if (pl_pointmode && pl_npts >= 1) {
			if (gw)
			    GtDrawPolymarker (gw, pl_p, pl_npts);
		    } else if (pl_areamode && pl_npts >= 1) {
			if (gw)
			    GtDrawPolygon (gw, pl_p, pl_npts);
		    } else if (pl_npts >= 2) {
			if (gw)
			    GtDrawPolyline (gw, pl_p, pl_npts);
		    }

		    if (pl_npts > 0) {
			cur_x = pl_p[pl_npts-1].x;
			cur_y = pl_p[pl_npts-1].y;
			pl_npts = 0;
		    }

		    if (ch < 040) {
			gio_mode = COMMAND_MODE;
			pl_op = 0;
			goto again;
		    }
		}

		break;

	    case CURSOR_MODE:
		/* Initiate a cursor read, i.e., inform the GUI that the
		 * client is ready for the next input command.  If a request
		 * has already been queued for output to the client we send
		 * it back immediately with gio_output.  Otherwise cursor
		 * mode is entered for the active graphics widget and the
		 * wait_cursor flag is set to indicate that the client is
		 * ready for input.
		 */
		if (wait_cursor++) {
		    /* This shouldn't ever happen. */
		    g_ungetc (ch);
		    gio_mode = COMMAND_MODE;
		    if (gw)
			GtSetCursorType (gw, GtBusyCursor);
		} else {
		    /* Return the next request from the output queue, or
		     * enter cursor input mode if no requests are queued.
		     */
		    if (gio_output() == 0) {
			wait_cursor = 0;
			gio_mode = COMMAND_MODE;
		    } else {
			ObmActivate (obm);
			if (gw)
			    GtSetCursorType (gw, GtGinmodeCursor);
		    }
		}
		break;
	    }
	}

exit:
	/* Flush any buffered text before exiting, as applications will assume
	 * that text appears on the screen as soon as chars are written to the
	 * terminal (any buffering must be hidden).
	 */
	if (tx_len > 0) {
	    ch = 0;
	    goto again;
	}

	if (gw)
	    GtFlush (gw);

	if (g_havedata && !g_equal(ip_start) && ch != ESC && !wait_cursor)
	    return (0);			/* call again */
	else {
	    if (!wait_cursor) {
		/* Update the alpha cursor to indicate we are ready
		 * for more input.
		 */
		if (gw)
		    GtWriteAlphaCursor (gw, cur_x, cur_y);
	    }
	    gio_pending = 0;
	    return (1);			/* all done */
	}
}


/* PL_DECODEPTS -- Convert a sequence of textronix encoded polyline vertices
 * into a simple array of [x,y] coordinate pairs.  Each coordinate pair is
 * encoded as a sequence of from 1 to 4 bytes, with bytes being optionally
 * eliminated which do not change from one coordinate pair to the next.  The
 * possible coordinate pair encodings are as follows:
 *
 *	  HIY	  LOY	  HIX	  LOX
 *	01xxxxx	11xxxxx	01xxxxx	10xxxxx
 *	  040	  140	  040	  100
 *
 *	HIY         LOX
 *	HIY LOY     LOX
 *	HIY LOY HIX LOX
 *	    LOY HIX LOX
 *	    LOY     LOX
 *	            LOX
 *
 * In words, bytes which do not change need not be sent, except for the low-x
 * byte (LOX).  If the high-x byte changes, then the low-x byte must also be
 * sent.  The current position, stored as the 4 byte encoding, is cleared to
 * zero when the screen is cleared.
 */
static void
pl_decodepts()
{
	register char	*ip, *itop;
	int	hiy, loy, hix, lox, type, data, nb;
	char	*ip_save;

	for (ip_save=ip=pl_text, itop = &pl_text[pl_op];  ip < itop;  ) {
	    hiy = ohiy;  loy = oloy;
	    hix = ohix;  lox = olox;

	    for (nb=0;  nb < 99 && ip < itop;  nb++) {
		type = (*ip & 0140);
		data = (*ip++ & 037);

		switch (type) {
		case 040:			/* HIY, HIX */
		    if (nb == 0)
			hiy = data;
		    else
			hix = data;
		    break;
		case 0140:			/* LOY */
		    loy = data;
		    break;

		case 0100:
		    /* Receipt of LOX marks the end of the variable length
		     * sequence of bytes required to form the next [x,y].
		     */
		    lox = data;
                    pl_p[pl_npts].x = X_TEK2WIN ((hix << 5) + lox);
                    pl_p[pl_npts].y = Y_TEK2WIN ((hiy << 5) + loy);
                    if (gw && GtGetRaster(gw))
                        pl_p[pl_npts].y = (win_yres-1) - pl_p[pl_npts].y;

		    /* Update current position. */
		    ohiy = hiy;  oloy = loy;
		    ohix = hix;  olox = lox;

		    ip_save = ip;
		    pl_npts++;
		    nb = 99;			/* EXIT */
		    break;
		}
	    }
	}

	/* If there is any data left over (too few bytes to form a coordinate
	 * pair) move these to the start of the buffer.
	 */
	for (pl_op=0, ip=ip_save;  ip < itop;  )
	    pl_text[pl_op++] = *ip++;
}


/* GIO_KEYINPUT -- Called by the Gterm widget when keyboard input occurs.
 * If cursor mode is in effect keyboard input terminates the cursor read,
 * causing a cursor value sequence to be output, otherwise character input is
 * merely passed on.
 */
static void
gio_keyinput (notused, w, event)
XtPointer notused;
Widget	w;
XEvent	*event;
{
        XKeyEvent *xkey = &event->xkey;
        char strbuf[SZ_STRBUF];
        int mapping, raster, sx, sy, rx, ry;
        int nbytes;

        sx = xkey->x;
        sy = xkey->y;

        nbytes = XLookupString (xkey, strbuf, SZ_STRBUF, NULL, NULL);
        if (nbytes > 0) {
            if (wait_cursor) {
		/* Return raster number and raster coordinates of raster
		 * cursor is in, in addition to the usual screen coordinates.
		 */
                if (wincursor) {
		    raster = GtSelectRaster (w, 0, GtPixel, sx, sy, 
			GtNDC, &rx, &ry, &mapping);
		    ry = MAXNDC - ry;
		} else
		    raster = rx = ry = 0;

                gio_retcursor (strbuf[0], sx, sy, raster, rx, ry, 0);
                if (w)
                    GtSetCursorType (w, GtBusyCursor);
            } else
                v_write (pty_fd, strbuf, nbytes);
        }
}


/* GIO_RETCURSOR -- Encode and return a cursor value to the pty (and thence
 * to the program which initiated the cursor read).  Clear the cursor read
 * pending flag so that output processing can resume, and restart the output
 * processing routine.
 */
static int
gio_retcursor (key, sx, sy, raster, rx, ry, datalen)
int	key;			/* key (or whatever) typed to trigger read */
int	sx, sy;			/* screen coords of event */
int	raster;			/* raster number */
int	rx, ry;			/* raster coords of event */
int	datalen;		/* nchars of data following cursor value */
{
	register int n=0, mc_x, mc_y;
	char curval[20];

	/* Ignore cursor events unless requested via program control.
	 */
	if (!wait_cursor)
	    return (-1);

	curval[n++] = key;

        mc_x = (sx > 0) ? X_WIN2TEK(sx) : 0;
        curval[n++] = ((mc_x >> 5) & 037) | 040;
        curval[n++] = ((mc_x     ) & 037) | 040;

        mc_y = (sy > 0) ? Y_WIN2TEK(sy) : 0;
        curval[n++] = ((mc_y >> 5) & 037) | 040;
        curval[n++] = ((mc_y     ) & 037) | 040;

	/* The following optional fields are not part of a standard Tek
	 * cursor return value sequence.
	 */
	if (wincursor) {
	    curval[n++] = ((datalen >> 5) & 037) | 040;
	    curval[n++] = ((datalen     ) & 037) | 040;

	    curval[n++] = ((raster >> 5) & 037) | 040;
	    curval[n++] = ((raster     ) & 037) | 040;

	    mc_x = (rx > 0) ? rx : 0;
	    curval[n++] = ((mc_x >> 10) & 037) | 040;
	    curval[n++] = ((mc_x >>  5) & 037) | 040;
	    curval[n++] = ((mc_x      ) & 037) | 040;

	    mc_y = (ry > 0) ? ry : 0;
	    curval[n++] = ((mc_y >> 10) & 037) | 040;
	    curval[n++] = ((mc_y >>  5) & 037) | 040;
	    curval[n++] = ((mc_y      ) & 037) | 040;
	}

	if (trailer1 >= 0)
	    curval[n++] = trailer1;
	if (trailer2 >= 0)
	    curval[n++] = trailer2;

	v_write (pty_fd, curval, n);

	wait_cursor = 0;
	gio_mode = COMMAND_MODE;

	if (!gio_delay)
	    gio_processdata();
}


/* GIO_RETENQ -- Respond to the ESC ENQ request.
 */
static void
gio_retenq()
{
	register int	mc_x, mc_y;
	char	curval[7];
	int	len;

	/* Graphics status word. */
	curval[0] = (061 | ((gio_mode == ALPHA_MODE) << 2)
			 | ((tx_leftmargin != 0)     << 1));

	/* Alpha cursor position. */
        mc_x = X_WIN2TEK (cur_x);
        mc_y = Y_WIN2TEK (cur_y);

	curval[1] = ((mc_x >> 5) & 037) | 040;
	curval[2] = ((mc_x     ) & 037) | 040;
	curval[3] = ((mc_y >> 5) & 037) | 040;
	curval[4] = ((mc_y     ) & 037) | 040;
	curval[5] = trailer1;
	curval[6] = trailer2;

	len = 5;
	if (trailer1 >= 0) len++;
	if (trailer2 >= 0) len++;
	v_write (pty_fd, curval, len);
}


/* Definitions and data structures for a fast table driven fixed pattern
 * escape sequence recognizer.  Given character I of the sequence there will
 * be N candidate sequences that have matched the first I-1 chars.  Examine
 * each to produce the next list of candidate sequences.  Continue until either
 * a sequence is matched or there are no more candidates.  Variable length
 * sequences such as "ESC[Pl;PcH" are handled as a special case: the general
 * form of these is ESC '[' <digits> [';' <digits>...] LET.
 */
#define	MAX_CANDIDATES	64		/* max candidate escseq		*/
#define	MAX_FIELDS	6		/* max fields in an escseq	*/

struct	_esc {
	char	e_tag;			/* integer code for escseq	*/
	char	e_seq[MAX_FIELDS+1];	/* the sequence itself		*/
};

static	struct	_esc *e_cand1[MAX_CANDIDATES];	/* 1st candidates array */
static	struct	_esc *e_cand2[MAX_CANDIDATES];	/* 2nd candidates array	*/
static	struct	_esc **e_pcand, **e_acand;	/* candidates arrays	*/
static	int	e_npcand, e_nacand;		/* number of candidates	*/
static	int	e_charno;			/* char being examined	*/
static	int	scanok;				/* clr if decode fails  */
static	int	startscan(), getint(), getstr(), endscan();

static	struct _esc e_table[] = {
#include "gtermio.esc"			/* Gterm escape sequence table	*/
	{ 0, 0,0,0,0,0,0,0 }
};


/* GIO_ESCAPE -- Recognize and process graphics escape sequences, i.e.,
 * all multicharacter command codes beginning with ESC.  The simple single
 * character command codes are handled directly by the data processing code.
 * The escapes have no well defined pattern to them, hence we must simply
 * consume characters until a legal escape sequence is recognized or the
 * sequence is found to not match any known sequence.  It is possible that
 * all of the characters forming a sequence will not yet have been deposited
 * in the input buffer, in which case we return -1, indicating to our caller
 * that we should be called back later to rescan the same input, when more
 * data becomes available.  Otherwise, we take whatever action is implied
 * for the escape sequence and return the new mode to the interpreter code.
 * If an unrecognized escape sequence is encountered it is discarded and we
 * return in alpha mode so that subsequent input appears as garbage on the
 * screen.
 */
static int
gio_escape()
{
	register struct	_esc *esc;
	register int ch, i, j;
	struct	_esc **e_temp;
	int tag;

	/* Discard the ESC and get the first char. */
	g_getc (ch);
	if (g_getc (ch) < 0)
	    return (-1);

	/* Build the initial list of candidates. This is the most expensive
	 * step, since all sequences must be examined.
	 */
	for (esc=e_table, e_pcand=e_cand1, e_npcand=0;  esc->e_tag;  esc++)
	    if (ch == esc->e_seq[0]) {
		if (esc->e_seq[1] == 0) {
		    tag = esc->e_tag;
		    goto action;
		}
		e_pcand[e_npcand++] = esc;
	    }

	/* If there were no candidates, we are done. */
	if (e_npcand == 0) {
	    g_ungetc (ch);
	    return (ALPHA_MODE);
	}

	/* Examine successive characters from the input, building a new,
	 * shorter candidate list on each iteration.  This should converge
	 * very rapidly one way or the other.
	 */
	for (j=1, e_acand=e_cand2;  j < MAX_FIELDS && e_npcand > 0;  j++) {
	    if (g_getc(ch) < 0)
		return (-1);

	    /* Examine the next character of each sequence in the list of
	     * candidate sequences.  If we have a complete match, we are
	     * done, else if we have a single character match add the seq
	     * to the new candidates list.
	     */
	    e_nacand = 0;
	    for (i=0;  i < e_npcand;  i++) {
		esc = e_pcand[i];
		if (ch == esc->e_seq[j]) {
		    if (esc->e_seq[j+1] == 0) {
			tag = esc->e_tag;
			goto action;
		    }
		    e_acand[e_nacand++] = esc;
		}
	    }

	    e_temp = e_pcand; e_pcand = e_acand; e_acand = e_temp;
	    e_npcand = e_nacand;
	}

	/* If the escape sequence was recognized the above code should have
	 * vectored off to the action marker below.  If we fall through the
	 * loop it can only mean that we have an unrecognized escape sequence,
	 * so discard it and return in command mode.
	 */
	g_ungetc (ch);
	return (ALPHA_MODE);

action:
	/* Process the escape sequence. */
	switch (tag) {
	case ESC_SETTEXTMODE:
	    sl_x = 0;
	    tx_len = 0;
	    if (gw)
		GtStartDialog (gw);
	    return (TEXT_MODE);

	case ESC_ENQUIRE:
	    gio_retenq();
	    break;
	case ESC_READCURSOR:
	    wincursor = 0;
	    return (CURSOR_MODE);
	case ESC_WINCURSOR:
	    wincursor = 1;
	    return (CURSOR_MODE);
	case ESC_SETCURSOR:
	    if (gw)
		GtSetCursorPos (gw, cur_x, cur_y);
	    break;

	case ESC_CLEARSCREEN:
	    if (gw) {
		GtClearScreen (gw);
		GtSetRaster (gw, 0);
	    }
	    tx_leftmargin = 0;
	    cur_x = tx_leftmargin;
	    cur_y = tx_charbase;
	    ohiy = 0; oloy = 0;
	    ohix = 0; olox = 0;
	    gio_datalevel = SET_BITS;
	    pl_pointmode = 0;
	    pl_areamode = 0;
	    return (ALPHA_MODE);

	case ESC_SETCHARSIZE0:
	case ESC_SETCHARSIZE1:
	case ESC_SETCHARSIZE2:
	case ESC_SETCHARSIZE3:
	    /* Ignore these for now. */
	    break;

	case ESC_SETDATALEVEL0:
	    if (gw)
		GtSetDataLevel (gw, GtSet);
	    break;
	case ESC_SETDATALEVEL1:
	    if (gw)
		GtSetDataLevel (gw, GtClear);
	    break;
	case ESC_SETDATALEVEL2:
	    if (gw)
		GtSetDataLevel (gw, GtInvert);
	    break;

	case ESC_SETLINESTYLE0:
	    if (gw)
		GtSetLineStyle (gw, GtSolid);
	    break;
	case ESC_SETLINESTYLE1:
	    if (gw)
		GtSetLineStyle (gw, GtDashed);
	    break;
	case ESC_SETLINESTYLE2:
	    if (gw)
		GtSetLineStyle (gw, GtDotted);
	    break;
	case ESC_SETLINESTYLE3:
	    if (gw)
		GtSetLineStyle (gw, GtDashDot);
	    break;
	case ESC_SETLINESTYLE4:
	    if (gw)
		GtSetLineStyle (gw, GtDash3Dot);
	    break;

	case ESC_SETLINEWIDTH0:
	    if (gw)
		GtSetLineWidth (gw, 1);
	    break;
	case ESC_SETLINEWIDTH1:
	    if (gw)
		GtSetLineWidth (gw, 2);
	    break;
	case ESC_SETLINEWIDTH2:
	    if (gw)
		GtSetLineWidth (gw, 3);
	    break;

	case ESC_SETCOLOR0:
	    if (gw)
		GtSetColorIndex (gw, 0);
	    break;
	case ESC_SETCOLOR1:
	    if (gw)
		GtSetColorIndex (gw, 1);
	    break;
	case ESC_SETCOLOR2:
	    if (gw)
		GtSetColorIndex (gw, 2);
	    break;
	case ESC_SETCOLOR3:
	    if (gw)
		GtSetColorIndex (gw, 3);
	    break;
	case ESC_SETCOLOR4:
	    if (gw)
		GtSetColorIndex (gw, 4);
	    break;
	case ESC_SETCOLOR5:
	    if (gw)
		GtSetColorIndex (gw, 5);
	    break;
	case ESC_SETCOLOR6:
	    if (gw)
		GtSetColorIndex (gw, 6);
	    break;
	case ESC_SETCOLOR7:
	    if (gw)
		GtSetColorIndex (gw, 7);
	    break;
	case ESC_SETCOLOR8:
	    if (gw)
		GtSetColorIndex (gw, 8);
	    break;
	case ESC_SETCOLOR9:			/* see also SETCOLORN */
	    if (gw)
		GtSetColorIndex (gw, 9);
	    break;

	/* Imaging escape sequences.  These are encoded as follows:
	 *
	 * 	ESC r <code> [ P ; P ; ... ]	<data>
	 *
	 * By the time we get here the input is positioned to just before the
	 * first '[' as only the fixed pattern part of the escape will have
	 * been recognized.
	 *
	 * Imaging instructions consist of a fixed pattern escape sequence
	 * identifying the instruction, followed by a fixed number of ASCII
	 * encoded parameters, followed in some cases by a variable-sized data
	 * field.
	 *
	 * Pixels are encoded as ascii codes by adding 040 to the value of each
	 * pixel.  The range of possible pixel values is about 200.  The EC
	 * field in the read/write pixels instruction specifies the encoding
	 * type; this field is ignored at present and should be zero.
	 *
	 * The query raster instruction returns the raster size as a flag 0/1
	 * indicating whether the given raster exists, followed by pair of
	 * decimal encoded ascii numbers giving the raster size.  Raster 0 is
	 * the gterm window hence this function may be used to query the window
	 * size.  The create raster instruction may be used to request that the
	 * window size be changed; the server may or may not permit such a
	 * resize request to succeed.
	 *
	 * Colors are RGB color intensity triplets.  Intensities are 8 bit
	 * values (0-255) encoded as two bytes, 4 bits plus 040 in each byte.
	 *
	 * Return values are encoded the same as input data, with a two byte
	 * trailer following the data (usually CR).
	 */

	case ESC_SETRESET:
	    {	/* parameters: reset-str */
		if (startscan() || getstr(s_reset) || endscan())
		    return (-1);
	    }
	    break;

	case ESC_SETRESIZE:
	    {	/* parameters: resize-str */
		if (startscan() || getstr(s_resize) || endscan())
		    return (-1);
	    }
	    break;

	case ESC_RASTERINIT:
	    if (gw)
		GtRasterInit (gw);
	    break;

	case ESC_CREATERASTER:
	    {	/* parameters: RN RT NX NY BP */
		int raster, type, width, height, depth;
		if (startscan())
		    return (-1);
		if (getint(&raster) || getint(&type) ||
			getint(&width) || getint(&height) || getint(&depth))
		    return (-1);
		if (endscan())
		    return (-1);
		if (gw && scanok)
		    GtCreateRaster (gw, raster, type, width, height, depth);
	    }
	    break;

	case ESC_DESTROYRASTER:
	    {	/* parameters: RN */
		int raster;
		if (startscan() || getint(&raster) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtDestroyRaster (gw, raster);
	    }
	    break;

	case ESC_QUERYRASTER:
	    {	/* parameters: RN */
		int status, raster, type, width, height, depth;
		char obuf[80];

		if (startscan() || getint(&raster) || endscan())
		    return (-1);
		if (gw) {
		    if (!GtReady (gw)) {
			gio_delay = 1;
			return (-1);
		    }
		    status = GtQueryRaster (gw, raster,
			&type, &width, &height, &depth);
		}

		sprintf (obuf, "\033[5;%d;%d;%d;%d;%d]",
		    status, type, width, height, depth);
		v_write (pty_fd, obuf, strlen(obuf));
	    }
	    break;

	case ESC_SETRASTER:
	    {	/* parameters: RN */
		int raster;
		if (startscan() || getint(&raster) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtSetRaster (gw, raster);
	    }
	    break;

	case ESC_WRITEPIXELS:
	    {	/* parameters: RN EC X1 Y1 NX NY BP (NX*NY pixels follow) */
		if (startscan())
		    return (-1);
		if (getint(&wi_raster) || getint(&wi_encoding) ||
			getint(&wi_x1) || getint(&wi_y1) ||
			getint(&wi_nx) || getint(&wi_ny) || getint(&wi_bp))
		    return (-1);
		if (endscan())
		    return (-1);
		msg_op = 0;
		return (scanok ? WIMAGE_MODE : COMMAND_MODE);
	    }
	    break;

	case ESC_READPIXELS:
	    {	/* parameters: RN EC X1 Y1 NX NY BP (return NX*NY pixels) */
		int raster, encoding, x1, y1, nx, ny, nbits, npix=0;
		register unsigned char *data, *op;
		unsigned char obuf[128];
		register int i;

		if (startscan())
		    return (-1);
		if (getint(&raster) || getint(&encoding) || getint(&x1) ||
			getint(&y1) || getint(&nx) || getint(&ny) ||
			getint(&nbits))
		    return (-1);
		if (endscan())
		    return (-1);

		if (scanok) {
		    npix = nx * ny;
		    if ((data = (unsigned char *) malloc (npix)) == NULL)
			return (-1);
		    if (gw && scanok) {
			if (GtReadPixels (gw, raster, data,
				nbits, x1, y1, nx, ny) == ERR)
			    npix = 0;
		    }
		}

		/* Return the pixels bracked by ESC and the trailers.  This is
		 * done even if no data is returned, e.g. due to a parameter
		 * error or read error.
		 */
		op = obuf;
		*op++ = '\033';
		for (i=0, op=obuf;  i < npix;  i++) {
/*		    *op++ = data[i] + 040;*/
		    *op++ = ((data[i] >> 4) & 017) + 040;
		    *op++ = ((data[i]     ) & 017) + 040;
		    if (op - obuf > 100) {
			v_write (pty_fd, obuf, op-obuf);
			op = obuf;
		    }
		}
		if (trailer1 >= 0) *op++ = trailer1;
		if (trailer2 >= 0) *op++ = trailer2;
		v_write (pty_fd, obuf, op-obuf);

		if (scanok)
		    free (data);
	    }
	    break;

	case ESC_REFRESHPIXELS:
	    {	/* parameters: RN CT X1 Y1 NX NY */
		int x1, y1, nx, ny;
		int raster, ct;

		if (startscan())
		    return (-1);
		if (getint(&raster) || getint(&ct) ||
			getint(&x1) || getint(&y1) ||
			getint(&nx) || getint(&ny))
		    return (-1);
		if (endscan())
		    return (-1);

		if (gw && scanok)
		    GtRefreshPixels (gw, raster, ct, x1,y1,nx,ny);
	    }
	    break;

	case ESC_SETPIXELS:
	    {	/* parameters: RN CT X1 Y1 NX NY CO OP */
		int raster, ct, co, op;
		int x1, y1, nx, ny;

		if (startscan())
		    return (-1);
		if (getint(&raster) || getint(&ct) ||
			getint(&x1) || getint(&y1) ||
			getint(&nx) || getint(&ny) ||
			getint(&co) || getint(&op))
		    return (-1);
		if (endscan())
		    return (-1);

		if (gw && scanok)
		    GtSetPixels (gw, raster, ct, x1,y1,nx,ny, co,op);
	    }
	    break;

	case ESC_WRITECMAP:
	    {	/* parameters: MP FC NC (NC color triplets follow) */

		if (startscan() || getint(&wc_map) || getint(&wc_first) ||
			getint(&wc_ncolors) || endscan())
		    return (-1);

		msg_op = 0;
		if (wc_ncolors > MAX_COLORS)
		    wc_ncolors = MAX_COLORS;
		return (scanok ? WCMAP_MODE : COMMAND_MODE);
	    }
	    break;

	case ESC_READCMAP:
	    {	/* parameters: MP FC NC (return NC color triplets) */
		int map, first, ncolors, buflen;
		unsigned short *buf, *r, *g, *b, v[3];
		unsigned char obuf[128];
		register unsigned char *op;
		register int i, j;

		if (startscan() || getint(&map) || getint(&first) ||
			getint(&ncolors) || endscan())
		    return (-1);

		/* Get the colormap data into a buffer. */
		if (scanok) {
		    buflen = ncolors * 3 * sizeof (unsigned short);
		    if ((buf = (unsigned short *) malloc (buflen)) == NULL)
			return (-1);
		    b = (g = (r = buf) + ncolors) + ncolors;
		    if (gw) {
			ncolors = GtReadColormap (gw, map,first,ncolors,r,g,b);
			if (ncolors == ERR)
			    ncolors = 0;
		    } else
			ncolors = 0;
		} else
		    ncolors = 0;

		/* Return the encoded colors bracked by ESC and the trailers.
		 * This is done even if no data is returned, e.g. due to a
		 * parameter error or read error.
		 */
		op = obuf;
		*op++ = '\033';
		for (i=0, op=obuf;  i < ncolors;  i++) {
		    v[0] = (r[i] >> 8); v[1] = (g[i] >> 8); v[2] = (b[i] >> 8);
		    for (j=0;  j < 3;  j++) {
			*op++ = ((v[j] >> 4) & 017) + 040;
			*op++ = ((v[j]     ) & 017) + 040;
		    }

		    if (op - obuf > 100) {
			v_write (pty_fd, obuf, op-obuf);
			op = obuf;
		    }
		}
		if (trailer1 >= 0) *op++ = trailer1;
		if (trailer2 >= 0) *op++ = trailer2;
		v_write (pty_fd, obuf, op-obuf);

		if (scanok)
		    free (buf);
	    }
	    break;

	case ESC_LOADCMAP:
	    {	/* parameters: MP OF DX DY */
		int colormap, gki_offset, dx, dy;
		float offset, slope;

		if (startscan() || getint(&colormap) || getint(&gki_offset) ||
			getint(&dx) || getint(&dy) || endscan())
		    return (-1);

		offset = (float)gki_offset / (float)((MAXNDC + 1) / 4);
		slope  = dx ? (float)dy / (float)dx : MAXNDC;

		if (gw && scanok)
		    GtLoadColormap (gw, colormap, offset, slope);
	    }
	    break;

	case ESC_FREECMAP:
	    {	/* parameters: MP */
		int colormap;
		if (startscan() || getint(&colormap) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtFreeColormap (gw, colormap);
	    }
	    break;

	case ESC_WRITEIOMAP:
	    {	/* parameters: FC NC (NC color triplets follow) */

		if (startscan() || getint(&wc_first) ||
			getint(&wc_ncolors) || endscan())
		    return (-1);

		msg_op = 0;
		if (wc_ncolors > MAX_COLORS)
		    wc_ncolors = MAX_COLORS;
		return (scanok ? WIOMAP_MODE : COMMAND_MODE);
	    }
	    break;

	case ESC_READIOMAP:
	    {	/* parameters: FC NC (return NC color triplets) */
		int first, ncolors, buflen;
		unsigned short *iomap, v;
		unsigned char obuf[128];
		register unsigned char *op;
		register int i;

		if (startscan() || getint(&first) ||
			getint(&ncolors) || endscan())
		    return (-1);

		/* Get the iomap data into a buffer. */
		if (scanok) {
		    buflen = ncolors * sizeof (unsigned short);
		    if ((iomap = (unsigned short *) malloc (buflen)) == NULL)
			return (-1);
		    if (gw) {
			GtReadIomap (gw, iomap, first, ncolors);
		    } else
			ncolors = 0;
		} else
		    ncolors = 0;

		/* Return the encoded array bracked by ESC and the trailers.
		 * This is done even if no data is returned, e.g. due to a
		 * parameter error or read error.
		 */
		op = obuf;
		*op++ = '\033';
		for (i=0, op=obuf;  i < ncolors;  i++) {
		    v = iomap[i];
		    *op++ = ((v >> 4) & 017) + 040;
		    *op++ = ((v     ) & 017) + 040;

		    if (op - obuf > 100) {
			v_write (pty_fd, obuf, op-obuf);
			op = obuf;
		    }
		}
		if (trailer1 >= 0) *op++ = trailer1;
		if (trailer2 >= 0) *op++ = trailer2;
		v_write (pty_fd, obuf, op-obuf);

		if (scanok)
		    free (iomap);
	    }
	    break;

	case ESC_SETCOLORN:
	    {	/* parameters: CN */
		int color;
		if (startscan() || getint(&color) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtSetColorIndex (gw, color);
	    }
	    break;

	case ESC_SETLINEWIDTHN:
	    {	/* parameters: LW */
		int width;
		if (startscan() || getint(&width) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtSetLineWidth (gw, width);
	    }
	    break;

	case ESC_INITMAPPINGS:
	    if (gw)
		GtInitMappings (gw);
	    break;

	case ESC_COPYRASTER:
	    {	/* parameters: OP SR ST SX SY SW SH DR DT DX DY DW DH */
		int src, dst, rop;
		int st, sx, sy, sw, sh;
		int dt, dx, dy, dw, dh;

		if (startscan())
		    return (-1);
		if (getint(&rop) || getint(&src) || getint(&st))
		    return (-1);
		if (getint(&sx) || getint(&sy) || getint(&sw) || getint(&sh))
		    return (-1);
		if (getint(&dst) || getint(&dt))
		    return (-1);
		if (getint(&dx) || getint(&dy) || getint(&dw) || getint(&dh))
		    return (-1);
		if (endscan())
		    return (-1);

		if (gw && scanok)
		    GtCopyRaster (gw, rop,
			src, st, sx, sy, sw, sh,
			dst, dt, dx, dy, dw, dh);
	    }
	    break;

	case ESC_SETMAPPING:
	    {	/* parameters: MP OP SR ST SX SY SW SH DR DT DX DY DW DH */
		int mapping, rop, src, st, sx, sy, sw, sh;
		int dst, dt, dx, dy, dw, dh;

		if (startscan())
		    return (-1);
		if (getint(&mapping) || getint(&rop))
		    return (-1);
		if (getint(&src) || getint(&st))
		    return (-1);
		if (getint(&sx) || getint(&sy) || getint(&sw) || getint(&sh))
		    return (-1);
		if (getint(&dst) || getint(&dt))
		    return (-1);
		if (getint(&dx) || getint(&dy) || getint(&dw) || getint(&dh))
		    return (-1);
		if (endscan())
		    return (-1);

		if (gw && scanok)
		    GtSetMapping (gw, mapping, rop,
			src, st, sx, sy, sw, sh,
			dst, dt, dx, dy, dw, dh);
	    }
	    break;

	case ESC_GETMAPPING:
	    {	/* parameters: MP (return mapping) */
		int mapping, rop, enable;
		int src, st, sx, sy, sw, sh;
		int dst, dt, dx, dy, dw, dh;
		char obuf[128];

		if (startscan() || getint(&mapping) || endscan())
		    return (-1);

		enable = src=st=sx=sy=sw=sh = dst=dt=dx=dy=dw=dh = 0;
		if (gw && scanok) {
		    enable = GtGetMapping (gw, mapping, &rop,
			&src, &st, &sx, &sy, &sw, &sh,
			&dst, &dt, &dx, &dy, &dw, &dh);
		}

		sprintf (obuf,
		    "\033[6;%d;%d %d;%d;%d;%d;%d;%d %d;%d;%d;%d;%d;%d]",
		    enable + 1, rop,
		    src, st, sx, sy, sw, sh,
		    dst, dt, dx, dy, dw, dh);
		v_write (pty_fd, obuf, strlen(obuf));
	    }
	    break;

	case ESC_ENABLEMAPPING:
	    {	/* parameters: MP FL */
		int mapping, flags;
		if (startscan() || getint(&mapping) ||
			getint(&flags) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtEnableMapping (gw, mapping, flags);
	    }
	    break;

	case ESC_DISABLEMAPPING:
	    {	/* parameters: MP FL */
		int mapping, flags;
		if (startscan() || getint(&mapping) ||
			getint(&flags) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtDisableMapping (gw, mapping, flags);
	    }
	    break;

	case ESC_REFRESHMAPPING:
	    {	/* parameters: MP */
		int mapping;
		if (startscan() || getint(&mapping) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtRefreshMapping (gw, mapping);
	    }
	    break;

	case ESC_FREEMAPPING:
	    {	/* parameters: MP */
		int mapping;
		if (startscan() || getint(&mapping) || endscan())
		    return (-1);
		if (gw && scanok)
		    GtFreeMapping (gw, mapping);
	    }
	    break;

	default:
	    ;
	}

	return (COMMAND_MODE);
}


/* STARTSCAN -- Reset the scanok flag at the start of a scan.
 */
static int
startscan()
{
	register int ch;

	/* Skip forward to the '[' preceeding the first argument. */
	while (g_getc (ch) >= 0)
	    if (ch == '[') {
		scanok = 1;
		return (0);
	    }

	scanok = 0;
	return (1);
}


/* GETINT -- Get the next integer token from the input stream.  A nonzero
 * value is returned if the input is exhausted.  The scanok flag is cleared
 * if a decode error occurs.
 */
static int
getint (value)
int *value;
{
	register int ch;
	register int v;
	int	neg = 0;

	if (scanok) {
	    /* Skip to the next integer token. */
	    for (;;) {
		if (g_getc (ch) < 0)
		    return (1);
		if (isdigit(ch) || ch == '-')
		    break;
		else if (ch == '[' || ch == ' ' || ch == ';')
		    ;
		else if (ch < 040) {
		    g_ungetc (ch);
		    scanok = 0;
		    return (1);
		}
	    }

	    /* Accumulate the integer value. */
	    if (ch == '-') {
		neg++;
		g_getc (ch);
	    } else
		neg = 0;
	    if (isdigit (ch)) {
		for (v = ch - '0';  ;  ) {
		    if (g_getc (ch) < 0)
			return (1);
		    if (isdigit(ch))
			v = v * 10 + ch - '0';
		    else {
			*value = neg ? -v : v;
			g_ungetc (ch);
			break;
		    }
		}
	    }
	}

	return (0);
}


/* GETSTR -- Get the next string token from the input stream.  A nonzero
 * value is returned if the input is exhausted.  The scanok flag is cleared
 * if a decode error occurs.
 */
static int
getstr (value)
char *value;
{
	register int ch;
	register char *op = value;

	if (scanok) {
	    /* Skip to the next string token. */
	    for (;;) {
		if (g_getc (ch) < 0)
		    return (1);
		if (ch && ch != '[' && ch != ' ' && ch != ';')
		    break;
	    }

	    /* Accumulate the string value. */
	    for (*op++ = ch;  ;  ) {
		if (g_getc (ch) < 0)
		    return (1);
		if (!isspace(ch) && ch != ';' && ch != ']')
		    *op++ = ch;
		else {
		    *op = '\0';
		    g_ungetc (ch);
		    break;
		}
	    }
	}

	return (0);
}


/* ENDSCAN -- Scan forward to the ']' input argument list delimiter.
 */
static int
endscan()
{
	register int ch;

	/* Skip to the ']' delimiter. */
	for (;;) {
	    if (g_getc (ch) < 0)
		return (1);
	    if (isdigit(ch) || ch == ' ' | ch == ';')
		;
	    else if (ch == ']')
		break;
	    else if (ch < 040) {
		g_ungetc (ch);
		scanok = 0;
		break;
	    }
	}

	return (0);
}
