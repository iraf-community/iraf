/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <suntool/sunview.h>
#include <pixrect/pr_line.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <stdio.h>
#include "gterm.h"

/*
 * GTERMIO -- Graphics terminal i/o.  This is the low level code which
 * filters graphics output out of the pseudoterminal output stream and
 * then processes the graphics instructions, drawing into the graphics
 * pixwin.  Graphics output from the pty is written into a circular
 * buffer by a low level routine called when data is read from the pty
 * by the ttysw code.  A differed routine is called back by the notifier
 * to process the data, writing graphics into the gio pixwin.  The graphics
 * data language implemented is tektronix standard plus extensions.
 */

extern	int clip_graphics;
extern	int cursor_show;
extern	int gio_canvas;
extern	FILE *gt_logfp;
int	gio_graphicsenabled = 0;	/* switch text/graphics output	*/
int	gio_enabled = 1;		/* enable graphics window	*/

/* Size limiting definitions. */
#define	SZ_GBUF		8192		/* max buffered graphics data	*/
#define	GB_MINSPACE	2048		/* XOFF when this much left	*/
#define	GB_BIGSPACE	3072		/* XON when this much available */
#define	MAX_PLPTS	4096		/* max points in a polyline	*/
#define	SZ_TXBUF	256		/* max chars in a polytext	*/
#define	MAX_TEXTCHARS	80		/* max chars in text line	*/
#define	SL_XOFFSET	0		/* x offset to status line	*/
#define	SL_YOFFSET	5		/* y offset to status line	*/
#define	MAX_QUOTA	512		/* limit for one proc. loop	*/
#define	WSOPEN_DELAY	100		/* delay at openws (msec)	*/
#define	WSCLOSE_DELAY	0		/* delay at closews (msec)	*/
#define	LOG_SYNCTIME	15		/* sync interval for logfile	*/

/* Magic numbers. */
#define	SET_BITS	0		/* draw vectors|points		*/
#define	CLEAR_BITS	1		/* erase vectors|points		*/
#define	TOGGLE_BITS	2		/* toggle data bits		*/
#define	COMMAND_MODE	0		/* initial state		*/
#define	ALPHA_MODE	1		/* tek-alpha character drawing	*/
#define	TEXT_MODE	2		/* output to status line	*/
#define	VECTOR_MODE	3		/* draw vectors or points	*/
#define	CURSOR_MODE	4		/* read crosshair cursor posn	*/
#define	BREAK_LINE	(-2)		/* special automargin code	*/

/* ASCII codes. */
#define	ENQ		'\005'
#define	BEL		'\007'
#define	CR		'\015'
#define	CAN		'\030'
#define	SUB		'\032'
#define	ESC		'\033'
#define	FS		'\034'
#define	GS		'\035'
#define	RS		'\036'
#define	US		'\037'

/* Pseudoterminal i/o.
 */
static int	pty_fd;			/* fd of pseudoterminal		*/
static int	pty_stop = 0;		/* set when XOFF is set on pty	*/

/* The graphics data buffer, a circular buffer.
 */
static	char	g_buf[SZ_GBUF];		/* circular buffer		*/
static	char	*g_top= &g_buf[SZ_GBUF];/* end of buffer + 1		*/
static	char	*g_ip = g_buf;		/* input pointer		*/
static	char	*g_op = g_buf;		/* output pointer		*/

#define g_getc(c)\
	(g_ip==g_op ? -1 : ((c) = *g_ip++, g_ip >= g_top ? *(g_ip=g_buf):0))
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

/* Polyline (polymarker) output-point buffer.
 */
static	char	pl_text[MAX_PLPTS];	/* encoded [x,y] coord data	*/
static	struct	pr_pos pl_p[MAX_PLPTS];	/* polyline storage		*/
static	int	pl_npts = 0;		/* npoints in polyline		*/
static	int	pl_op = 0;		/* which char in coord pair	*/
static	int	pl_linestyle = 0;	/* dashline drawing style	*/
static	int	pl_pointmode = 0;	/* point or line mode		*/

static	int	ohiy=0; oloy=0;		/* encoded current position	*/
static	int	ohix=0; olox=0;

static	Pr_brush brush = { 1 };
#define	pl_linewidth brush.width	/* vector drawing linewidth	*/

/* Graphics text variables.
 */
static	struct	fonttab *alpha_font;	/* alpha mode font		*/
static	struct	fonttab *text_font;	/* text mode font		*/
static	char	tx_buf[SZ_TXBUF+1];	/* polytext text buffer		*/
static	int	tx_len = 0;		/* nchars in buffer		*/
static	int	tx_maxlines;		/* nlines of text on a screen	*/
static	int	tx_maxcols;		/* ncols of text on a screen	*/
static	int	tx_charheight;		/* height of a char in pixels	*/
static	int	tx_charwidth;		/* width of a char in pixels	*/
static	int	tx_charbase;		/* topline to baseline distance	*/
static	int	tx_leftmargin;		/* where columns start		*/
static	int	sl_x, sl_y;		/* current pos. in status line	*/
static	int	sl_xoff, sl_yoff;	/* x,y offset of status line	*/
static	int	sl_cwidth;		/* status line char width	*/
static	int	sl_cheight;		/* status line char height	*/
static	int	sl_cbase;		/* topline to baseline distance	*/
static	int	sl_width;		/* status line rect width	*/
static	int	sl_height;		/* status line rect height	*/
static	int	sl_rect_saved = 0;	/* set after sl rect is saved	*/
static	struct	pixrect *sl_pr=NULL;	/* saved status line pixrect	*/
static	struct	fonttab *chcur_font;	/* character cursor font	*/
static	int	chcur_x;		/* character cursor xpos	*/
static	int	chcur_y;		/* character cursor ypos	*/
static	int	chcur_on = 0;		/* character cursor displayed?	*/
static	int	chcur_skip = 0;		/* used to skip cursor updates	*/

/* Miscellaneous variables.
 */
static	struct	pixwin *pw;		/* graphics pixwin		*/
static	struct	rect pw_r;		/* full rect of the pixwin	*/
static	int	cur_x, cur_y;		/* current x,y position		*/
static	int	win_xres, win_yres;	/* size of graphics pixwin	*/
static	int	tek_xres;		/* X resolution of input data	*/
static	int	tek_yres;		/* Y resolution of input data	*/
static	int	trailer1 = '\r';	/* trailer code, cursor value	*/
static	int	trailer2 = -1;		/* second trailer code (opt)	*/
static	int	gio_mode=COMMAND_MODE;	/* graphics drawing mode	*/
static	int	gio_datalevel=SET_BITS;	/* set, clear, or toggle bits	*/
static	int	workstation_open = 0;	/* have issued open workstation	*/
static	int	wait_cursor = 0;	/* waiting for cursor input	*/
static	int	gio_delay = 0;		/* programmed delay in progress	*/

int	ev_ptyoutput();
static	Pr_texture *pl_texture();
static	Notify_value ev_gioprocessdata();

/* Macros to convert between tektronix and window coordinates. */
#define	X_TEK2WIN(x)  (            ((x)  * win_xres + tek_xres/2) / tek_xres)
#define	Y_TEK2WIN(y)  (win_yres - (((y)  * win_yres + tek_yres/2) / tek_yres))
#define	X_WIN2TEK(x)  (((           (x)) * tek_xres + win_xres/2) / win_xres)
#define	Y_WIN2TEK(y)  (((win_yres - (y)) * tek_yres + win_yres/2) / win_yres)


/* GIO_SETUP -- Called by the high level Gterm window management code to
 * give the gtermio code the file descriptor of the pty and the pixwin of
 * the graphics frame.
 */
gio_setup (fd, gio_pw)
int	fd;			/* fd of pty			*/
struct	pixwin *gio_pw;		/* graphics pixwin		*/
{
	pty_fd = fd;
	pw = gio_pw;

	notify_read_post_monitor_fcn (pty_fd, ev_ptyoutput);
	notify_set_event_func (ev_gioprocessdata,
	    ev_gioprocessdata, NOTIFY_SAFE);
}


/* GIO_HARDRESET -- Reset everything, including cancelling any cursor read
 * that may be in progress.
 */
gio_hardreset (mc_xres, mc_yres, a_font, t_font) 
int	mc_xres, mc_yres;	/* virtual x,y resolution	*/
struct	fonttab *a_font;	/* alpha mode font		*/
struct	fonttab *t_font;	/* text mode font		*/
{
	gio_mode = COMMAND_MODE;
	gio_graphicsenabled = 0;
	workstation_open = 0;
	wait_cursor = 0;
	gio_delay = 0;
	pty_stop = 0;

	ioctl (pty_fd, TIOCSTART, NULL);
	gio_reset (mc_xres, mc_yres, a_font, t_font);
}


/* GIO_RESET -- Reset the state of the gtermio code.  Should be called
 * whenever any important data structures change, e.g., if the graphics
 * frame is resized.
 */
gio_reset (mc_xres, mc_yres, a_font, t_font) 
int	mc_xres, mc_yres;	/* virtual x,y resolution	*/
struct	fonttab *a_font;	/* alpha mode font		*/
struct	fonttab *t_font;	/* text mode font		*/
{
	erase_cursor();
	chcur_skip = 0;

	tek_xres = mc_xres;
	tek_yres = mc_yres;
	alpha_font = a_font;
	text_font = t_font;

	pw_get_region_rect (pw, &pw_r);
	win_xres = pw_r.r_width;
	win_yres = pw_r.r_height;

	tx_leftmargin = 0;
	tx_charwidth  = alpha_font->ch_xsize;
	tx_charheight = alpha_font->ch_ysize;
	tx_charbase   = -alpha_font->pixfont->pf_char['0'].pc_home.y;
	tx_maxlines = win_yres / tx_charheight;
	tx_maxcols  = win_yres / tx_charwidth;
	tx_len      = 0;

	sl_cwidth   = text_font->ch_xsize;
	sl_cheight  = text_font->ch_ysize;
	sl_cbase    = -text_font->pixfont->pf_char['0'].pc_home.y;
	sl_xoff	    = SL_XOFFSET;
	sl_yoff     = win_yres - SL_YOFFSET;
	sl_x	    = sl_xoff;
	sl_y	    = sl_yoff;
	sl_width    = win_xres - sl_xoff;
	sl_height   = sl_cheight;

	if (sl_pr != NULL)
	    pr_destroy (sl_pr);
	sl_pr = mem_create (sl_width, sl_height, 1);
	sl_rect_saved = 0;

	g_top = &g_buf[SZ_GBUF];
	g_ip  = g_op = g_buf;
	pl_npts = 0;
	pl_op = 0;
	pl_linestyle = 0;
	pl_linewidth = 1;
	pl_pointmode = 0;
	ohiy = 0; oloy = 0;
	ohix = 0; olox = 0;

	cur_x = tx_leftmargin;
	cur_y = tx_charbase;
}


/* GIO_ENABLE -- Enable or disable the graphics window.  If graphics is
 * disabled, all i/o is directed to the text window.
 */
gio_enable (onoff)
int	onoff;
{
	if ((gio_enabled = onoff) == GRAPHICS_OFF)
	    gio_graphicsenabled = 0;
}


/* GIO_SETGINMODETERM -- Set the GIN mode (cursor read) trailer codes,
 * expressed as octal constants in the input string argument.
 */
gio_setginmodeterm (str)
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
}


/* EV_PTYOUTPUT -- Process pty output packets.  Output directed to the
 * terminal (/dev/tty) by the applications program appears as read-pending
 * events on the pty seen by the Gterm program.  We let the TTY code monitor
 * the pty and respond to read-pending events.  The low level read primitive
 * (notify_read) ultimately called to service a read request by TTY reads
 * the data and then calls us to process the data packet.  We extract any
 * graphics output from the packet and append it to the gio buffer.  If data
 * is added to the gio buffer a gio-data-pending event is queued so that
 * the graphics drawing code will be called to process the new data.  The
 * remaining data, or a null length packet if the packet contained only
 * graphics data, is returned to TTY, completing the read.  Sometime later
 * the graphics drawing code will be called to process the data.
 */
ev_ptyoutput (ttybuf, nchars)
char	*ttybuf;		/* raw data on input, tty data on output */
int	nchars;			/* nchars of raw data */
{
	register char	*itop = ttybuf + nchars;
	register char	*op, *ip = ttybuf, ch;
	static	unsigned long	oldtime = 0;

	/* Copy to logfile if logging is enabled. */
	if (gt_logfp) {
	    fwrite (ttybuf, nchars, 1, gt_logfp);
	    if (time(0) - oldtime > LOG_SYNCTIME) {
		/* Sync the logfile output every so often. */
		fflush (gt_logfp);
		oldtime = time(0);
	    }
	}

	if (gio_enabled == GRAPHICS_OFF || nchars <= 0)
	    return (nchars);

	/* If in text mode, make a quick scan for the GS character and return
	 * the entire data packet if GS is not seen.
	 */
	if (!gio_graphicsenabled) {
	    while (ip < itop && *ip != GS)
		ip++;
	    if (ip >= itop)
		return (nchars);
	    else {
		gio_graphicsenabled++;
		op = ip;
	    }
	} else
	    op = ttybuf;

	/* Process rest of data in graphics mode.  IP is pointing at the
	 * first char of graphics data, ITOP at the top of the buffer,
	 * and OP at the next tty output char.  Filter out any NULs in
	 * the process of copying the data.
	 */
	while (ip < itop)
	    if (gio_graphicsenabled) {
		while (ip < itop)
		    if ((ch = *ip++) == CAN) {
			g_putc (ch);
			gio_graphicsenabled = 0;
			break;
		    } else if (ch)
			g_putc (ch);
	    } else {
		while (ip < itop)
		    if ((ch = *ip++) == GS) {
			g_putc (ch);
			gio_graphicsenabled = 1;
			break;
		    } else if (ch)
			*op++ = ch;
	    }

	/* If the gio buffer has reached the high-water mark and XOFF is
	 * not currently set, send XOFF to the terminal driver.
	 */
	if (g_spaceleft < GB_MINSPACE && !pty_stop) {
	    ioctl (pty_fd, TIOCSTOP, NULL);
	    pty_stop++;
	}

	/* Post an event with the notifier to call the graphics drawing code
	 * back to process the new data.
	 */
	if (!gio_delay)
	    notify_post_event (ev_gioprocessdata, NULL, NOTIFY_SAFE);

	return (op - ttybuf);
}


/* EV_GIOPROCESSDATA -- Called to process graphics instructions and data from
 * the gio buffer.  This is the routine which actually draws lines and text
 * in the graphics frame.  May be called repeatedly to process any amount of
 * data at a time.  If there is a great amount of data to be processed the
 * routine should return occasionally to allow the other GTERM event handlers
 * to run (operation is not fully asynchronous).
 *
 * Graphics data is processed as a stream with no record boundaries, so that
 * operation is not dependent on how data is buffered through the system.
 * The graphics frame is a state machine which is by definition always in a
 * legal state; garbage input causes garbage output, just like a real terminal.
 * The states are as follows:
 *
 *	COMMAND_MODE	This is the initial state.  Characters are accumulated
 *			until a known state is recognized.  Receipt of ESC
 *			always causes command mode to be entered, since
 *			additional characters are needed to define the next
 *			instruction.
 *
 *	ALPHA_MODE	Characters are drawn in the graphics frame at the
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
static Notify_value
ev_gioprocessdata()
{
	register int	quota, ch;
	char	*save_ip, *ip_start;
	int	delay = 0;

	pw_lock (pw, &pw_r);
	g_mark (ip_start);

	/* Process data.
	 */
	for (quota=MAX_QUOTA;  --quota >= 0 && g_getc(ch) >= 0;  ) {
	    if (ch == 0 || gio_enabled == GRAPHICS_DISCARD)
		continue;
again:
	    switch (gio_mode) {
	    case COMMAND_MODE:
		switch (ch) {
		case GS:
		case FS:
		    gio_mode = VECTOR_MODE;
		    pl_npts = 0;
		    pl_op = 0;
		    pl_pointmode = (ch == FS);
		    chcur_skip = -1;
		    if (cursor_show)
			gio_setcursor (CURSOR_OFF, 0);

		    /* Only execute an open workstation if we have not already
		     * done so and if the next command is something other than
		     * close workstation, i.e., no-op sequences GS-CAN are
		     * filtered out, since they would only cause a pointless
		     * switch to the graphics frame and back without drawing.
		     */
		    if (ch == GS && !workstation_open)
			if (g_getc(ch) < 0) {
			    g_ungetc (GS);
			    gio_mode = COMMAND_MODE;
			    goto exit;
			} else if (ch != CAN) {
			    gio_open_workstation();
			    workstation_open = 1;
			    delay = WSOPEN_DELAY;
			    g_ungetc (ch);
			    goto exit;
			}
		    break;

		case US:
		case CR:
		    gio_mode = ALPHA_MODE;
		    tx_len = 0;
		    if (ch == CR)
			goto again;
		    break;

		case CAN:
		    if (workstation_open) {
			pw_unlock (pw);
			gio_close_workstation();
			workstation_open = 0;
			delay = WSCLOSE_DELAY;
		    }
		    gio_mode = COMMAND_MODE;
		    goto exit;

		case ESC:
		    g_ungetc (ch);
		    g_mark (save_ip);
		    erase_cursor();
		    if ((gio_mode = gio_escape()) == -1) {
			gio_mode = COMMAND_MODE;
			g_reset (save_ip);
			goto exit;
		    } else if (gio_mode == CURSOR_MODE)
			goto again;
		    break;

		case BEL:
		    window_bell (gio_canvas);
		    break;

		default:
		    ; /* ignore unknown control chars */
		}
		break;

	    case ALPHA_MODE:
		/* Tek alpha mode is used to write text to random positions on
		 * the screen, or to write lines of text to the gio frame in
		 * "storage scope" mode, where the left and right columns are
		 * alternately written into with an inclusive-or rop.
		 */
		if (ch >= 040) {
		    tx_buf[tx_len++] = ch;
		} else if (ch == '\t') {
		    tx_buf[tx_len++] = 040;
		    if (tx_leftmargin == 0)
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
			erase_cursor();
			pw_text (pw, cur_x, cur_y, gio_rop(),
			    alpha_font->pixfont, tx_buf);
		    }

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
		if (cur_x + (tx_len * tx_charwidth) >= win_xres) {
		    ch = BREAK_LINE;
		    goto flush_alpha;
		}

		break;

	    case TEXT_MODE:
		if (ch >= 040)
		    tx_buf[tx_len++] = ch;
		else if (ch == '\t')
		    tx_buf[tx_len++] = 040;
		else if (ch == '\010' || ch == '\177') {
		    if (tx_len > 0) {
			--tx_len;
			sl_x -= sl_cwidth;
			erase_cursor();
			pw_text (pw, sl_x, sl_y,
			    PIX_SRC, text_font->pixfont, " ");
		    }
		} else {
		    if (tx_len > 0) {
			tx_buf[tx_len] = '\0';
			erase_cursor();
			pw_text (pw, sl_x, sl_y,
			    PIX_NOT(PIX_SRC), text_font->pixfont, tx_buf);
		    }

		    sl_x += tx_len * sl_cwidth;
		    if (sl_x > win_xres - sl_cwidth)
			sl_x = win_xres - sl_cwidth;
		    tx_len = 0;

		    if (ch == '\r' || ch == '\n') {
			sl_x = sl_xoff;
			sl_restore_rect();
		    } else if (ch != 0) {
			gio_mode = COMMAND_MODE;
			goto again;
		    }
		}

		/* Truncate long lines. */
		if (sl_x / sl_cwidth + tx_len >= MAX_TEXTCHARS)
		    if (tx_len > 0)
			--tx_len;
		    else
			sl_x -= sl_cwidth;
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
			pw_polypoint (pw, 0, 0, pl_npts, pl_p,
			    PIX_COLOR(1) | gio_rop());
		    } else if (pl_npts >= 2) {
			/* Must use clipping if dashed line. */
			pw_polyline (pw, 0, 0, pl_npts, pl_p,
			    POLY_DONTCLOSE, &brush, pl_texture(pl_linestyle),
			    (PIX_COLOR(1) | gio_rop()) &
			    ~(pl_linestyle ? PIX_DONTCLIP : 0));
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
		if (wait_cursor++) {
		    g_ungetc (ch);
		    gio_mode = COMMAND_MODE;
		} else
		    gio_readcursor();
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

	update_cursor();
	pw_unlock (pw);

	/* If XOFF is set and the buffer has emptied sufficiently,
	 * send XON to the terminal driver to accept more data.
	 */
	if (pty_stop && g_spaceleft > GB_BIGSPACE) {
	    ioctl (pty_fd, TIOCSTART, NULL);
	    pty_stop = 0;
	}

	/* If there is still data in the buffer (other than a partially
	 * formed escape sequence) post another callback event before exiting
	 * to allow other event handlers to run.
	 */
	if (delay)
	    gio_pause (delay);
	else if (g_havedata && !g_equal(ip_start) && ch != ESC && !wait_cursor)
	    notify_post_event (ev_gioprocessdata, NULL, NOTIFY_SAFE);

	return (NOTIFY_DONE);
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
static
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


/* GIO_PAUSE -- Suspend output for the indicated number of milliseconds, to
 * allow other event processing to catch up.  When the specified interval has
 * passed an ev_gioprocessdata event is posted to resume output processing.
 */
gio_pause (msec)
int	msec;
{
	static	Notify_value ev_restart();
	static	struct itimerval itimer_delay;

	gio_delay = msec;

	itimer_delay.it_interval.tv_usec = 0;
	itimer_delay.it_interval.tv_sec  = 0;

	itimer_delay.it_value.tv_usec = (msec % 1000) * 1000;
	itimer_delay.it_value.tv_sec  = (msec / 1000);

	notify_set_itimer_func (&itimer_delay, ev_restart, ITIMER_REAL,
	    &itimer_delay, NULL);
}


/* EV_RESTART -- Called when the specified interval has passed to restart
 * output processing.
 */
static Notify_value
ev_restart()
{
	gio_delay = 0;
	notify_post_event (ev_gioprocessdata, NULL, NOTIFY_SAFE);

	return (NOTIFY_DONE);
}


/* GIO_RETCURSOR -- Encode and return a cursor value to the pty (and thence
 * to the program which initiated the cursor read).  Clear the cursor read
 * pending flag so that output processing can resume, and post an event to
 * restart the output processing routine.
 */
gio_retcursor (key, x, y)
int	key;			/* key (or whatever) typed to trigger read */
int	x, y;			/* pixwin coords of event */
{
	register int	mc_x, mc_y;
	char	curval[7];
	int	len;

	/* Ignore cursor events unless requested via program control.
	 */
	if (!wait_cursor)
	    return (-1);

	mc_x = X_WIN2TEK (x);
	mc_y = Y_WIN2TEK (y);

	curval[0] = key;
	curval[1] = ((mc_x >> 5) & 037) | 040;
	curval[2] = ((mc_x     ) & 037) | 040;
	curval[3] = ((mc_y >> 5) & 037) | 040;
	curval[4] = ((mc_y     ) & 037) | 040;
	curval[5] = trailer1;
	curval[6] = trailer2;

	len = 5;
	if (trailer1 >= 0) len++;
	if (trailer2 >= 0) len++;
	write (pty_fd, curval, len);

	wait_cursor = 0;
	gio_mode = COMMAND_MODE;
	chcur_skip = -1;

	if (!gio_delay)
	    notify_post_event (ev_gioprocessdata, NULL, NOTIFY_SAFE);
}


/* GIO_RETENQ -- Respond to the ESC ENQ request.
 */
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
	write (pty_fd, curval, len);
}


/* Definitions and data structures for a fast table driven fixed pattern
 * escape sequence recognizer.  Given character I of the sequence there will
 * be N candidate sequences that have matched the first I-1 chars.  Examine
 * each to produce the next list of candidate sequences.  Continue until either
 * a sequence is matched or there are no more candidates.  Variable length
 * sequences such as "ESC[Pl;PcH" are handled as a special case: the general
 * form of these is ESC '[' <digits> [';' <digits>...] LET.
 */
#define	MAX_CANDIDATES	32		/* max candidate escseq		*/
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

static	struct _esc e_table[] = {
#include "gterm.esc"			/* Gterm escape sequence table	*/
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
gio_escape()
{
	register struct	_esc *esc;
	register int	ch, i, j;
	struct	_esc **e_temp;
	int	tag;

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
	    tx_len = 0;
	    sl_x = sl_xoff;
	    if (sl_rect_saved)
		sl_restore_rect();
	    else
		sl_save_rect();
	    return (TEXT_MODE);

	case ESC_ENQUIRE:
	    gio_retenq();
	    break;
	case ESC_READCURSOR:
	    return (CURSOR_MODE);
	case ESC_SETCURSOR:
	    gio_setcursorpos (cur_x, cur_y);
	    break;

	case ESC_CLEARSCREEN:
	    pw_writebackground (pw, 0, 0, win_xres, win_yres, PIX_SRC);
	    tx_leftmargin = 0;
	    cur_x = tx_leftmargin;
	    cur_y = tx_charbase;
	    ohiy = 0; oloy = 0;
	    ohix = 0; olox = 0;
	    gio_datalevel = SET_BITS;
	    pl_linestyle = 0;
	    pl_linewidth = 1;
	    pl_pointmode = 0;
	    sl_rect_saved = 0;
	    chcur_on = 0;
	    chcur_skip = 0;
	    return (ALPHA_MODE);

	case ESC_SETCHARSIZE0:
	case ESC_SETCHARSIZE1:
	case ESC_SETCHARSIZE2:
	case ESC_SETCHARSIZE3:
	    /* Ignore these for now. */
	    break;

	case ESC_SETDATALEVEL0:
	    gio_datalevel = SET_BITS;
	    break;
	case ESC_SETDATALEVEL1:
	    gio_datalevel = CLEAR_BITS;
	    break;
	case ESC_SETDATALEVEL2:
	    gio_datalevel = TOGGLE_BITS;
	    break;

	case ESC_SETLINESTYLE0:
	    pl_linestyle = 0;
	    break;
	case ESC_SETLINESTYLE1:
	    pl_linestyle = 1;
	    break;
	case ESC_SETLINESTYLE2:
	    pl_linestyle = 2;
	    break;
	case ESC_SETLINESTYLE3:
	    pl_linestyle = 3;
	    break;
	case ESC_SETLINESTYLE4:
	    pl_linestyle = 4;
	    break;

	case ESC_SETLINEWIDTH0:
	    pl_linewidth = 1;
	    break;
	case ESC_SETLINEWIDTH1:
	    pl_linewidth = 2;
	    break;
	case ESC_SETLINEWIDTH2:
	    pl_linewidth = 3;
	    break;
	default:
	    ;
	}

	return (COMMAND_MODE);
}


/* UPDATE_CURSOR -- Update the state of the alpha mode cursor, used to mark
 * the position of the next character on the screen when in alpha mode.
 * In any other mode this cursor is turned off.
 */
update_cursor()
{
	erase_cursor();
	if (gio_mode == ALPHA_MODE && chcur_skip++ >= 0) {
	    /* Update the position of the alpha character cursor.
	     */
	    pw_text (pw, cur_x, cur_y,
		PIX_NOT(PIX_DST), alpha_font->pixfont, " ");
	    chcur_font = alpha_font;
	    chcur_x = cur_x;
	    chcur_y = cur_y;
	    chcur_on = 1;

	    /* Turn the mouse cursor on too, if currently disabled.
	     */
	    gio_setcursor (CURSOR_ON, 0);
	}
}


/* ERASE_CURSOR -- If the character cursor is currently displayed, restore the
 * character under the cursor to its former state.
 */
erase_cursor()
{
	if (chcur_on) {
	    pw_text (pw, chcur_x, chcur_y,
		PIX_NOT(PIX_DST), chcur_font->pixfont, " ");
	    chcur_on = 0;
	}
}


/* SL_SAVE_RECT -- Make a copy of the status line pixrect in a memory
 * pixrect, so that we can later "erase" the status line by overwriting
 * it with the saved data rect.
 */
sl_save_rect()
{
	if (sl_pr) {
	    pw_read (sl_pr, 0, 0, sl_width, sl_height, PIX_SRC,
		pw, sl_xoff, sl_yoff - sl_cbase);
	    sl_rect_saved = 1;
	}
}


/* SL_RESTORE_RECT -- Restore the saved status line data pixrect.
 */
sl_restore_rect()
{
	if (sl_pr)
	    pw_write (pw, sl_xoff, sl_yoff - sl_cbase, sl_width, sl_height,
		PIX_SRC, sl_pr, 0, 0);
}


/* GIO_ROP -- Return the raster op appropriate for the datalevel control
 * option set by the user, i.e, set, clear, or toggle bits.
 */
gio_rop()
{
	register int	rop;

	switch (gio_datalevel) {
	case SET_BITS:
	    rop = PIX_SRC | PIX_DST;
	    break;
	case CLEAR_BITS:
	    rop = PIX_NOT(PIX_SRC) & PIX_DST;
	    break;
	case TOGGLE_BITS:
	    rop = PIX_SRC ^ PIX_DST;
	    break;
	default:
	    rop = PIX_SRC;
	    break;
	}

	if (!clip_graphics)
	    rop |= PIX_DONTCLIP;

	return (rop);
}


#define	NLINETYPES	5
static	short lt_dashed[]	= {  8, 3, 8, 3,  8, 3, 8, 3, 0 };
static	short lt_dotted[]	= {  2, 3, 2, 3,  2, 3, 2, 3, 0 };
static	short lt_dashdot[]	= { 14, 3, 1, 3, 14, 3, 1, 3, 0 };
static	short lt_dash3dot[]	= { 20, 3, 1, 3,  1, 3, 1, 3, 0 };

static	short *lt_pattern[] = {
	NULL,
	lt_dashed,
	lt_dotted,
	lt_dashdot,
	lt_dash3dot
};


/* PL_TEXTURE -- Return a pointer to a texture descriptor (Breshingham
 * dashed line drawing algorithm) to be used to draw a dashed polyline.
 * The case linetype==0 is special, signifying a solid line.  The descriptor
 * must be initialized to a known state, i.e., zeroed, on each call or
 * the pixrect polyline code will produce garbage.
 */
static Pr_texture *
pl_texture (linetype)
int	linetype;
{
	register char	*p;
	register int	n;
	static	Pr_texture tex;
	short	*pattern;

	if (linetype == 0)
	    return (NULL);		/* solid line */
	else
	    pattern = lt_pattern[linetype % NLINETYPES];

	for (p=(char *)(&tex), n=sizeof(tex);  --n >= 0;  )
	    *p++ = NULL;

	tex.pattern = pattern;
	tex.options.givenpattern = 1;

	return (&tex);
}
