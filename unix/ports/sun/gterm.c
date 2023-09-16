/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <suntool/tty.h>
#include <suntool/walkmenu.h>
#include <suntool/tool_struct.h>
#include <sunwindow/win_cursor.h>
#include <sgtty.h>
#include <stdio.h>
#include <ctype.h>
#include "gterm.h"

/*
 * GTERM -- Graphics terminal emulator.  This code implements a virtual
 * graphics terminal consisting of independent text and graphics frames.
 * The text (tty) frame is a standard Sun TTY frame.  The graphics (gio)
 * frame is a tektronix 4012 with extensions, some of which are patterned
 * after the Retrographics vt100, Pericom, etc., and others of which are or
 * will be added to provide GKS like capabilities via a conventional data
 * driven graphics terminal interface.  The tty and gio frames are independent
 * frames under Sunview, hence they may be resized, moved, opened or closed,
 * hidden or exposed, etc., independently of each other.  Both frames are
 * active at all times, but since a single terminal i/o stream is used to
 * drive both frames an applications program can communicate with only one
 * frame at a time, just like a conventional graphics terminal. 
 *
 * D.Tody, January 1987 (NOAO/IRAF project)
 */

#define	MAX_ARGS	50		/* max tty or gio cmdline args	*/
#define	DEF_TEKXRES	1024		/* default logical X resolution	*/
#define	DEF_TEKYRES	780		/* default logical Y resolution	*/

#define	NO_ACTION	0		/* no action on open/close ws	*/
#define	SHOW_FRAME	1		/* show or unshow gio frame	*/
#define	UNSHOW_FRAME	1		/* show or unshow gio frame	*/
#define	EXPOSE_FRAME	2		/* expose or hide gio frame	*/
#define	HIDE_FRAME	2		/* expose or hide gio frame	*/
#define	CROSSHAIR_OFF	1		/* crosshair cursor off		*/
#define	CROSSHAIR_ON	2		/* turn crosshair cursor on	*/
#define	SZ_LOGBUF	1024		/* size of logfile buffer	*/
#define	SZ_GINMODETERM	10		/* ginmode string length	*/
#define	SZ_FNAME	128

#define	R_TYPE		0		/* 0=postscript, 1=rasterfile */
#define	R_DISPOSE	"lpr -s %s"	/* dispose command */
#define	R_FILENAME	""		/* output filename */

/* External variables. */
extern	int gio_graphicsenabled;	/* set when in graphics mdoe	*/
int	cursor_show = -1;		/* cursor state: on or off	*/
Window	gt_baseframe, gt_ttysw, gio_frame, gio_canvas;
FILE	*gt_logfp = NULL;		/* logfile file pointer		*/

/* Screendump stuff. */
int	r_type = R_TYPE;
char	r_dispose[SZ_FNAME+1] = R_DISPOSE;
char	r_filename[SZ_FNAME+1] = R_FILENAME;

/* Both external and a user option. */
int	clip_graphics = 1;		/* disable rasterop clipping	*/

/* User options. */
static	int graphics_enable;		/* enable graphics plane	*/
static	int openws_action;		/* default action on openws	*/
static	int closews_action;		/* default action on closews	*/
static	int closews_pause;		/* pause/confirm closews?	*/
static	int canvas_retained;		/* retain the canvas pixwin?	*/
static	int reverse_video;		/* normal or reverse video	*/
static	int color;			/* color or monochrome graphics	*/
static	int win_xsize;			/* standard size graphics frame	*/
static	int win_ysize;
static	int win_xoff;			/* standard offset from tty fr.	*/
static	int win_yoff;
static	int ignore;			/* ignore suspend/stop codes?	*/
static	char gin_modeterm[SZ_GINMODETERM+1];
static	char logfile[SZ_FNAME+1] = "gterm.out";

/* The following are used to save and later restore the option parameters. */
static	int s_clip_graphics = 1;
static	int s_graphics_enable = GRAPHICS_ON;
static	int s_openws_action = SHOW_FRAME;
static	int s_closews_action= HIDE_FRAME;
static	int s_closews_pause = 0;
static	int s_canvas_retained = 1;
static	int s_reverse_video = 0;
static	int s_color = 1;
static	int s_win_xsize = 800;
static	int s_win_ysize = 630;
static	int s_win_xoff  = 100;
static	int s_win_yoff  =  19;
static	int s_ignore = 0;
static	char s_gin_modeterm[SZ_GINMODETERM] = "015";

/* Internal state variables. */
static	int cursor_type = -1;		/* crosshair or small cursor	*/
static	int cursor_read_pending=0;	/* waiting for cursor event	*/
static	int key_left = 0;		/* key aliased to left msbutton	*/
static	int key_middle = 0;		/* key aliased to mid msbutton	*/
static	int key_right = 0;		/* key aliased to mid msbutton	*/
static	int last_key = 0;		/* last cursor read key		*/
static	int last_x= -1,last_y= -1;	/* last cursor read position	*/ 
static	int pause_mode = 0;		/* waiting for closews event	*/
static	int cursor_used = 0;		/* set if cursor is used	*/
static	int gio_frame_has_moved=0;	/* set when user moves giowin	*/
static	int setup_xoff = 140;		/* offset to setup panel	*/
static	int setup_yoff = 60;
static	int tty_lastx, tty_lasty;	/* cursor position in gt_ttysw	*/
static	int height_is_set = 0;		/* tty w. height set on cmdline	*/
static	int tty_nlines = 0;		/* number of lines in tty w.	*/
static	int shell = 0;			/* we are running a shell	*/
static	char t_suspc, t_dsuspc, t_set=0;

/* The following is a kludge and should be determined at runtime */
static	int pty_fd = 3;			/* fd of tty pseudoterminal	*/

/* Graphics fonts. */
static	char	fb_courier[] = "/usr/lib/fonts/fixedwidthfonts/cour.b.%d";
static	char	fr_courier[] = "/usr/lib/fonts/fixedwidthfonts/cour.r.%d";
static	char	fr_screen[]  = "/usr/lib/fonts/fixedwidthfonts/screen.r.%d";
static	char	fb_screen[]  = "/usr/lib/fonts/fixedwidthfonts/screen.b.%d";

#define FULLSCREEN	5
struct fonttab alpha_fonts[] = {
	10,  7,	12,  560, 420, NULL, fb_courier, "10:[560x420]",
	12,  8,	14,  640, 490, NULL, fb_courier, "12:[640x490]",
	14,  9,	16,  720, 560, NULL, fb_courier, "14:[720x560]",
	16, 10,	18,  800, 630, NULL, fb_courier, "16:[800x630]",
	18, 11,	19,  880, 665, NULL, fb_courier, "18:[880x665]",
	24, 14,	25, 1142, 890, NULL, fb_courier, "24:fullscreen",
	/* Note nentries must match panel choice list */
	0, 0, 0, 0, 0, NULL, NULL, NULL
};

struct fonttab text_fonts[] = {
	11,  7,	11,  560,   0, NULL, fr_screen,  "screen.r.11",
	12,  8,	14,  640,   0, NULL, fb_screen,  "screen.b.12",
	14,  9,	16,  720,   0, NULL, fb_screen,  "screen.b.14",
	16, 10,	18,  800,   0, NULL, fb_courier, "courier.b.16",
	18, 11,	19,  880,   0, NULL, fb_courier, "courier.b.18",
	24, 14,	25, 1142,   0, NULL, fb_courier, "courier.b.24",
	/* Note nentries must match panel choice list */
	0, 0, 0, 0, 0, NULL, NULL, NULL
};

#define	DEF_TEXTFONT	4
static	int	alpha_font_index;
static	int	text_font_index = DEF_TEXTFONT;
static	struct	fonttab *alpha_font = NULL;
static	struct	fonttab *text_font = NULL;

#define	SWAP(a,b) {int temp; temp=a;a=b;b=temp;}
#define HEIGHTADJUST \
	(tool_headerheight((int)window_get(gt_baseframe, FRAME_SHOW_LABEL)) + \
	TOOL_BORDERWIDTH)

static	short iconimage[] = {
#include "gterm.icon"
};
DEFINE_ICON_FROM_IMAGE (icon, iconimage);

static	int main_argc, tty_argc, gio_argc;
static	char **main_argv, *tty_argv[MAX_ARGS], *gio_argv[MAX_ARGS];
static	unsigned char red[2]   = {   0, 255 };
static	unsigned char green[2] = {   0, 255 };
static	unsigned char blue[2]  = { 128,   0 };

static	Menu_item logitem;
static	struct rect screen;
static	struct	pixwin *pw;
static	Panel	setup_panel, pause_panel;
static	Window	setup_frame, pause_frame;
static	Notify_value ev_gt_ttysw();
static	Notify_value ev_ttyframe();
static	Notify_value ev_gioframe();
static	Notify_value ev_gioinput();
static	Notify_func sig_tstp();
static	tty_adjustheight();
extern	char *getenv();


/* GTERM_MAIN -- Create the graphics terminal window tree, i.e., the panel
 * subwindow and tty subwindow, the gio subframe canvas in which graphics
 * will be drawn, and the setup popup which is used to set the terminal
 * options.  Only the panel and tty (text) window is shown initially; this
 * is functionally equivalent to a shelltool window with all the same command
 * line arguments.  Additional command line arguments are recognized for
 * initialization of the graphics frame.
 */
#ifdef STANDALONE
main (argc, argv)
#else
gterm_main (argc, argv)
#endif
int	argc;
char	**argv;
{
	char	*s;

	/* Set user settable options to their initial compiled in values. */
	restore_params();

	main_argc = argc;
	main_argv = argv;
	parse_args (argc, argv, &tty_argc, tty_argv, &gio_argc, gio_argv);

	/* Screendump stuff. */
	if (s = getenv ("R_DISPOSE"))
	    strcpy (r_dispose, s);
	if (s = getenv ("R_FILENAME"))
	    strcpy (r_filename, s);
	if (s = getenv ("R_RASTERFILE")) {
	    strcpy (r_filename, s);
	    r_type = 1;
	}

	/* Create the base frame for all the GTERM windows. */
	gt_baseframe = window_create (NULL, FRAME,
		FRAME_ICON,		&icon,
		FRAME_LABEL, "gterm - NOAO/IRAF Sunview Graphics Terminal V1.2",
		FRAME_ARGC_PTR_ARGV,	&tty_argc, tty_argv,
		FRAME_NO_CONFIRM,	FALSE,
		0);
	if (gt_baseframe == NULL)
	    _exit (1);
	screen = *(struct rect *) window_get (gt_baseframe, WIN_SCREEN_RECT);

	create_tty_subwindow (tty_argc, tty_argv);
	create_gio_popup (gio_argc, gio_argv);
	create_frame_menu (gt_baseframe);

	/* Save initial options settings for later reset (in setup panel). */
	save_params();

	notify_interpose_event_func (gt_baseframe, ev_ttyframe, NOTIFY_SAFE);
	notify_interpose_event_func (gt_ttysw,     ev_gt_ttysw, NOTIFY_SAFE);
	notify_interpose_event_func (gio_frame,    ev_gioframe, NOTIFY_SAFE);
	notify_interpose_event_func (gio_canvas,   ev_gioinput, NOTIFY_SAFE);

	notify_set_signal_func (gt_baseframe, sig_tstp, SIGTSTP, NOTIFY_SYNC);

#ifdef TTY_TTY_FD
	/* SunOS 3.4 provides just what we needed to fix the pty-fd kludge! */
	pty_fd = (int) window_get (ttysw, TTY_TTY_FD);
#endif
	gio_setup (pty_fd, pw);
	gio_hardreset (DEF_TEKXRES, DEF_TEKYRES, alpha_font, text_font);

	window_main_loop (gt_baseframe);
	exit (0);
}


/* PARSE_ARGS -- Parse the argument list into the arguments for the tty
 * frame and the arguments for the gio frame.  This is very easy; the gio
 * args, if any, are whatever follows the arg "-G" in the argument list.
 * All args preceding the -G are considered to be tty args.
 */
static
parse_args (argc, argv, tty_argc, tty_argv, gio_argc, gio_argv)
int	argc;
char	*argv[];
int	*tty_argc, *gio_argc;
char	*tty_argv[], *gio_argv[];
{
	register char	*argp, *last;
	register int	arg = 1;

	/* Copy the tty arguments. */
	tty_argv[0] = argv[0];
	for (*tty_argc=1;  arg <= argc && (argp = argv[arg]) != NULL;  arg++)
	    if (strcmp (argv[arg], "-G") != 0) {
		tty_argv[(*tty_argc)++] = argp;
		if ((strcmp(argp, "-Ws") == 0) || (strcmp(argp, "-size") == 0))
		    height_is_set = 1;
		else if ((strcmp(argp, "-Wh") == 0) ||
		         (strcmp(argp, "-height") == 0)) {
		    height_is_set = 0;
		    tty_nlines = atoi (argv[arg+1]);
		}
	    } else {
		arg++;
		break;
	    }

	gio_argv[0] = argv[0];
	last = "";

	/* Copy the gio arguments. */
	for (*gio_argc=1;  arg <= argc && (argp = argv[arg]) != NULL;  arg++) {

	    /* If an argument string is encountered which is not an argument
	     * to a GIO window switch, assume that it is the first word of the
	     * command to be executed in the text window, and move the
	     * remaining arguments to the tty_argv list.
	     */
	    if (strncmp(last,"-Gopen",3) && strncmp(last,"-Gclose",3) &&
		isalpha(argp[0])) {

		for (;  arg <= argc && (argp = argv[arg]) != NULL;  arg++)
		    tty_argv[(*tty_argc)++] = argp;
		break;
	    }

	    gio_argv[(*gio_argc)++] = argp;
	    last = argp;
	}

	tty_argv[(*tty_argc)] = NULL;
	gio_argv[(*gio_argc)] = NULL;
}


/* CREATE_TTY_SUBWINDOW -- Create a standard TTY subwindow of the base frame.
 * This code emulates the shelltool arguments.
 */
static
create_tty_subwindow (argc, argv)
int	argc;
char	*argv[];
{
	static	 char	*sh_argv[2];
	register char	*argp;
	register int	arg;
	char	**tty_argv = argv;
	char	*tool_name = argv[0];
	char	*init_cmd = NULL;
	char	*bold_name = NULL;
	int	become_console = 0;

	/* Get gt_ttysw related args.  The standard frame arguments will
	 * already have been removed from the list when the base frame was
	 * created, leaving only the tty specific arguments and possibly the
	 * command to be run.
	 */
	sh_argv[0] = NULL;
	sh_argv[1] = NULL;

	for (arg=1;  arg < argc && (argp = argv[arg]);  arg++) {
	    if (*argp != '-')
		break;
	    else if (!strncmp (argp, "-ignore", 3)) {
		ignore++;
		continue;
	    }
	    
	    switch (*(argp+1)) {
	    case 'C':
		become_console = 1;
		break;
	    case '?':
		tool_usage (tool_name);
		print_usage (tool_name);
		window_destroy (gt_baseframe);
		exit (1);
	    case 'B':
		if (argc > 1)
		    bold_name = argv[++arg];
		break;
	    case 'I':
		if (argc > 1)
		    init_cmd = argv[++arg];
		break;
	    case 'T':
		gio_enable (0);
		break;
	    }
	}

	/* Get path to command interpreter to be run. */
	if (argp == NULL || arg >= argc) {
	    shell++;
	    tty_argv = sh_argv;
	    if ((tty_argv[0] = getenv("SHELL")) == NULL) {
		tty_argv[0] = "/bin/sh";
		if (ignore)
		    shell = 0;
	    }
	} else {
	    tty_argv = &argv[arg];
	    if (!strcmp(tty_argv,"csh") || !strcmp(tty_argv,"/bin/csh"))
		shell++;
	    else if (!ignore)
		shell++;
	}

	gt_ttysw = window_create (gt_baseframe, TTY,
	    TTY_ARGV,			tty_argv,
	    TTY_QUIT_ON_CHILD_DEATH,	TRUE,
	    TTY_CONSOLE,		become_console,
	    0);

	window_set (gt_ttysw,
	    WIN_CONSUME_KBD_EVENTS, KEY_TOP(7), KEY_TOP(8), KEY_TOP(9), 0,
	    0);

	if (bold_name)
	    window_set (gt_ttysw, TTY_BOLDSTYLE_NAME, bold_name, 0);

	/* Pass user supplied command, if given, to the shell. */
	if (init_cmd) {
	    int	len = strlen (init_cmd);

	    if (init_cmd[len-1] != '\n') {
		init_cmd[len] = '\n';
		len++;
	    }
	    ttysw_input (gt_ttysw, init_cmd, len);
	}

	window_fit_height (gt_ttysw);
	window_fit_height (gt_baseframe);

	/* Only correct the height if the user did not give a pixel value
 	 * for the height.
	 */
	if (!height_is_set)
	    tty_adjustheight (tty_nlines);
}


/* CREATE_GIO_POPUP -- Create a canvas popup to be used for graphics i/o.  
 * We use a separate subframe for this so that it may be moved and sized
 * independently of the tty window; tty windows like to high and narrow
 * (lots of lines) whereas graphs tend to be short and wide (good resolution
 * in X).  We also like a large graph window to get good resolution, but
 * a separate window is desired so that the screen space can be freed up
 * when we are not actually doing graphics, and so that the last graph can
 * easily be recalled at any time.  The standard graphics window should have
 * a landscape aspect ratio (y/x=.77), like most terminals and laser printers.
 */
static
create_gio_popup (argc, argv)
int	argc;
char	**argv;
{
	static unsigned char mono[2];
	register char	*argp;
	register int	arg;
	char	*frame_argv[64];
	int	frame_argc;
	char	pathname[256];
	char	mapname[64];
	int	name;

	frame_argv[0] = argv[0];
	frame_argc = 1;

	/* Override the builtin defaults with the values given by the user
	 * on the command line, if any.
	 */
	for (arg=1;  arg <= argc && (argp = argv[arg]) != NULL;  arg++) {
	    /* Standard Sunview frame args. */
	    if (!strcmp (argp, "-Wb") || !strncmp (argp, "-back", 5)) {
		red[0]    = atoi (argv[++arg]);
		green[0]  = atoi (argv[++arg]);
		blue[0]   = atoi (argv[++arg]);
	    } else if (!strcmp (argp, "-Wf") || !strncmp (argp, "-fore", 5)) {
		red[1]    = atoi (argv[++arg]);
		green[1]  = atoi (argv[++arg]);
		blue[1]   = atoi (argv[++arg]);
	    } else if (!strcmp (argp, "-Wp") || !strncmp (argp, "-pos", 4)) {
		win_xoff  = atoi (argv[++arg]) -
		    (int) window_get (gt_baseframe, WIN_X);
		win_yoff  = atoi (argv[++arg]) -
		    (int) window_get (gt_baseframe, WIN_Y);
	    } else if (!strcmp (argp, "-Ws") || !strncmp (argp, "-size", 5)) {
		win_xsize = atoi (argv[++arg]) - (TOOL_BORDERWIDTH * 2);
		win_ysize = atoi (argv[++arg]) - (TOOL_BORDERWIDTH * 2);

	    /* Graphics options. */
	    } else if (!strncmp (argp, "-Gopen", 3)) {
		argp = argv[++arg];
		if (!strncmp (argp, "noaction", 1))
		    openws_action = NO_ACTION;
		else if (!strncmp (argp, "show", 1))
		    openws_action = SHOW_FRAME;
		else if (!strncmp (argp, "expose", 1))
		    openws_action = EXPOSE_FRAME;
		else {
		    fprintf (stderr,
			"Warning: unknown argument `%s' to -Gopen\n", argp);
		}
	    } else if (!strncmp (argp, "-Gclose", 3)) {
		argp = argv[++arg];
		if (!strncmp (argp, "noaction", 1))
		    closews_action = NO_ACTION;
		else if (!strncmp (argp, "blank", 1))
		    closews_action = UNSHOW_FRAME;
		else if (!strncmp (argp, "hide", 1))
		    closews_action = HIDE_FRAME;
		else {
		    fprintf (stderr,
			"Warning: unknown argument `%s' to -Gclose\n", argp);
		}
	    } else if (!strncmp (argp, "-pause", 2)) {
		closews_pause = 1;
	    } else if (!strncmp (argp, "-nopause", 4)) {
		closews_pause = 0;
	    } else if (!strncmp (argp, "-retain", 4)) {
		canvas_retained = 1;
	    } else if (!strncmp (argp, "-noretain", 6)) {
		canvas_retained = 0;
	    } else if (!strncmp (argp, "-clip", 3)) {
		clip_graphics = 1;
	    } else if (!strncmp (argp, "-noclip", 4)) {
		clip_graphics = 0;
	    } else if (!strncmp (argp, "-color", 3)) {
		color = 1;
	    } else if (!strncmp (argp, "-mono", 2)) {
		color = 0;
	    } else if (!strncmp (argp, "-reverse", 4)) {
		SWAP (red[0],   red[1]);
		SWAP (green[0], green[1]);
		SWAP (blue[0],  blue[1]);
		reverse_video = 1;
	    } else if (!strncmp (argp, "-noreverse", 6)) {
		reverse_video = 0;
	    } else if (!strncmp (argp, "-ginterm", 2)) {
		if (argv[arg+1] && isdigit (argv[arg+1][0]))
		    strcpy (gin_modeterm, argv[++arg]);
		if (argv[arg+1] && isdigit (argv[arg+1][0])) {
		    strcat (gin_modeterm, " ");
		    strcat (gin_modeterm, argv[++arg]);
		}
		gio_setginmodeterm (gin_modeterm);
	    } else if (!strncmp (argp, "-logfile", 2)) {
		strcpy (logfile, argv[++arg]);
	    } else
		frame_argv[frame_argc++] = argp;
	}

	/* Open the tek-alpha and text mode graphics fonts. */
	if (gio_getbestfont (win_xsize, win_ysize) == -1)
	    exit (1);

	/* Open graphics window and canvas.  Generate a default size
	 * landscape mode window and position it relative to the base frame.
	 */
	gio_frame = window_create (gt_baseframe, FRAME,
		FRAME_ARGS,	frame_argc, frame_argv,
		FRAME_NO_CONFIRM,	TRUE,
		WIN_HEIGHT,	win_ysize + (TOOL_BORDERWIDTH * 2),
		WIN_WIDTH,	win_xsize + (TOOL_BORDERWIDTH * 2),
		WIN_X,		win_xoff,
		WIN_Y,		win_yoff,
		0);
	gio_canvas = window_create (gio_frame, CANVAS,
		CANVAS_RETAINED, canvas_retained ? TRUE : FALSE,
		0);

	/* Set the initial "plus" type graphics cursor.  This changes to
	 * a full crosshair cursor only when the cursor position is being
	 * read by the applications program.
	 */
	gio_setcursor (CURSOR_ON, CROSSHAIR_OFF);

	/* Set input event flags. */
	window_set (gio_canvas,
		WIN_CONSUME_PICK_EVENTS, WIN_NO_EVENTS,
		    WIN_MOUSE_BUTTONS, LOC_DRAG, WIN_UP_EVENTS, 0,
		WIN_CONSUME_KBD_EVENTS, WIN_NO_EVENTS, WIN_ASCII_EVENTS,
		    WIN_LEFT_KEYS, WIN_TOP_KEYS, WIN_RIGHT_KEYS, 0,
		0);

	/* Get canvas pixwin */
	pw = canvas_pixwin (gio_canvas);
	if (pw->pw_pixrect->pr_depth == 1)
	    color = 0;

	/* Set up the default color map.  */
	sprintf (mapname, "MONO%04d", getpid() % 10000);
	pw_setcmsname (pw, mapname);
	pw_putcolormap (pw, 0, 2, red, green, blue);

	if (!color) {
	    /* Monochrome graphics. */
	    /* window_set (gio_canvas, CANVAS_FAST_MONO, TRUE, 0); */
	    if (reverse_video)
		pw_whiteonblack (pw, 0, 1);
	    else
		pw_blackonwhite (pw, 0, 1);
	}
}


static	Panel_item pan_graphics_enable, pan_openws_action, pan_closews_action;
static	Panel_item pan_closews_pause, pan_retain_graphics, pan_clip_graphics;
static	Panel_item pan_graphics_screen, pan_graphics_video, pan_alpha_font;

/* CREATE_SETUP_POPUP -- Create the popup menu used to set the terminal
 * setup options.
 */
static
create_setup_popup()
{
	extern	reset_proc(), clear_proc(), gclear_proc();
	extern	setup_proc(), toggle_graphics();
	static	panel_set_item(), set_ginmodeterm(), set_logfile();

	setup_frame = window_create (gt_baseframe, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		WIN_X,			setup_xoff,
		WIN_Y,			setup_yoff,
		0);
	setup_panel = window_create (setup_frame, PANEL, 0);

	panel_create_item (setup_panel, PANEL_MESSAGE,
		PANEL_ITEM_X,		ATTR_COL(11),
		PANEL_ITEM_Y,		ATTR_ROW(0),
		PANEL_LABEL_STRING,	"Graphics Terminal Setup Options",
		0);

	pan_graphics_enable = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(1),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Graphics plane:                ",
		PANEL_CHOICE_STRINGS,	"Disable", "Enable",
					"Discard Graphics Data", 0,
		PANEL_VALUE,		graphics_enable,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_openws_action = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(2),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Open workstation action:       ",
		PANEL_CHOICE_STRINGS,	"No action", "Show graphics",
					"Expose graphics", 0,
		PANEL_VALUE,		openws_action,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_closews_action = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(3),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Close workstation action:      ",
		PANEL_CHOICE_STRINGS,	"No action", "Blank graphics",
					"Hide graphics", 0,
		PANEL_VALUE,		closews_action,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_closews_pause = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(4),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Pause on close workstation:    ",
		PANEL_CHOICE_STRINGS,	"No", "Yes", 0,
		PANEL_VALUE,		closews_pause,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_retain_graphics = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(5),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Retain graphics frame:         ",
		PANEL_CHOICE_STRINGS,	"No", "Yes", 0,
		PANEL_VALUE,		canvas_retained,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_clip_graphics = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(6),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Clip graphics:                 ",
		PANEL_CHOICE_STRINGS,	"No", "Yes", 0,
		PANEL_VALUE,		clip_graphics,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	if (pw->pw_pixrect->pr_depth > 1) {
	    pan_graphics_screen = panel_create_item (setup_panel, PANEL_CYCLE,
		    PANEL_ITEM_X,		ATTR_COL(0),
		    PANEL_ITEM_Y,		ATTR_ROW(7),
		    PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		    PANEL_LABEL_STRING,	"Graphics screen type:          ",
		    PANEL_CHOICE_STRINGS,	"Mono", "Color", 0,
		    PANEL_VALUE,		color,
		    PANEL_NOTIFY_PROC,	panel_set_item,
		    0);
	} else {
	    pan_graphics_screen = panel_create_item (setup_panel, PANEL_CYCLE,
		    PANEL_ITEM_X,		ATTR_COL(0),
		    PANEL_ITEM_Y,		ATTR_ROW(7),
		    PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		    PANEL_LABEL_STRING,	"Graphics screen type:          ",
		    PANEL_CHOICE_STRINGS,	"Mono only", 0,
		    PANEL_VALUE,		color,
		    PANEL_NOTIFY_PROC,	panel_set_item,
		    0);
	}

	pan_graphics_video = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(8),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Graphics video:                ",
		PANEL_CHOICE_STRINGS,	"Normal", "Reverse", 0,
		PANEL_VALUE,		reverse_video,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_alpha_font = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(9),
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Graphics font and screen sizes:",
		PANEL_CHOICE_STRINGS,	alpha_fonts[0].label,
					alpha_fonts[1].label,
					alpha_fonts[2].label,
					alpha_fonts[3].label,
					alpha_fonts[4].label,
					alpha_fonts[5].label,
					0,
		PANEL_VALUE,		alpha_font_index,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(10) + 3,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Logfile name:                  ",
		PANEL_VALUE,		logfile,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, 20,
		PANEL_NOTIFY_PROC,	set_logfile,
		0);

	panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(11) + 3,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"GIN mode terminators (octal):  ",
		PANEL_VALUE,		gin_modeterm,
		PANEL_VALUE_STORED_LENGTH, SZ_GINMODETERM,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_GINMODETERM,
		PANEL_NOTIFY_PROC,	set_ginmodeterm,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(12) + 3,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Reset", 0,0),
		PANEL_NOTIFY_PROC,	reset_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Clear", 0,0),
		PANEL_NOTIFY_PROC,	clear_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Gclear", 0,0),
		PANEL_NOTIFY_PROC,	gclear_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Show graphics", 0,0),
		PANEL_NOTIFY_PROC,	toggle_graphics,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Quit", 0,0),
		PANEL_NOTIFY_PROC,	setup_proc,
		0);

	window_fit (setup_panel);
	window_fit (setup_frame);
}


/* PANEL_SET_ITEM -- Called when an item is seleted in the setup panel to
 * set the associated global variable and possibly take some action.
 */
static
panel_set_item (item, value)
Panel_item	item;
int		value;
{
	if (item == pan_graphics_enable) {
	    gio_enable (graphics_enable = value);
	} else if (item == pan_openws_action) {
	    openws_action = value;
	} else if (item == pan_closews_action) {
	    closews_action = value;
	} else if (item == pan_closews_pause) {
	    if (closews_pause != value) {
		closews_pause = value;
		show_pausepanel (0);
	    }
	} else if (item == pan_clip_graphics) {
	    clip_graphics = value;
	} else if (item == pan_graphics_screen) {
	    if (color != value) {
		if (value)
		    pw_putcolormap (pw, 0, 2, red, green, blue);
		else if (reverse_video)
		    pw_whiteonblack (pw, 0, 1);
		else
		    pw_blackonwhite (pw, 0, 1);
		color = value;
	    }
	} else if (item == pan_graphics_video) {
	    if (reverse_video != value) {
		if (pw->pw_pixrect->pr_depth > 1) {
		    unsigned char r[2], g[2], b[2];

		    pw_getcolormap (pw, 0, 2, r, g, b);
		    SWAP (r[0], r[1]);
		    SWAP (g[0], g[1]);
		    SWAP (b[0], b[1]);
		    pw_putcolormap (pw, 0, 2, r, g, b);
		    if (color)
			pw_getcolormap (pw, 0, 2, red, green, blue);
		} else if (value) {
		    pw_whiteonblack (pw, 0, 1);
		} else
		    pw_blackonwhite (pw, 0, 1);

		reverse_video = value;
	    }
	} else if (item == pan_retain_graphics) {
	    if (canvas_retained != value) {
		canvas_retained = value;
		notify_remove_event_func (gio_canvas,
		    ev_gioinput, NOTIFY_SAFE);
		window_destroy (gio_frame);
		parse_args (main_argc, main_argv,
		    &tty_argc, tty_argv, &gio_argc, gio_argv);
		create_gio_popup (gio_argc, gio_argv);
		notify_interpose_event_func (gio_canvas,
		    ev_gioinput, NOTIFY_SAFE);
		gio_setup (pty_fd, pw);
		gio_reset (DEF_TEKXRES, DEF_TEKYRES, alpha_font, text_font);
	    }
	} else if (item == pan_alpha_font) {
	    if (alpha_font_index != value) {
		struct  fonttab *ft = &alpha_fonts[value];
		int	xorigin, yorigin, xoffset, yoffset;
		int	g_xsize, g_ysize, g_xorig, g_yorig;

		/* Determine whether the offset of the subframe from the base
		 * frame needs to be modified to keep the frame on the screen.
		 */
		xorigin = (int) window_get (gt_baseframe, WIN_X);
		yorigin = (int) window_get (gt_baseframe, WIN_Y);
		xoffset = win_xoff;
		yoffset = win_yoff;

		g_xsize = ft->win_xsize + (TOOL_BORDERWIDTH * 2);
		g_ysize = ft->win_ysize + (TOOL_BORDERWIDTH * 2);
		g_xorig = xorigin + xoffset;	/* screen relative */
		g_yorig = yorigin + yoffset;	/* screen relative */

		if (g_xorig + g_xsize >= screen.r_width)
		    xoffset -= (g_xorig + g_xsize - screen.r_width);
		if (g_yorig + g_ysize >= screen.r_height)
		    yoffset -= (g_yorig + g_ysize - screen.r_height);

		/* Resize and/or move the gio frame.  The gio_frame event
		 * handler will detect the resize and select a new font.
		 */
		window_set (gio_frame, WIN_SHOW, FALSE,
		    WIN_WIDTH, g_xsize, WIN_HEIGHT, g_ysize, 0);
		
		/* Cannot set a negative subframe offset, so move base frame
		 * to screen origin temporarily if the gio frame won't fit at
		 * the lower right.  Note that win_[xy]off are not permanently
		 * changed, so restoring the original size should return the
		 * window to its original position.
		 */
		if (gio_frame_has_moved = (xoffset < 0 || yoffset < 0)) {
		    window_set (gt_baseframe, WIN_X, 0, WIN_Y, 0, 0);
		    window_set (gio_frame,
			WIN_X, xorigin + xoffset, WIN_Y, yorigin + yoffset, 0);
		    window_set (gt_baseframe, WIN_X,xorigin, WIN_Y,yorigin, 0);
		} else
		    window_set (gio_frame, WIN_X, xoffset, WIN_Y, yoffset, 0);

		window_set (gio_frame, WIN_SHOW, TRUE, 0);

		/* If the setup panel is on, move it to the top. */
		if ((int) window_get (setup_frame, WIN_SHOW) == TRUE)
		    window_set (setup_frame, WIN_SHOW, TRUE, 0);
	    }
	}
}


/* TOGGLE_FULLSCREEN -- Toggle between a full screen graph and a regular size
 * graph.
 */
static
toggle_fullscreen()
{
	static	save_index = -1;

	if (alpha_font_index == FULLSCREEN && save_index >= 0)
	    panel_set_item (pan_alpha_font, save_index);
	else {
	    save_index = alpha_font_index;
	    panel_set_item (pan_alpha_font, FULLSCREEN);
	}
}


/* SET_GINMODETERM -- Set the GIN mode terminators.
 */
static Panel_setting
set_ginmodeterm (item, event)
Panel_item	item;
Event		*event;
{
	strcpy (gin_modeterm,
	    (char *)panel_get_value (item));
	gio_setginmodeterm (gin_modeterm);

	return (panel_text_notify (item,event));
}


/* SET_LOGFILE -- Set the filename of the logfile.
 */
static Panel_setting
set_logfile (item, event)
Panel_item	item;
Event		*event;
{
	if (gt_logfp) {
	    fclose (gt_logfp);
	    gt_logfp = NULL;
	    menu_set (logitem, MENU_STRING, "Logging on", 0);
	}

	strcpy (logfile,
	    (char *)panel_get_value (item));
	return (panel_text_notify (item,event));
}


/* SETUP_PROC -- Toggle whether or not the setup panel is shown.
 */
static
setup_proc()
{
	if (setup_frame == NULL) {
	    create_setup_popup();
	    create_setup_popup (0, NULL);
	    panel_set_item();
	    window_set (setup_frame, WIN_SHOW, TRUE, 0);
	} else {
	    window_destroy (setup_frame);
	    setup_frame = NULL;
	}
}


/* TEXTCOPY_PROC -- Make a hardcopy of the tty window on the laserwriter.
 */
static
textcopy_proc()
{
	int	depth = 1;

	window_set (gt_baseframe, WIN_SHOW, TRUE, 0);
	notify_dispatch();

	screendump (
	    (int)window_get(gt_ttysw,WIN_FD), win_get_pixwin(gt_ttysw),
	    (int) window_get (gt_baseframe, WIN_WIDTH) - TOOL_BORDERWIDTH * 2,
	    (int) window_get (gt_baseframe, WIN_HEIGHT) - HEIGHTADJUST,
	    (int) window_get (gt_baseframe, WIN_X) + TOOL_BORDERWIDTH,
	    (int) window_get (gt_baseframe, WIN_Y) + HEIGHTADJUST -
		TOOL_BORDERWIDTH,
	    depth);
}


/* GRAPHCOPY_PROC -- Make a hardcopy of the gio window on the laserwriter.
 */
static
graphcopy_proc()
{
	int	depth = 1;

	window_set (gio_frame, WIN_SHOW, TRUE, 0);
	notify_dispatch();

	screendump (
	    (int)window_get(gio_canvas,WIN_FD), pw,
	    (int) window_get (gio_frame, WIN_WIDTH) - TOOL_BORDERWIDTH * 2,
	    (int) window_get (gio_frame, WIN_HEIGHT) - TOOL_BORDERWIDTH * 2,
	    (int) window_get (gt_baseframe, WIN_X) +
		(int) window_get (gio_frame, WIN_X) + TOOL_BORDERWIDTH,
	    (int) window_get (gt_baseframe, WIN_Y) +
		(int) window_get (gio_frame, WIN_Y) + TOOL_BORDERWIDTH,
	    depth);
}


/* SCREENCOPY_PROC -- Make a bitmap hardcopy of the screen on the laserwriter.
 */
static
screencopy_proc()
{
	int	depth = 1;

	screendump (
	    (int)window_get(gt_ttysw,WIN_FD), win_get_pixwin(gt_ttysw),
	    screen.r_width, screen.r_height, screen.r_left, screen.r_top,
	    depth);
}


/* SCREENCOPY8_PROC -- Make a greyscale hardcopy of the screen on the
 * laserwriter.
 */
static
screencopy8_proc()
{
	int	depth = 8;

	screendump (
	    (int)window_get(gt_ttysw,WIN_FD), win_get_pixwin(gt_ttysw),
	    screen.r_width, screen.r_height, screen.r_left, screen.r_top,
	    depth);
}


/* CREATE_PAUSE_POPUP -- Create the pause popup menu used to wait for
 * a user response.
 */
static
create_pause_popup()
{
	pause_frame = window_create (gt_baseframe, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		WIN_X,			win_xoff + 5,
		WIN_Y,			win_yoff + 5,
		0);
	pause_panel = window_create (pause_frame, PANEL, 0);

	panel_create_item (pause_panel, PANEL_MESSAGE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(0),
		PANEL_LABEL_STRING,	"Hit any key to continue",
		0);
	
	window_fit (pause_panel);
	window_fit (pause_frame);
}


/* SHOW_PAUSEPANEL -- Toggle display of the pause panel.
 */
static
show_pausepanel (onoff)
int	onoff;
{
	if (onoff == 0) {
	    if (pause_frame) {
		window_destroy (pause_frame);
		pause_frame = NULL;
	    }
	} else {
	    if (!pause_frame)
		create_pause_popup();
	    window_set (pause_frame,
		WIN_X,	win_xoff + 5,
		WIN_Y,	win_yoff + 5,
		0);
	    window_set (pause_frame, WIN_SHOW, TRUE, 0);
	}
}


/* CREATE_FRAME_MENU -- Gterm uses a special frame menu which provides the
 * standard frame menu as a submenu.
 */
static
create_frame_menu (frame)
Frame	frame;
{
	extern	textcopy_proc(), graphcopy_proc(), screencopy_proc();
	extern	setup_proc(), toggle_graphics(), restart_childpgrp();
	extern	toggle_logging();
	Menu	new_menu, old_menu;

	/* Get the standard frame menu. */
	old_menu = (Menu) window_get (frame, WIN_MENU);

	/* Create the new frame root menu */
	new_menu = menu_create (
	    MENU_PULLRIGHT_ITEM,
		"Frame",
		old_menu,
	    MENU_ACTION_ITEM,
		"Setup",
		setup_proc,
	    MENU_ACTION_ITEM,
		"Continue",
		restart_childpgrp,
	    MENU_ACTION_ITEM,
		"Logging on",
		toggle_logging,
	    MENU_ACTION_ITEM,
		"Show graph",
		toggle_graphics,
	    MENU_ACTION_ITEM,
		"Textcopy",
		textcopy_proc,
	    MENU_ACTION_ITEM,
		"Graphcopy",
		graphcopy_proc,
	    MENU_ACTION_ITEM,
		"Screencopy",
		screencopy_proc,
	    /*
	    MENU_ACTION_ITEM,
		"Screencopy8",
		screencopy8_proc,
	     */
	    0);

	logitem = menu_find (new_menu, MENU_STRING, "Logging on", 0);

	/* Install the new menu. */
	window_set (frame, WIN_MENU, new_menu, 0);
}


/* TOGGLE_LOGGING -- Called from the frame menu to turn logging on/off.
 */
static
toggle_logging()
{
	if (gt_logfp) {
	    fclose (gt_logfp);
	    gt_logfp = NULL;
	    menu_set (logitem, MENU_STRING, "Logging on", 0);
	} else {
	    if ((gt_logfp = fopen (logfile, "a")) == NULL)
		fprintf (stderr, "cannot open logfile %s\n", logfile);
	    else
		menu_set (logitem, MENU_STRING, "Logging off", 0);
	}
}


/* EV_GT_TTYSW -- TTY subwindow input event handler.
 */
static Notify_value
ev_gt_ttysw (frame, event, arg, type)
Frame	frame;
Event	*event;
Notify_arg arg;
Notify_event_type type;
{
	register int key = event_id(event);

	if (pause_mode && (event_is_ascii(event) || event_is_button(event))) {
	    /* If we are pausing for input before closing the workstation,
	     * complete the close workstation operation and discard the event.
	     */
	    show_pausepanel (0);
	    gio_close_workstation();
	    return (NOTIFY_DONE);
	}

	if (event_is_ascii(event)) {
	    /* Save the cursor position in tty window for later restoration. */
	    tty_lastx = event_x(event);
	    tty_lasty = event_y(event);

	    /* Ignore control codes if -ignore set? */
	    if (iscntrl(key) && key != '\r' && ignore_inputevent(event))
		return (NOTIFY_DONE);

	    /* Ignore typein into text window while a cursor read is in
	     * progress, else an invalid exit from the cursor read may result.
	     */
	    if (cursor_read_pending)
		return (NOTIFY_DONE);

	} else if (event_id(event) == KEY_TOP(7) && event_is_down(event)) {
	    /* Toggle between full screen and regular size graph. */
	    toggle_fullscreen();

	} else if (event_id(event) == KEY_TOP(8) && event_is_down(event)) {
	    /* Clear or enable the graphics plane. */
	    char    buf[4];

	    tty_lastx = event_x(event);
	    tty_lasty = event_y(event);

	    /* If already in graphics mode, clear the graphics plane, else
	     * switch to graphics mode w/o clearing the screen (the user can
	     * simply type F8 again if they also want to clear the screen).
	     */
	    if (gio_graphicsenabled && !cursor_read_pending) {
		buf[0] = '\035';
		buf[1] = '\033';
		buf[2] = '\014';
		ev_ptyoutput (buf, 3);

	    } else if (!gio_graphicsenabled && graphics_enable == GRAPHICS_ON) {
		buf[0] = '\035';
		buf[1] = '\037';
		ev_ptyoutput (buf, 2);
		/*
		window_set (gt_ttysw,
		    WIN_CONSUME_KBD_EVENTS, KEY_TOP(9), 0,
		    0);
		 */
	    }

	    return (NOTIFY_DONE);

	} else if (event_id(event) == KEY_TOP(9) && event_is_down(event)) {
	    /* Exit graphics mode, returning to text mode.
	     */
	    if (gio_graphicsenabled && !cursor_read_pending) {
		char	buf[1];

		cursor_used++;		/* disable pause on CW */
		buf[0] = '\030';
		ev_ptyoutput (buf, 1);
		/*
		window_set (gt_ttysw,
		    WIN_IGNORE_KBD_EVENTS, KEY_TOP(9), 0,
		    0);
		 */
		return (NOTIFY_DONE);
	    } else if (!gio_graphicsenabled)
		ttysw_output (gt_ttysw, "\f", 1);
	}

	/* Let frame operate upon the event. */
	return (notify_next_event_func (frame, event, arg, type));
}


/* IGNORE_INPUTEVENT -- Ignore the current input event?
 * The following is a kludge intended to prevent delivering the suspend
 * signal to a process which is run in the gterm window without benefit of
 * an intermediate shell.  This is only done if "-ignore" (stop signals)
 * is specified on the command line, and only if the command being run in
 * GTERM is not a shell.  This is a dangerous thing to do, as the characters
 * may be valid input data to a program operating in raw input mode.
 */
static
ignore_inputevent (event)
Event	*event;
{
	register int	key = event_id(event);
	struct ltchars	lt;
	struct sgttyb	sg;

	if (!shell && event_ctrl_is_down(event)) {
	    /* Control code assignments may change, so reread them. */
	    if (ioctl (pty_fd, TIOCGLTC, &lt) != -1) {
		t_suspc  = lt.t_suspc;
		t_dsuspc = lt.t_dsuspc;
	    }

	    /* Echo but ignore suspend control characters if not in raw mode. */
	    if (key == t_suspc || key == t_dsuspc) {
		if (ioctl(pty_fd,TIOCGETP,&sg) != -1 && (!(sg.sg_flags&RAW))) {
		    char out[2];
		    out[0] = '^';
		    out[1] = (key & 077) + '@';
		    ttysw_output (gt_ttysw, out, 2);
		    return (1);
		}
	    }
	}

	return (0);
}


/* EV_TTYFRAME -- TTY frame event handler.
 */
static Notify_value
ev_ttyframe (frame, event, arg, type)
Frame	frame;
Event	*event;
Notify_arg arg;
Notify_event_type type;
{
	Notify_value	value;
	static		int ignore_resize = 1;

	/* Let frame operate upon the event. */
	value = notify_next_event_func (frame, event, arg, type);

	if (event_id(event) == WIN_RESIZE) {
	    /* Tty_adjustheight, if it resizes the window, will cause a resize
	     * event to be queued and we will be called back.  Set a flag to
	     * ignore this event or an infinite loop will result.
	     */
	    if (ignore_resize)
		ignore_resize = 0;
	    else {
		ignore_resize = 1;
		tty_adjustheight (0);
	    }
	}

	return (value);
}


/* TTY_ADJUSTHEIGHT -- Called when the tty window is initially sized or when
 * it is dynamically resized to adjust the size to an integral number of lines
 * and allow space for the panel at the top (this is NOT done when the tty
 * window resizes itself, although it should be).
 */
static
tty_adjustheight (nrows)
int	nrows;
{
	struct	pixfont *pf;
	int	height;

	if (nrows == 0)
	    nrows = (int) window_get (gt_ttysw, WIN_ROWS);

	pf = (struct pixfont *) window_get (gt_baseframe, WIN_FONT);
	height = HEIGHTADJUST + pf->pf_defaultsize.y * nrows + 1;

	if (height != (int) window_get (gt_baseframe, WIN_HEIGHT))
	    window_set (gt_baseframe, WIN_HEIGHT, height, 0);
}


/* GIO_OPEN_WORKSTATION -- Called by the low level gtermio code when there is
 * output to the graphics frame but the frame is in a closed state.
 */
gio_open_workstation()
{
	cursor_used = 0;
	gio_setcursor (CURSOR_OFF, 0);
	if (openws_action == NO_ACTION)
	    return;

	if (!gio_frame_has_moved)
	    window_set (gio_frame,
		WIN_HEIGHT,	win_ysize + (TOOL_BORDERWIDTH * 2),
		WIN_WIDTH,	win_xsize + (TOOL_BORDERWIDTH * 2),
		WIN_X,	win_xoff,
		WIN_Y,	win_yoff,
		0);

	switch (openws_action) {
	case SHOW_FRAME:
	    window_set (gio_frame, WIN_SHOW, TRUE, 0);
	    break;
	case EXPOSE_FRAME:
	    set_coverwindow (gio_frame, gt_baseframe);
	    break;
	}

	/* If the setup panel is on, move it to the top. */
	if ((int) window_get (setup_frame, WIN_SHOW) == TRUE)
	    window_set (setup_frame, WIN_SHOW, TRUE, 0);
}


/* GIO_CLOSE_WORKSTATION -- Called by the low level gtermio code when the CAN
 * sequence is received, to turn off or hide the now inactive graphics window.
 */
gio_close_workstation()
{
	if (cursor_used)
	    window_set (gt_ttysw, WIN_MOUSE_XY, tty_lastx, tty_lasty, 0);
	gio_setcursor (CURSOR_ON, CROSSHAIR_OFF);

	if (closews_action != NO_ACTION)
	    if (closews_pause && !pause_mode && !cursor_used) {
		show_pausepanel (1);
		pause_mode = 1;
		return;
	    }

	/* If the window has been closed, do nothing. */
	if (window_get (gio_frame, WIN_SHOW))
	    switch (closews_action) {
	    case UNSHOW_FRAME:
		window_set (gio_frame, WIN_SHOW, FALSE, 0);
		break;
	    case HIDE_FRAME:
		set_coverwindow (gt_baseframe, gio_frame);
		break;
	    }

	pause_mode = 0;
}


/* SET_COVERWINDOW -- Make the first window cover the second if they overlap.
 */
static
set_coverwindow (win_top, win_bot)
Frame	win_top, win_bot;
{
	int	fd_top, fd_bot;

	fd_top = (int) window_get (gt_baseframe, WIN_FD);
	fd_bot = (int) window_get (gio_frame, WIN_FD);

	win_lockdata (fd_top);
	win_lockdata (fd_bot);
	win_remove   (fd_bot);
	win_setlink  (fd_bot, WL_COVERING, win_fdtonumber(fd_top)); 
	win_insert   (fd_bot);
	win_unlockdata (fd_bot);
	win_unlockdata (fd_top);
}


/* EV_GIOFRAME -- GIO frame event handler.
 */
static Notify_value
ev_gioframe (frame, event, arg, type)
Frame	frame;
Event	*event;
Notify_arg arg;
Notify_event_type type;
{
	Notify_value	value;
	int	o_xoff, o_yoff, n_xoff, n_yoff;

	o_xoff = (int) window_get (gio_frame, WIN_X);
	o_yoff = (int) window_get (gio_frame, WIN_Y);

	/* Let frame operate upon the event. */
	value = notify_next_event_func (frame, event, arg, type);

	n_xoff = (int) window_get (gio_frame, WIN_X);
	n_yoff = (int) window_get (gio_frame, WIN_Y);

	/* Determine if the graphics window has been moved by the user.
	 * If so, we don't want to move it ourselves any more.
	 */
	if (n_xoff != o_xoff || n_yoff != o_yoff) {
	    gio_frame_has_moved = 1;
	    win_xoff = n_xoff;
	    win_yoff = n_yoff;
	}

	return (value);
}


/* EV_GIOINPUT -- GIO input event handler.
 */
static Notify_value
ev_gioinput (frame, event, arg, type)
Frame	frame;
Event	*event;
Notify_arg arg;
Notify_event_type type;
{
	register int	key;
	Notify_value	value;
	char	ch;

	/* If we are pausing for input before closing the workstation,
	 * complete the close workstation operation and discard the event.
	 */
	if (pause_mode && (event_is_ascii(event) || event_is_button(event))) {
	    show_pausepanel (0);
	    gio_close_workstation();
	    return (NOTIFY_DONE);
	}

	/* Let frame operate upon the event. */
	value = notify_next_event_func (frame, event, arg, type);

	switch (key = event_id (event)) {
	case WIN_RESIZE:
	    win_xsize = (int) window_get (gio_frame, WIN_WIDTH) -
			    (TOOL_BORDERWIDTH * 2);
	    win_ysize = (int) window_get (gio_frame, WIN_HEIGHT) -
			    (TOOL_BORDERWIDTH * 2);
	    if (!gio_frame_has_moved) {
		win_xoff = (int) window_get (gio_frame, WIN_X);
		win_yoff = (int) window_get (gio_frame, WIN_Y);
	    }

	    if (!cursor_read_pending)
		gio_setcursor (CROSSHAIR_OFF, CURSOR_ON);

	    gio_getbestfont (win_xsize, win_ysize);
	    gio_reset (DEF_TEKXRES, DEF_TEKYRES, alpha_font, text_font);
	    break;

	case MS_RIGHT:
	    /* When a cursor read is not in progress, i.e., the gio window
	     * is idle, the right mouse button will cause the crosshairs to
	     * be displayed while the button is held down and the cursor is
	     * in the graphics window.  During a cursor read the right button
	     * may be used to alias a key, like the left and middle buttons
	     * below.
	     */
	    if (cursor_read_pending && event_is_down(event)) {
		if (event_ctrl_is_down (event))
		    key_right = last_key;
		else if (key = key_right)
		    goto readcur;
	    } else if (!cursor_read_pending) {
		if (event_is_down (event))
		    gio_setcursor (CROSSHAIR_ON, CURSOR_ON);
		else
		    gio_setcursor (CROSSHAIR_OFF, 0);
	    }
	    break;

	case MS_LEFT:
	case MS_MIDDLE:
	    /* The left and middle mouse buttons may be used while in the
	     * graphics window to alias keyboard events.  Typing ctrl/button
	     * causes the last key to be aliased with the indicated button.
	     * Thereafter, pressing that mouse button during a cursor read
	     * causes the cursor read to terminate, returning the aliased
	     * key just as if the key had been typed on the keyboard.
	     */
	    if (event_is_down (event))
		if (event_ctrl_is_down (event)) {
		    if (key == MS_LEFT)
			key_left = last_key;
		    else
			key_middle = last_key;
		} else if (cursor_read_pending) {
		    if (key == MS_LEFT)
			key = key_left;
		    else
			key = key_middle;
		    if (key)
			goto readcur;
		}
	    break;
	
	case KEY_TOP(7):
	    /* Toggle full screen graphics mode.
	     */
	    if (event_is_down(event))
		toggle_fullscreen();
	    break;

	case KEY_TOP(8):
	    /* Clear the graphics screen, leaving the terminal in graphics
	     * mode.
	     */
	    if (event_is_down(event) && !cursor_read_pending) {
		char    buf[3];

		buf[0] = '\035';
		buf[1] = '\033';
		buf[2] = '\014';
		ev_ptyoutput (buf, 3);
	    }
	    break;

	case KEY_TOP(9):
	case KEY_LEFT(7):
	    /* Exit graphics mode, returning to text mode.
	     */
	    if (event_is_down(event) && !cursor_read_pending) {
		char	buf[1];

		/*
		window_set (gt_ttysw,
		    WIN_IGNORE_KBD_EVENTS, KEY_TOP(9), 0,
		    0);
		 */

		if (key == KEY_LEFT(7))		/* delay CW until after L7 */
		    gio_pause (500);

		cursor_used++;			/* disable pause on CW */
		buf[0] = '\030';
		ev_ptyoutput (buf, 1);
	    }
	    break;

	default:
	    /* Terminate a cursor read, returning the encoded cursor value
	     * sequence to the terminal output.
	     */
	    if (event_is_down(event) &&
	       (event_is_ascii(event) || event_is_key_right(event))) {

		/* Ignore control codes if -ignore set? */
		if (iscntrl(key) && key != '\r' && ignore_inputevent(event))
		    return (value);

		/* Terminate cursor read? */
		if (cursor_read_pending) {
		    /* Map keypad function keys to digits. */
		    if (event_is_key_right(event)) {
			switch (key = event_id(event) - KEY_RIGHT(1) + 1) {
			case  7: case  8: case  9:
			    break;
			case 10: case 11: case 12:
			    key -= 6;
			    break;
			case 13: case 14: case 15:
			    key -= 12;
			    break;
			default:
			    return (value);
			}
			key += '0';
		    }
readcur:
		    last_x = event_x (event);
		    last_y = event_y (event);
		    last_key = key;

		    cursor_read_pending = 0;
		    gio_setcursor (CURSOR_OFF, CROSSHAIR_OFF);
		    gio_retcursor (key, last_x, last_y);
		    enable_arrow_keys();

		} else
		    write (pty_fd, (ch=key, &ch), 1);
	    }
	}

	return (value);
}


/* SIG_TSTP -- Signal handler for signal SIGTSTP.
 */
static Notify_func
sig_tstp()
{
	kill (getpid(), SIGSTOP);
}


/* RESTART_CHILDPGRP -- Send the SIGCONT signal to the process group
 * associated with this terminal, e.g., to restart the processes after
 * the user has accidentally typed the stop character.
 */
static
restart_childpgrp()
{
	int	pgrp;

	ioctl (pty_fd, TIOCGPGRP, &pgrp);
	killpg (pgrp, SIGCONT);
}


/* GIO_GETBESTFONT -- Scan the font table and open the tek-alpha font which
 * best fits the given size window.  The important dimension is the window
 * width, since the character spacing is fixed.  Also pick a text (status
 * line) font small enough to provide space for at least 80 chars.
 */
static
gio_getbestfont (xsize, ysize)
int	xsize, ysize;		/* canvas size, x,y */
{
	register struct fonttab *ft, *o_ft;
	register int	i;
	struct	pixfont *newfont;
	char	pathname[256];

	/* Select tek-alpha font.
	 */
	for (i=0, o_ft=ft=alpha_fonts;  ft->pointsize != 0;  o_ft=ft++, i++)
	    if (ft->win_xsize > xsize)
		break;

	if (i == 0)
	    i = 1;

	sprintf (pathname, o_ft->path, o_ft->pointsize);
	if ((newfont = pf_open (pathname)) == NULL) {
	    fprintf (stderr, "cannot open font %s\n", pathname);
	    return (-1);
	} else if (alpha_font != NULL)
	    pf_close (alpha_font->pixfont);

	alpha_font = o_ft;
	alpha_font->pixfont = newfont;
	alpha_font_index = i - 1;

	/* Load the text (status line) font.
	 */
	o_ft = &text_fonts[i-1];
	sprintf (pathname, o_ft->path, o_ft->pointsize);
	if ((newfont = pf_open (pathname)) == NULL) {
	    fprintf (stderr, "cannot open font %s\n", pathname);
	    return (-1);
	} else if (text_font != NULL)
	    pf_close (text_font->pixfont);

	text_font = o_ft;
	text_font->pixfont = newfont;
	text_font_index = i - 1;

	return (0);
}


/* GIO_SETCURSOR -- Set graphics frame cursor options.
 */
gio_setcursor (op1, op2)
int	op1, op2;
{
	Cursor	cursor;
	int	option[2], i;
	int	type=cursor_type, show=cursor_show;

	/* Normalize the argument list. */
	for (option[0]=op1, option[1]=op2, i=0;  i < 2;  i++)
	    switch (option[i]) {
	    case CROSSHAIR_OFF:
	    case CROSSHAIR_ON:
		type = option[i];
		break;
	    case CURSOR_OFF:
	    case CURSOR_ON:
		show = option[i];
		break;
	    }

	/* Do we need to change anything? */
	if (type == cursor_type && show == cursor_show)
	    return;

	/* Modify the cursor attributes. */
	cursor = window_get (gio_canvas, WIN_CURSOR);
	cursor_set (cursor,
	    CURSOR_SHOW_CURSOR, FALSE,
	    CURSOR_SHOW_CROSSHAIRS,  (show==CURSOR_ON) ? TRUE : FALSE,
	    CURSOR_CROSSHAIR_THICKNESS, 1,
	    CURSOR_CROSSHAIR_LENGTH, (type==CROSSHAIR_ON) ? CURSOR_TO_EDGE : 10,
	    CURSOR_CROSSHAIR_OP, PIX_SRC,
	    CURSOR_CROSSHAIR_COLOR, 1,
	    0);
	window_set (gio_canvas, WIN_CURSOR, cursor, 0);

	cursor_type = type;
	cursor_show = show;
}


/* GIO_SETCURSORPOS -- Set the position of the graphics cursor within the
 * graphics frame.
 */
gio_setcursorpos (x, y)
int	x, y;			/* pixwin pixel coords */
{
	window_set (gio_canvas, WIN_MOUSE_XY, x, y, 0);
	last_x = x;
	last_y = y;
	cursor_used++;
}


/* GIO_READCURSOR -- Initiate a cursor read.  Set the cursor type to
 * a full crosshair cursor to indicate to the user that the program is
 * waiting for cursor input.  Set the cursor read pending flag so that
 * the next input event in graphics window will cause termination of the
 * cursor read and transmission of the cursor value to the terminal output.
 */
gio_readcursor()
{
	/* When a cursor read is initiated, move the cursor into the gio
	 * window so that the user knows that cursor input is expected.
	 */
	if (last_x < 0) {
	    last_x = win_xsize / 2;
	    last_y = win_ysize / 2;
	}

	/* Disable the mapping of the right function keys to the ansi arrow
	 * key escape sequences, since we want to receive these keys as
	 * function key events.
	 */
	disable_arrow_keys();

	gio_setcursorpos (last_x, last_y);
	gio_setcursor (CURSOR_ON, CROSSHAIR_ON);
	cursor_read_pending++;
	cursor_used++;
}


/* TOGGLE_GRAPHICS -- Show or hide the graphics frame.
 */
static
toggle_graphics()
{
	if ((int) window_get (gio_frame, WIN_SHOW) == TRUE)
	    window_set (gio_frame, WIN_SHOW, FALSE, 0);
	else {
	    if (!gio_frame_has_moved)
		window_set (gio_frame,
		    WIN_HEIGHT,	win_ysize + (TOOL_BORDERWIDTH * 2),
		    WIN_WIDTH,	win_xsize + (TOOL_BORDERWIDTH * 2),
		    WIN_X,		win_xoff,
		    WIN_Y,		win_yoff,
		    0);
	    window_set (gio_frame, WIN_SHOW, TRUE, 0);

	    /* If the setup panel is on, move it to the top. */
	    if ((int) window_get (setup_frame, WIN_SHOW) == TRUE)
		window_set (setup_frame, WIN_SHOW, TRUE, 0);
	}
}


/* RESET_PROC -- Called from the setup panel to reset the state of the
 * terminal.
 */
static
reset_proc()
{
	/* Cancel any pending cursor read. */
	if (cursor_read_pending) {
	    gio_retcursor ('\r', 0, 0);
	    enable_arrow_keys();
	}

	/* Cancel logging if enabled. */
	if (gt_logfp) {
	    fclose (gt_logfp);
	    gt_logfp = NULL;
	    menu_set (logitem, MENU_STRING, "Logging on", 0);
	}

	/* Restore user settable options. */
	restore_params();

	/* Reset internal state variables. */
	cursor_type = -1;		/* crosshair or small cursor	*/
	cursor_show = -1;		/* cursor on or off		*/
	cursor_read_pending = 0;	/* waiting for cursor event	*/
	key_left = 0;			/* key aliased to left msbutton	*/
	key_middle = 0;			/* key aliased to mid msbutton	*/
	key_right = 0;			/* key aliased to mid msbutton	*/
	last_key = 0;			/* last cursor read key		*/
	last_x= -1,last_y= -1;		/* last cursor read position	*/ 
	setup_xoff = 150;		/* offset to setup panel	*/
	setup_yoff = 150;
	red[0] = green[0] = 0;
	red[1] = green[1] = 255;
	blue[0] = 128; blue[1] = 0;
	gio_frame_has_moved = 0;
	pause_mode = 0;
	cursor_used = 0;

	/* Reset internal state. */
	notify_remove_event_func (gio_canvas, ev_gioinput, NOTIFY_SAFE);
	notify_remove_event_func (gio_frame, ev_gioframe, NOTIFY_SAFE);
	window_destroy (gio_frame);
	parse_args (main_argc, main_argv,
	    &tty_argc, tty_argv, &gio_argc, gio_argv);
	create_gio_popup (gio_argc, gio_argv);
	notify_interpose_event_func (gio_frame, ev_gioframe, NOTIFY_SAFE);
	notify_interpose_event_func (gio_canvas, ev_gioinput, NOTIFY_SAFE);
	gio_setup (pty_fd, pw);
	gio_hardreset (DEF_TEKXRES, DEF_TEKYRES, alpha_font, text_font);
	gio_enable (graphics_enable);

	if (pause_frame)
	    window_destroy (pause_frame);

	window_destroy (setup_frame);
	create_setup_popup (0, NULL);
	panel_set_item();
	window_set (setup_frame, WIN_SHOW, TRUE, 0);
}


/* SAVE_PARAMS -- Save the user settable options.
 */
static
save_params()
{
	s_clip_graphics = clip_graphics;
	s_graphics_enable = graphics_enable;
	s_openws_action = openws_action;
	s_closews_action = closews_action;
	s_closews_pause = closews_pause;
	s_canvas_retained = canvas_retained;
	s_reverse_video = reverse_video;
	s_color = color;
	s_win_xsize = win_xsize;
	s_win_ysize = win_ysize;
	s_win_xoff = win_xoff;
	s_win_yoff = win_yoff;
	s_ignore = ignore;
	strcpy (s_gin_modeterm, gin_modeterm);
}


/* RESTORE_PARAMS -- Restore the user settable options.
 */
static
restore_params()
{
	clip_graphics = s_clip_graphics;
	graphics_enable = s_graphics_enable;
	openws_action = s_openws_action;
	closews_action = s_closews_action;
	closews_pause = s_closews_pause;
	canvas_retained = s_canvas_retained;
	reverse_video = s_reverse_video;
	color = s_color;
	win_xsize = s_win_xsize;
	win_ysize = s_win_ysize;
	win_xoff = s_win_xoff;
	win_yoff = s_win_yoff;
	ignore = s_ignore;

	strcpy (gin_modeterm, s_gin_modeterm);
	gio_setginmodeterm (gin_modeterm);
}


/* CLEAR_PROC -- Called when the clear button is clicked to transmit the
 * screen clear sequence to the TTY subwindow.
 */
static
clear_proc()
{
	static	char clearscreen[] = "\f";
	ttysw_output (gt_ttysw, clearscreen, strlen(clearscreen));
}


/* GCLEAR_PROC -- Called when the gclear button is clicked to clear the
 * graphics window.
 */
static
gclear_proc()
{
	static	char clearscreen[] = "\035\033\f";
	ev_ptyoutput (clearscreen, strlen(clearscreen));
}


/* PRINT_USAGE -- Print instructions on how to use this window tool.
 */
static
print_usage (toolname)
char	*toolname;
{
	char	*bstyle = "[-B boldstyle] ";
	
	fprintf (stderr,
	"syntax: %s [-C] %s[program [args]]\n", toolname, bstyle);
	fprintf (stderr,
	"-C	redirect console output to this instance of %s\n", toolname);
	fprintf (stderr,
	"-B	set boldstyle for this instance of %s\n", toolname);
	fprintf (stderr,
	"	where boldstyle is a number from 1 to 8\n");
	fprintf (stderr,
	"-I	input the next argument to the shell run from %s\n", toolname);
}
