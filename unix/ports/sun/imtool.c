/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <suntool/walkmenu.h>
#include <suntool/tool_struct.h>
#include <sunwindow/win_cursor.h>
#include <sys/fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "imtool.h"

/*
 * IMTOOL -- Software image display for the Sun workstation under Sunview.
 * A variable sized window is used to peer into a "fixed" size frame buffer
 * at an arbitrary offset (pan) and scale (zoom/dezoom).  The number of frames
 * and the size of a frame buffer is arbitrary and is a setup option.  The
 * depth of a frame on an 8 bit display is 7 bits with a 1 bit graphics
 * overlay plane; this leaves almost half of the color table space for use
 * by other windows.  A data stream command interface is provided for software
 * control of the display server by other processes, not necessarily on the
 * same node.
 *
 * D.Tody, April 1987 (NOAO/IRAF project)
 * Extensively revised and extended December 1987.
 * Zoom and further datastream mods (WCS) added May 1989.
 * (but NeWS/X is coming soon and probably none of this will work anymore...)
 */

#ifndef abs
#define	abs(a)		(((a)<0)?(-(a)):(a))
#endif

#ifndef min
#define min(a,b)	((a)<(b)?(a):(b))
#endif
#ifndef max
#define max(a,b)	((a)<(b)?(b):(a))
#endif

/* Default values, size limiting values.
 */
#define	MAX_ARGS		50	/* max command line arguments	*/
#define	MAX_FBCONFIG		128	/* max possible frame buf sizes	*/
#define	MAX_FRAMES		16	/* max number of frames		*/
#define	FRAME_CHOICES	"1","2","3","4"	/* nframes choices for setup	*/
#define	NGREY			256	/* max physical greylevels	*/
#define	CLOCK_INTERVAL		500	/* main clock tick, msec	*/
#define	DEF_NCONFIG		1	/* number of f.b. configs	*/
#define	DEF_NFRAMES		1	/* save memory; only one frame	*/
#define	DEF_FRAME_WIDTH		512	/* 512 square frame		*/
#define	DEF_FRAME_HEIGHT	512	/* 512 square frame		*/
#define	DEF_GIOWIN_SIZE		512	/* default size gio window	*/
#define	SZ_LABEL		256	/* main frame label string	*/
#define	SZ_IMTITLE		128	/* image title string		*/
#define	SZ_WCTEXT		80	/* WCS box text 		*/
#define	SZ_WCSBUF		320	/* WCS text buffer size		*/
#define	SZ_PANTEXT		18	/* displayed chars in o_fname	*/
#define	SZ_COLORBAR		11	/* height of colorbar in pixels	*/
#define	SZ_FNAME		128
#define	SZ_LINE			256
#define	IO_TIMEOUT		30	/* i/o not getting anywhere	*/
#define	SZ_FIFOBUF		4000	/* transfer size for FIFO i/o	*/
#define	BKG_WHITE		0
#define	BKG_BLACK		1
#define	P_IMAGE			1
#define	P_GRAPHICS		2
#define	P_COLORBAR		4
#define	P_DONTCLIP		8
#define	PANNER_EVENT		99
#define	INTERRUPT		003
#define	NEXT_SCREEN		006
#define	PREV_SCREEN		022
#define	CYCLE_BLINK		002

/* Reserved colormap entries.
 */
#define	CMS_DATASTART		1	/* first data greylevel		*/
#define	CMS_DATAEND		200	/* last data greylevel		*/
#define	CMS_DATARANGE		200	/* number of data greylevels	*/
#define	CMS_CURSOR		201	/* cursor color table entry	*/
#define	CMS_BACKGROUND		202	/* background color		*/
#define	CMS_GRAPHICSSTART	203	/* first graphics greylevel	*/
#define	CMS_GRAPHICSEND		217	/* last graphics greylevel	*/
#define	CMS_GRAPHICSRANGE	15	/* number of graphics greylevel	*/
#define	CMS_GCOLOR(c)        (203+(c))	/* graphics color table entries	*/
#define	CMS_FIRST		1	/* first grey level used	*/
#define	CMS_NGREY		218	/* total number of grey levels	*/

/* Magic numbers. */
#define	BLINK_OFF	1		/* crosshair cursor off		*/
#define	BLINK_ON	2		/* turn crosshair cursor on	*/
#define	CMMAPNAME	"imtool"	/* color map segment name	*/
#define	OLD_DEVNAME	"/dev/imt1"	/* original pseudodevice name	*/
#define	I_DEVNAME	"/dev/imt1o"	/* pseudo device names		*/
#define	O_DEVNAME	"/dev/imt1i"	/* our IN is client's OUT	*/
#define	KEY_SETUP	KEY_TOP(4)	/* togglt the setup panel	*/
#define	KEY_REMARK	KEY_TOP(5)	/* remark list of objects	*/
#define	KEY_PCOORDS	KEY_TOP(6)	/* cont. coords display		*/
#define	KEY_SNAP	KEY_TOP(7)	/* image hardcopy key		*/
#define	TEXT_FONT	"/usr/lib/fonts/fixedwidthfonts/screen.b.14"
#define	MARK_FONT	"/usr/lib/fonts/fixedwidthfonts/screen.r.11"
#define	COORDFILE	"frame.%d.%d"
#define	FBCONFIG_1	".imtoolrc"
#define	FBCONFIG_2	"/usr/local/lib/imtoolrc"
#define	FBCONFIG_ENV1	"imtoolrc"
#define	FBCONFIG_ENV2	"IMTOOLRC"

#define	R_TYPE		0		/* 0=postscript, 1=rasterfile */
#define	R_DISPOSE	"lpr -s %s"	/* dispose command */
#define	R_FILENAME	""		/* output filename */

/* Global state codes. */
#define	TRACK_CURSOR	0
#define	WINDOW		1
#define	ROAM		2

#define	MONO		0
#define	HEAT		1
#define	RAMP1		2
#define	RAMP2		3
#define	HALLEY		4
#define	LINEARPS	5
#define	RANDOMPS	6
#define	CRANDOMPS	7
#define	ULUT1		8
#define	ULUT2		9

/* WCS definitions. */
#define	W_UNITARY	0
#define	W_LINEAR	1
#define	W_LOG		2
#define	W_DEFFORMAT	" %7.2f %7.2f %7.1f%c"

/* Internal variables. */
static	int snap_frame_too = 1;		/* include frame in imcopy?	*/
static	int cursor_blink = -1;		/* crosshair or small cursor	*/
static	int cursor_show = -1;		/* cursor state; on/off		*/
static	int display_coords = 0;		/* cont. display cursor coords 	*/
static	int p_cursor_setback = 0;	/* restore cursor pos after pan */
static	int panning = 0;		/* smooth image pan in progress */
static	int maptype = MONO;		/* initial mapping type		*/
static	char o_fname[SZ_FNAME];		/* name of coord output file	*/
static	char o_revtext=1;		/* reverse video text?		*/
static	int setup_xoff = 4;		/* offset to setup panel	*/
static	int setup_yoff = 18;		/* 	"	"		*/
static	int wc_xoff=0, wc_yoff=0;	/* offset to WCS output box	*/
static	int wc_width, wc_height;	/* size of WCS output box	*/
static	char wc_text[SZ_WCTEXT+1];	/* coordinate text		*/
static	struct pixfont *wc_font = NULL;	/* WCS box font			*/
static	int cb_height = SZ_COLORBAR;
static	int show_colorbar = 1;
static	int black = 0;
static	int white = NGREY - 1;
static	int background = 0;		/* background color index	*/
static	int state = TRACK_CURSOR;	/* mouse button state		*/
static	int last_sx, last_sy;		/* window x,y of last event	*/
static	int save_sx, save_sy;		/* save absolute mouse position	*/
static	int last_bx, last_by;		/* last button press		*/
static	int fb_bkgcolor_index = BKG_WHITE;
static	int window_open = 1;
static	int global_colortable = 1;	/* globally manage colortable	*/
static	int fbconfig = 0;
static	int last_x, last_y, last_key, key_left=0;
static	int reading_imcursor = 0;
static	int imcursor_wcs;
static	char wcsbuf[MAX_FRAMES][SZ_WCSBUF];

#define	CRANDOM_CHOICES	"1", "2", "4", "8", "16", "32"
static	int cr_msec[] = { 1000, 2000, 4000, 8000, 16000, 32000 };
static	int crandom_blink_rate = 1;	/* rate for cont. random pseudo	*/

/* Blink parameters. */
#define	BLINKRATE_CHOICES "1/2","1","2","4","8","16","32"
static	int blink_rate[] = { 1, 2, 4, 8, 16, 32, 64 };
static	int blink_rate_index = 1;	/* current blink rate 		*/
static	int blink_timer = 0;		/* nticks left on clock		*/
static	int blink_frame = 0;		/* current blink frame		*/
static	int blink = 0;			/* blink enabled?		*/
static	int n_blink_frames = 8;		/* number of frames to blink	*/
static	int blink_frames[MAX_FRAMES] = { 1, 2, 3, 4, 5, 6, 7, 8 };
static	char s_blinklist[SZ_FNAME+1] = "all";	/* list of frames to blink */

/* Logical image cursor parameters. */
static	int marktype = 0;		/* mark cursor position		*/

/* The following is needed to provide sufficient precision to allow for
 * zooming virtual pixrects (Rects use type short).
 */
typedef struct bigrect {
	int	r_left, r_top;
	int	r_width, r_height;
} BRect;

/* Predefined lookup tables. */
struct triplet {
	float red, green, blue;
};
struct lut {
	int	lutlen;
	struct	triplet hue[NGREY];
};

struct lut heat = {
#include "heat.lut"
};

struct lut halley = {
#include "halley.lut"
};

/* User defined lookup tables. */
static	char u_lut1[SZ_FNAME+1] = "none";
static	char u_lut2[SZ_FNAME+1] = "none";

static	BRect pw_rect;			/* raw frame buffer		*/
static	int fb_depth  = 8;
static	int fb_ngrey  = CMS_DATARANGE;
static	int fb_nframes= DEF_NFRAMES;
static	int Fb_width  = DEF_FRAME_WIDTH;
static	int Fb_height = DEF_FRAME_HEIGHT;

static	BRect fb_rect;			/* zoomed frame buffer		*/
static	int fb_width  = DEF_FRAME_WIDTH;
static	int fb_height = DEF_FRAME_HEIGHT;

#define	MAX_ZOOMS 8
#define	ZOOM_CHOICES "1","2","4","8"
static	int zoom = 1;
static	int nzooms = 4;			/* number of zoom factors	*/
static	int zoom_index = 0;
static	int zooms[MAX_ZOOMS] = { 1, 2, 4, 8 };
static	char s_zoomslist[SZ_FNAME+1] = "1 2 4 8";

static	int gio_xsize = DEF_GIOWIN_SIZE;
static	int gio_ysize = DEF_GIOWIN_SIZE;
static	int initial_gio_xsize, initial_gio_ysize;

/* Rotation matrix defining world coordinate system (WCS) of a frame.
 */
struct ctran {
	int	valid;			/* has WCS been set?		*/
	float	a, b;			/* x, y scale factors		*/
	float	c, d;			/* x, y cross factors		*/
	float	tx, ty;			/* x, y translation		*/
	float	z1, z2;			/* greyscale range		*/
	int	zt;			/* greyscale mapping		*/
	char	format[32];		/* wcs output format		*/
	char	imtitle[SZ_IMTITLE+1];	/* image title from WCS	*/
};

/* The frame buffers. */
struct framebuf {
	struct	pixrect *fb_pr;		/* frame buffer pixrect		*/
	int	fb_frameno;		/* frame number			*/
	int	fb_xoff, fb_yoff;	/* fb coords of pixwin (0,0)	*/
	int	fb_xzoom, fb_yzoom;	/* zoom/dezoom factors		*/
	int	fb_objno;		/* object number		*/
	int	fb_imageno;		/* displayed frame counter	*/
	int	fb_maptype;		/* greyscale transformation	*/
	float	fb_center, fb_slope;	/* transfer function 		*/
	struct	ctran fb_ctran;		/* world coordinate system	*/
	char	fb_label[SZ_LABEL+1];	/* frame label string		*/
};

/* Possible frame buffer sizes. */
struct fbconfig {
	int	nframes;		/* number of frames		*/
	int	width;			/* frame buffer width		*/
	int	height;			/* frame buffer height		*/
};

static	int display_frame;		/* currently displayed frame	*/
static	int reference_frame;		/* reference (cmd i/o) frame	*/
static	struct framebuf *frames=NULL;	/* array of frame descriptors	*/
static	struct framebuf *df_p;		/* display frame descriptor	*/
static	struct framebuf *rf_p;		/* reference frame descriptor	*/
static	struct pixrect *cb_pr;		/* colorbar pixrect		*/
static	int fb_nconfig = 0;
static	int fb_config_index = 0;
static	struct fbconfig fb_config[MAX_FBCONFIG];
static	char startfile[SZ_FNAME+1];	/* image read on startup	*/
static	char rasterfile[SZ_FNAME+1] = "raster.%d.%d";

/* Screendump stuff. */
int	r_type = R_TYPE;
char	r_dispose[SZ_FNAME+1] = R_DISPOSE;
char	r_filename[SZ_FNAME+1] = R_FILENAME;

#define HEIGHTADJUST \
	(tool_headerheight((int)window_get(gio_frame, FRAME_SHOW_LABEL)) + \
	TOOL_BORDERWIDTH)

static	short iconimage[] = {
#include "imtool.icon"
};
DEFINE_ICON_FROM_IMAGE (icon, iconimage);

Window	gio_frame, gio_canvas;
static	int	gio_frame_fd;
static	int	datain, dataout;
static	Menu_item blink_item;
static	Panel	setup_panel;
static	Window	setup_frame;
static	Panel_item pan_show_colorbar, pan_globalcolor;
static	Panel_item pan_set_nframes, pan_blink_rate, pan_blink_list;
static	Panel_item pan_set_maptype, pan_crandom_rate, pan_set_ofname;
static	Panel_item pan_snapframetoo, pan_set_background, pan_set_rfname;
static	Panel_item pan_set_rtype, pan_set_rdispose, pan_set_rfilename;
static	Panel_item pan_set_marker, pan_zoom_list;
static	Panel_item pan_set_ulut1, pan_set_ulut2;
static	unsigned char red[NGREY], blue[NGREY], green[NGREY];
static	unsigned char m_red[NGREY], m_blue[NGREY], m_green[NGREY];

static	struct	pixwin *gio_pw;
static	int	main_argc, gio_argc;
static	char	**main_argv, *gio_argv[MAX_ARGS];

static	Notify_value ev_gioframe(), set_colortable();
static	Notify_value ev_gioinput(), ev_cmdinput(), ev_panner();
static	char *getfname(), *framelabel();
static	struct ctran *wcs_update();
static	struct pixrect *get_screen_rect();
static	refresh_display();
extern	char *getenv();


/* IMTOOL_MAIN -- Create the Imtool windows, i.e., the main display window,
 * the region of interest or "cursor" subwindow, and the setup panel.  There
 * are two principal event handlers, the display window event handler, used
 * to process cursor input and mouse button commands, and the command input
 * event handler, used to communicate with the client process.
 */
#ifdef STANDALONE
main (argc, argv)
#else
imtool_main (argc, argv)
#endif
int	argc;
char	**argv;
{
	char	*s;

	main_argc = argc;
	main_argv = argv;
	parse_args (argc, argv, &gio_argc, gio_argv);

	/* Screendump stuff. */
	if (s = getenv ("R_DISPOSE"))
	    strcpy (r_dispose, s);
	if (s = getenv ("R_FILENAME"))
	    strcpy (r_filename, s);
	if (s = getenv ("R_RASTERFILE")) {
	    strcpy (r_filename, s);
	    r_type = 1;
	}

	gio_frame = window_create (NULL, FRAME,
		FRAME_ICON,		&icon,
		FRAME_LABEL,
		    "imtool - NOAO/IRAF Sunview Image Display V1.1",
		FRAME_ARGS,		gio_argc, gio_argv,
		FRAME_NO_CONFIRM,	FALSE,
		0);
	if (gio_frame == NULL)
	    exit (1);

	gio_frame_fd = (int) window_get (gio_frame, WIN_FD);
	parse_args (argc, argv, &gio_argc, gio_argv);
	create_gio_canvas (gio_argc, gio_argv);
	create_frame_menu (gio_frame);
	create_setup_popup();
	notify_interpose_event_func (gio_frame,  ev_gioframe, NOTIFY_SAFE);
	notify_interpose_event_func (gio_canvas, ev_gioinput, NOTIFY_SAFE);

	get_fbconfig();
	set_fbconfig (fbconfig, 0);
	load_testpattern (0);
	set_colortable();
	gio_setcursor (CURSOR_ON, BLINK_OFF);
	set_transfer_function (gio_pw, df_p->fb_center, df_p->fb_slope);

	Bpw_get_region_rect (gio_pw, &pw_rect);
	pw_rect.r_left = df_p->fb_xoff;
	pw_rect.r_top  = df_p->fb_yoff;
	init_colorbar (pw_rect.r_width);

	if (startfile[0])
	    load_raster (rf_p->fb_pr, startfile);

	initial_gio_xsize = gio_xsize;
	initial_gio_ysize = gio_ysize;

	/* Open the output fifo.  We have to open it ourselves first as a
	 * client to get around the fifo open-no-client error.
	 */
	if ((datain = open (O_DEVNAME, O_RDONLY|O_NDELAY)) != -1) {
	    if ((dataout = open (O_DEVNAME, O_WRONLY|O_NDELAY)) != -1)
		fcntl (dataout, F_SETFL, O_WRONLY);
	    close (datain);
	}

	/* Open the input stream, a FIFO pseudodevice file used by
	 * applications to send us commands and data.
	 */
	if ((datain = open (I_DEVNAME, O_RDONLY|O_NDELAY)) == -1) {
	    if ((datain = open (OLD_DEVNAME, O_RDONLY|O_NDELAY)) == -1)
		fprintf (stderr, "Warning: cannot open %s\n", I_DEVNAME);
	} else {
	    /* Clear O_NDELAY for reading. */
	    fcntl (datain, F_SETFL, O_RDONLY);
	    notify_set_input_func (gio_frame, ev_cmdinput, datain);
	}

	imtool_clock();
	window_main_loop (gio_frame);
	close (datain);
	exit (0);
}


/* PARSE_ARGS -- Parse the argument list into the arguments for the main frame
 * and the global arguments for the server.
 */
static
parse_args (argc, argv, gio_argc, gio_argv)
int	argc;
char	*argv[];
int	*gio_argc;
char	*gio_argv[];
{
	register char	*argp;
	register int	arg;
	static	int ncalls = 0;

	gio_argv[0] = argv[0];
	*gio_argc = 1;

	for (arg=1;  arg <= argc && (argp = argv[arg]) != NULL;  arg++) {
	    if (strncmp (argp, "-fbconfig", 3) == 0) {
		/* Set the frame buffer configuration.  */
		if ((argp = argv[arg+1]) && isdigit(*argp)) {
		    fbconfig = max (0, atoi(argp) - 1);
		    arg++;
		} else if (ncalls == 0)
		    fprintf (stderr, "-fbconfig argument missing\n");
	    } else if (strncmp (argp, "-raster", 4) == 0) {
		if ((argp = argv[arg+1])) {
		    strcpy (startfile, argp);
		    arg++;
		}
	    } else
		gio_argv[(*gio_argc)++] = argp;
	}

	gio_argv[(*gio_argc)] = NULL;
	ncalls++;
}


/* GET_FBCONFIG -- Read the IMTOOL startup file to get the set of possible
 * frame buffer sizes.
 *
 * File format:		configno nframes width height [extra fields]
 *	e.g.,			1  2  512  512
 *				2  2  800  800
 *				3  1 1024 1024		# comment
 */
static
get_fbconfig()
{
	register char	*ip;
	register FILE	*fp;
	int	config, nframes, width, height, i;
	char	lbuf[SZ_LINE+1], *fname;

	/* Initialize the config table. */
	for (i=0;  i < MAX_FBCONFIG;  i++)
	    fb_config[i].nframes = 0;

	/* Attempt to open the config file. */
	fp = NULL;
	if ((fname=getenv(FBCONFIG_ENV1)) || (fname=getenv(FBCONFIG_ENV2)))
	    fp = fopen (fname, "r");
	if (!fp && (fname = getenv ("HOME"))) {
	    sprintf (lbuf, "%s/%s", fname, FBCONFIG_1);
	    fp = fopen (fname = lbuf, "r");
	}
	if (!fp)
	    fp = fopen (fname = FBCONFIG_2, "r");

	/* If cannot find a config file, set up the default configuration. */
	if (!fp) {
	    fb_config_index = fbconfig = 0;
	    fb_nconfig = DEF_NCONFIG;
	    fb_config[0].nframes = DEF_NFRAMES;
	    fb_config[0].width   = DEF_FRAME_WIDTH;
	    fb_config[0].height  = DEF_FRAME_HEIGHT;
	    return;
	}

	/* Scan the frame buffer configuration file.
	 */
	fb_nconfig = 0;
	while (fgets (lbuf, SZ_LINE, fp) != NULL) {
	    /* Skip comment lines and blank lines. */
	    for (ip=lbuf;  *ip == ' ' || *ip == '\t';  ip++)
		;
	    if (*ip == '\n' || *ip == '#')
		continue;
	    if (!isdigit (*ip))
		continue;
	    switch (sscanf (ip, "%d%d%d%d", &config,&nframes,&width,&height)) {
	    case 4:
		break;			/* normal case */
	    case 3:
		height = width;		/* default to square format */
		break;
	    default:
		fprintf (stderr, "imtool: bad config `%s'\n", ip);
		continue;
	    }

	    nframes = max (1, nframes);
	    width   = max (1, width);
	    height  = max (1, height);

	    /* Since the frame buffer is stored in a memory pixrect
	     * (effectively), the line length should be an integral number
	     * of 16 bit words.
	     */
	    if (width & 1) {
		fprintf (stderr, "imtool warning: fb config %d [%d-%dx%d] - ",
		    config, nframes, width, height);
		fprintf (stderr, "frame width should be even, reset to %d\n",
		    --width);
	    }

	    config = max(1, min(MAX_FBCONFIG, config)) - 1;
	    fb_config[config].nframes = nframes;
	    fb_config[config].width   = width;
	    fb_config[config].height  = height;
	    fb_nconfig = max (fb_nconfig, config+1);
	}

	fclose (fp);
}


/* SET_FBCONFIG -- Setup a frame buffer configuration.
 */
static
set_fbconfig (config, nframes)
int	config;
int	nframes;
{
	register struct pixrect *pr = get_screen_rect();
	int	old_config = fb_config_index;
	int	old_nframes = fb_nframes;
	char	text[SZ_LINE];

	if (config < 0 || config >= fb_nconfig) {
	    fprintf (stderr,
		"imtool: no such frame buffer configuration - %d\n", config+1);
	    return;
	} else if (nframes <= 0)
	    nframes = fb_config[config].nframes;

	if (init_framebuffers (config, nframes) == -1) {
	    fprintf (stderr, "restore configuration %d\n", old_config + 1);
	    if (init_framebuffers (old_config, old_nframes) == -1) {
		fprintf (stderr,
		"cannot restore frame buffer configuration - imtool dies\n");
		exit (1);
	    }
	}

	if (gio_xsize <= 0 || gio_xsize > Fb_width)
	    gio_xsize = Fb_width;
	if (gio_ysize <= 0 || gio_ysize > (Fb_height + cb_height))
	    gio_ysize = Fb_height + cb_height;

	gio_xsize = min (pr->pr_width - TOOL_BORDERWIDTH * 2, gio_xsize);
	gio_ysize = min (pr->pr_height
	    - tool_headerheight ((int)window_get(gio_frame,FRAME_SHOW_LABEL))
	    - TOOL_BORDERWIDTH, gio_ysize);

	window_set (gio_canvas,
	    WIN_WIDTH,	gio_xsize,
	    WIN_HEIGHT,	gio_ysize,
	    0);

	window_fit (gio_canvas);
	window_fit (gio_frame);

	Bpw_get_region_rect (gio_pw, &pw_rect);
	pw_rect.r_left = df_p->fb_xoff;
	pw_rect.r_top  = df_p->fb_yoff;

	sprintf (text, "Frame buffer configuration %d: %d %dx%d frame%c",
	    fb_config_index + 1, fb_nframes, Fb_width, Fb_height,
	    fb_nframes > 1 ? 's' : ' ');

	window_set (gio_frame, FRAME_LABEL, text, 0);
	panel_set_value (pan_set_maptype, df_p->fb_maptype);
	panel_set_value (pan_set_nframes, fb_nframes - 1);
}


/* LOAD_RASTER -- Load a rasterfile into the display.
 */
static
load_raster (o_pr, fname)
Pixrect	*o_pr;
char	*fname;
{
	FILE	*fp;
	Pixrect	*i_pr;
	int	width, height;

	if ((fp = fopen (fname, "r")) == NULL)
	    fprintf (stderr, "cannot open rasterfile %s\n", fname);
	else {
	    if (i_pr = pr_load (fp, NULL)) {
		width  = min (Fb_width,  i_pr->pr_width);
		height = min (Fb_height, i_pr->pr_height);
		pr_rop (o_pr, 0, 0, width, height, PIX_SRC, i_pr, 0, 0);
		pr_close (i_pr);
		repaint (P_IMAGE|P_COLORBAR|P_GRAPHICS);
	    }
	    fclose (fp);
	}
}


/* SAVE_RASTER -- Save a frame buffer in a file.
 */
static
save_raster (i_pr, fname)
Pixrect	*i_pr;
char	*fname;
{
	FILE	*fp;

	if ((fp = fopen (fname, "w")) == NULL)
	    fprintf (stderr, "cannot create rasterfile %s\n", fname);
	else {
	    if (pr_dump (i_pr, fp, RMT_NONE, RT_STANDARD, 0) == PIX_ERR)
		fprintf (stderr, "error writing rasterfile %s\n", fname);
	    fclose (fp);
	}
}


/* CREATE_GIO_CANVAS -- Set up the canvas for the main display window.
 */
static
create_gio_canvas (argc, argv)
int	argc;
char	**argv;
{
	register char	*argp;
	register int	arg;
	char	pathname[NGREY];
	struct	pixwin *pw;
	int	name;

	/* Override the builtin defaults with the values given by the user
	 * on the command line, if any.
	 */
	for (arg=1;  arg <= argc && (argp = argv[arg]) != NULL;  arg++) {
	    if (!strcmp (argp, "-Ws") || !strncmp (argp, "-size", 5)) {
		gio_xsize = atoi (argv[++arg]) - (TOOL_BORDERWIDTH * 2);
		gio_ysize = atoi (argv[++arg]) -
		    (tool_headerheight ((int) window_get (gio_frame,
		    FRAME_SHOW_LABEL)) + TOOL_BORDERWIDTH);
	    } else if (!strncmp (argp, "-maptype", 4)) {
		argp = argv[++arg];
		if (!strncmp (argp, "mono", 1))
		    maptype = MONO;
		else if (!strncmp (argp, "heat", 1))
		    maptype = HEAT;
		else if (!strncmp (argp, "ramp1", 1))
		    maptype = RAMP1;
		else if (!strncmp (argp, "ramp2", 1))
		    maptype = RAMP2;
		else if (!strncmp (argp, "halley", 1))
		    maptype = HALLEY;
		else if (!strncmp (argp, "linear", 1))
		    maptype = LINEARPS;
		else if (!strncmp (argp, "random", 1))
		    maptype = RANDOMPS;
		else if (!strncmp (argp, "crandom", 1))
		    maptype = CRANDOMPS;
		else if (!strncmp (argp, "ulut1", 1))
		    maptype = ULUT1;
		else if (!strncmp (argp, "ulut2", 1))
		    maptype = ULUT2;
		else
		    fprintf (stderr, "unknown arg `%s' to -maptype\n", argp);
	    } else if (!strncmp (argp, "-nocolorbar", 9)) {
		cb_height = 0;
		show_colorbar = 0;
	    } else if (!strncmp (argp, "-colorbar", 7)) {
		cb_height = SZ_COLORBAR;
		show_colorbar = 1;
	    } else if (!strncmp (argp, "-black", 3)) {
		background = CMS_BACKGROUND;
		fb_bkgcolor_index = BKG_BLACK;
	    } else if (!strncmp (argp, "-white", 3)) {
		background = 0;
		fb_bkgcolor_index = BKG_WHITE;

	    } else if (!strncmp (argp, "-rtype", 3)) {
		argp = argv[++arg];
		r_type = (argp[0] == 'r');
	    } else if (!strncmp (argp, "-rdispose", 3)) {
		argp = argv[++arg];
		strcpy (r_dispose, argp);
	    } else if (!strncmp (argp, "-rfilename", 3)) {
		argp = argv[++arg];
		strcpy (r_filename, argp);
	    } else if (!strncmp (argp, "-ulut1", 6)) {
		argp = argv[++arg];
		strcpy (u_lut1, argp);
	    } else if (!strncmp (argp, "-ulut2", 6)) {
		argp = argv[++arg];
		strcpy (u_lut2, argp);
	    }
	}

	/* Open display canvas.  The display canvas is never retained at the
	 * pixwin level since we can easily refresh it from the frame buffer.
	 */
	gio_canvas = window_create (gio_frame, CANVAS,
		WIN_WIDTH,	gio_xsize,
		WIN_HEIGHT,	gio_ysize,
		CANVAS_RETAINED, FALSE,
		CANVAS_AUTO_CLEAR, FALSE,
		CANVAS_REPAINT_PROC, refresh_display,
		0);
	if (gio_canvas == NULL)
	    exit (1);

	/* Initialize the frame and lut parameters and the canvax pixwin
	 * color map.
	 */
	pw = (struct pixwin *) window_get (gio_canvas, WIN_PIXWIN);
	fb_depth = pw->pw_pixrect->pr_depth;
	if (fb_depth < 8) {
	    fprintf (stderr, "imtool cannot be used on monochrome displays\n");
	    exit (1);
	}

	white = (1 << fb_depth) - 1;
	if ((fb_ngrey = CMS_DATARANGE) > (white + 1))
	    fb_ngrey = 1;
	init_colormap (pw);
	init_colormap (gio_pw = canvas_pixwin (gio_canvas));

	/* Set input event flags. */
	window_set (gio_canvas,
		WIN_CONSUME_PICK_EVENTS, WIN_NO_EVENTS,
		    WIN_MOUSE_BUTTONS, WIN_UP_EVENTS, LOC_DRAG, LOC_WINEXIT, 0,
		WIN_CONSUME_KBD_EVENTS, WIN_NO_EVENTS,
		    WIN_ASCII_EVENTS, WIN_LEFT_KEYS, WIN_RIGHT_KEYS,
		    KEY_SETUP, KEY_REMARK, KEY_SNAP, KEY_PCOORDS, KBD_DONE, 0,
		0);

	notify_set_event_func (ev_panner, ev_panner, NOTIFY_SAFE);
}


/* CREATE_SETUP_POPUP -- Create the popup menu used to set the terminal
 * setup options.
 */
static
create_setup_popup ()
{
	extern	loadframe_proc(), saveframe_proc(), fitframe_proc();
	extern	reset_proc(), iclear_proc(), gclear_proc();
	extern	setup_proc(), toggle_graphics(), set_background();
	extern	setframe_proc(), blinkenable_proc(), register_proc();
	static	Panel_setting set_ofname(), set_rfname();
	static	Panel_setting set_rtype(), set_rdispose(), set_rfilename();
	static	Panel_setting set_ulut1(), set_ulut2();
	static	Panel_setting set_blinklist(), set_zoomslist();
	static	panel_set_item();

	setup_frame = window_create (gio_frame, FRAME,
		FRAME_NO_CONFIRM,	TRUE,
		WIN_X,			setup_xoff,
		WIN_Y,			setup_yoff,
		0);
	if (setup_frame == NULL)
	    exit (1);
	setup_panel = window_create (setup_frame, PANEL, 0);
	if (setup_panel == NULL)
	    exit (1);

	panel_create_item (setup_panel, PANEL_MESSAGE,
		PANEL_ITEM_X,		ATTR_COL(10),
		PANEL_ITEM_Y,		ATTR_ROW(0),
		PANEL_LABEL_STRING,	"Image Display Setup and Control",
		0);

	pan_set_nframes = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(1) + 5,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Number of frame buffers:        ",
		PANEL_CHOICE_STRINGS,	FRAME_CHOICES, 0,
		PANEL_VALUE,		fb_nframes - 1,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_set_maptype = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(2) + 5,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Greyscale mapping:              ",
		PANEL_CHOICE_STRINGS,	"Mono",
					"ESO-Heat",
					"Ramp1",
					"Ramp2",
					"Halley",
					"Linear-pseudo",
					"Random-pseudo",
					"Crandom-pseudo",
					"User 1",
					"User 2",
					0,
		PANEL_VALUE,		maptype,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_set_ulut1 = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(3) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"User lookup table 1:            ",
		PANEL_VALUE,		u_lut1,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_ulut1,
		0);

	pan_set_ulut2 = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(4) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"User lookup table 2:            ",
		PANEL_VALUE,		u_lut2,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_ulut2,
		0);

	pan_globalcolor = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(5) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Globally manage colortable:     ",
		PANEL_CHOICE_STRINGS,	"No", "Yes", 0,
		PANEL_VALUE,		global_colortable,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_crandom_rate = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(6) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Rate (sec) for crandom option:  ",
		PANEL_CHOICE_STRINGS,	CRANDOM_CHOICES, 0,
		PANEL_VALUE,		crandom_blink_rate,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_set_background = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(7) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Background color:               ",
		PANEL_CHOICE_STRINGS,	"white", "black", 0,
		PANEL_VALUE,		fb_bkgcolor_index,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_snapframetoo = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(8) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Include frame border in imcopy: ",
		PANEL_CHOICE_STRINGS,	"No", "Yes", 0,
		PANEL_VALUE,		snap_frame_too,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_show_colorbar = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(9) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Show colorbar:                  ",
		PANEL_CHOICE_STRINGS,	"No", "Yes", 0,
		PANEL_VALUE,		show_colorbar,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_set_marker = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(10) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Cursor marker:                  ",
		PANEL_CHOICE_STRINGS,	"None", "Circle", "Cross", "Square", 0,
		PANEL_VALUE,		marktype,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_blink_rate = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(11) + 8,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Blink rate (sec):               ",
		PANEL_CHOICE_STRINGS,	BLINKRATE_CHOICES, 0,
		PANEL_VALUE,		blink_rate_index,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);

	pan_blink_list = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(12) + 11,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Frames to be blinked:           ",
		PANEL_VALUE,		s_blinklist,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_blinklist,
		0);

	pan_zoom_list = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(13) + 11,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Zoom factors:                   ",
		PANEL_VALUE,		s_zoomslist,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_zoomslist,
		0);

	strcpy (o_fname, COORDFILE);
	pan_set_ofname = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(14) + 11,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Coordinate list output file:    ",
		PANEL_VALUE,		o_fname,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_ofname,
		0);

	pan_set_rfname = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(15) + 11,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Rasterfile name (load/save):    ",
		PANEL_VALUE,		rasterfile,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_rfname,
		0);

	pan_set_rtype = panel_create_item (setup_panel, PANEL_CYCLE,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(16) + 13,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Screendump output type:         ",
		PANEL_CHOICE_STRINGS,	"postscript", "rasterfile", 0,
		PANEL_VALUE,		r_type,
		PANEL_NOTIFY_PROC,	panel_set_item,
		0);


	pan_set_rdispose = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(17) + 13,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Screendump dispose command:     ",
		PANEL_VALUE,		r_dispose,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_rdispose,
		0);

	pan_set_rfilename = panel_create_item (setup_panel, PANEL_TEXT,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(18) + 11,
		PANEL_DISPLAY_LEVEL,	PANEL_CURRENT,
		PANEL_LABEL_STRING,	"Screendump output file:         ",
		PANEL_VALUE,		r_filename,
		PANEL_VALUE_STORED_LENGTH, SZ_FNAME,
		PANEL_VALUE_DISPLAY_LENGTH, SZ_PANTEXT,
		PANEL_NOTIFY_PROC,	set_rfilename,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(19) + 15,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel,"Register Frames",0,0),
		PANEL_NOTIFY_PROC,	register_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Fit Window", 0,0),
		PANEL_NOTIFY_PROC,	fitframe_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Blink", 0,0),
		PANEL_NOTIFY_PROC,	blinkenable_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Frame", 0,0),
		PANEL_NOTIFY_PROC,	setframe_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_ITEM_X,		ATTR_COL(0),
		PANEL_ITEM_Y,		ATTR_ROW(20) + 15,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Reset", 0,0),
		PANEL_NOTIFY_PROC,	reset_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Iclear", 0,0),
		PANEL_NOTIFY_PROC,	iclear_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Gclear", 0,0),
		PANEL_NOTIFY_PROC,	gclear_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Load", 0,0),
		PANEL_NOTIFY_PROC,	loadframe_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "Save", 0,0),
		PANEL_NOTIFY_PROC,	saveframe_proc,
		0);

	panel_create_item (setup_panel, PANEL_BUTTON,
		PANEL_LABEL_IMAGE,
		    panel_button_image (setup_panel, "DISMISS", 0,0),
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
	if (item == pan_set_nframes) {
	    set_fbconfig (fb_config_index, value + 1);
	} else if (item == pan_set_maptype) {
	    df_p->fb_maptype = maptype = value;
	    set_colortable();
	    set_transfer_function (gio_pw, df_p->fb_center, df_p->fb_slope);
	} else if (item == pan_crandom_rate) {
	    crandom_blink_rate = value;
	} else if (item == pan_set_marker) {
	    marktype = value;
	} else if (item == pan_blink_rate) {
	    blink_rate_index = value;
	} else if (item == pan_set_rtype) {
	    r_type = value;
	} else if (item == pan_globalcolor) {
	    global_colortable = value;

	} else if (item == pan_set_background) {
	    /* Set the background color.
	     */
	    if (fb_bkgcolor_index != value) {
		register unsigned char *fb, *op;
		register int	n = Fb_height * Fb_width;
		fb = (unsigned char *) mpr_d(df_p->fb_pr)->md_image;

		switch (fb_bkgcolor_index = value) {
		case BKG_BLACK:
		    background = CMS_BACKGROUND;
		    for (op=fb;  --n >= 0;  op++)
			if (!*op)
			    *op = background;
		    break;
		case BKG_WHITE:
		    background = 0;
		    for (op=fb;  --n >= 0;  op++)
			if (*op == CMS_BACKGROUND)
			    *op = 0;
		    break;
		}
		gclear_proc();
	    }

	} else if (item == pan_show_colorbar) {
	    if (show_colorbar != value) {
		struct pixrect *pr = get_screen_rect();

		show_colorbar = value;
		if (show_colorbar) {
		    cb_height = SZ_COLORBAR;
		    gio_ysize += cb_height;
		    gio_ysize = min (Fb_height + cb_height, gio_ysize);
		} else {
		    cb_height = 0;
		    gio_ysize -= SZ_COLORBAR;
		}

		gio_xsize =
		    min (pr->pr_width - TOOL_BORDERWIDTH * 2, gio_xsize);
		gio_ysize =
		    min (pr->pr_height -
			tool_headerheight ((int) window_get (gio_frame,
			    FRAME_SHOW_LABEL)) - TOOL_BORDERWIDTH,
			gio_ysize);

		window_set (gio_canvas,
		    WIN_WIDTH,gio_xsize, WIN_HEIGHT,gio_ysize, 0);

		window_fit (gio_canvas);
		window_fit (gio_frame);
	    }

	} else if (item == pan_snapframetoo)
	    snap_frame_too = value;
}


/* SET_ZOOMSLIST -- Set the list of zoom factors.
 */
static Panel_setting
set_zoomslist (item, event)
Panel_item	item;
Event		*event;
{
	register char	*ip;
	register int	i;

	strcpy (s_zoomslist, (char *) panel_get_value (item));
	for (ip=s_zoomslist;  isspace (*ip);  ip++)
	    ;

	nzooms = 0;
	while (*ip && isdigit (*ip)) {
	    zooms[nzooms++] = max (1, atoi(ip));
	    while (isdigit (*ip))
		ip++;
	    while (isspace (*ip))
		ip++;
	    if (nzooms >= MAX_ZOOMS)
		break;
	}

	return (panel_text_notify (item,event));
}


/* SET_BLINKLIST -- Set the list of frames to be blinked.
 */
static Panel_setting
set_blinklist (item, event)
Panel_item	item;
Event		*event;
{
	register char	*ip;
	register int	i;

	strcpy (s_blinklist, (char *) panel_get_value (item));
	for (ip=s_blinklist;  isspace (*ip);  ip++)
	    ;

	n_blink_frames = 0;
	if (strncmp (ip, "all", 3) == 0) {
	    n_blink_frames = MAX_FRAMES;
	    for (i=0;  i < MAX_FRAMES;  i++)
		blink_frames[i] = i + 1;
	} else {
	    while (*ip && isdigit (*ip)) {
		blink_frames[n_blink_frames++] = atoi(ip);
		while (isdigit (*ip))
		    ip++;
		while (isspace (*ip))
		    ip++;
	    }
	}

	return (panel_text_notify (item,event));
}


/* BLINKENABLE_PROC -- Turn frame blink on or off.
 */
static
blinkenable_proc()
{
	blink = !blink;
	menu_set (blink_item, MENU_STRING, blink ? "Blink off" : "Blink on", 0);
	blink_frame = n_blink_frames - 1;
	blink_timer = 0;
}


/* REGISTER_PROC -- Register all frames with the current frame.
 */
static
register_proc()
{
	register struct framebuf *fr_p;
	register int	i;

	for (i=0, fr_p=frames;  i < fb_nframes;  i++, fr_p++)
	    if (fr_p != df_p)
		set_zoom (fr_p, df_p->fb_xoff, df_p->fb_yoff, df_p->fb_xzoom);
}


/* SET_OFNAME -- Set the file name for coordinate output.
 */
static Panel_setting
set_ofname (item, event)
Panel_item	item;
Event		*event;
{
	char	*s;

	s = (char *) panel_get_value (item);
	if (strcmp (s, o_fname)) {
	    strcpy (o_fname, s);
	    df_p->fb_imageno = 0;
	    df_p->fb_objno = 1;
	    window_set (gio_frame, FRAME_LABEL, framelabel(), 0);
	}

	return (panel_text_notify (item,event));
}


/* SET_ULUT1 -- Set the file name for user lookup table 1.
 */
static Panel_setting
set_ulut1 (item, event)
Panel_item	item;
Event		*event;
{
	char	*s;

	strcpy (u_lut1, (char *) panel_get_value (item));
	return (panel_text_notify (item,event));
}


/* SET_ULUT2 -- Set the file name for user lookup table 2.
 */
static Panel_setting
set_ulut2 (item, event)
Panel_item	item;
Event		*event;
{
	char	*s;

	strcpy (u_lut2, (char *) panel_get_value (item));
	return (panel_text_notify (item,event));
}


/* SET_RFNAME -- Set the file name for rasterfile load/save.
 */
static Panel_setting
set_rfname (item, event)
Panel_item	item;
Event		*event;
{
	char	*s;

	strcpy (rasterfile, (char *) panel_get_value (item));
	return (panel_text_notify (item,event));
}


/* SET_RDISPOSE -- Set the raster file dispose command.
 */
static Panel_setting
set_rdispose (item, event)
Panel_item	item;
Event		*event;
{
	char	*s;

	strcpy (r_dispose, (char *) panel_get_value (item));
	return (panel_text_notify (item,event));
}


/* SET_RFILENAME -- Set the screendump filename template.
 */
static Panel_setting
set_rfilename (item, event)
Panel_item	item;
Event		*event;
{
	char	*s;

	strcpy (r_filename, (char *) panel_get_value (item));
	return (panel_text_notify (item,event));
}


/* LOADFRAME_PROC -- Load the named rasterfile into the current frame.
 */
static
loadframe_proc()
{
	char	fname[SZ_FNAME+1];
	char	buf[SZ_FNAME+1];

	sprintf (buf, rasterfile, df_p->fb_frameno, df_p->fb_imageno);
	strcpy (fname, getfname(buf, 0));
	load_raster (df_p->fb_pr, fname);
}


/* SAVEFRAME_PROC -- Save the current frame in the named rasterfile.
 */
static
saveframe_proc()
{
	char	fname[SZ_FNAME+1];
	char	buf[SZ_FNAME+1];

	sprintf (buf, rasterfile, df_p->fb_frameno, df_p->fb_imageno);
	strcpy (fname, getfname(buf, 0));
	save_raster (df_p->fb_pr, fname);
}


/* FITFRAME_PROC -- Fit the display window to the current frame buffer size.
 */
static
fitframe_proc()
{
	register struct pixrect *pr = get_screen_rect();

	gio_xsize = min (pr->pr_width - TOOL_BORDERWIDTH * 2, Fb_width);
	gio_ysize = min (pr->pr_height
	    - tool_headerheight ((int)window_get(gio_frame,FRAME_SHOW_LABEL))
	    - TOOL_BORDERWIDTH,
	    Fb_height + cb_height);

	window_set (gio_canvas,
	    WIN_WIDTH,	gio_xsize,
	    WIN_HEIGHT,	gio_ysize,
	    0);

	window_fit (gio_canvas);
	window_fit (gio_frame);
}


/* GET_SCREEN_RECT -- Determine the size of the workstation screen.
 */
static struct pixrect *
get_screen_rect()
{
	static	struct pixrect screen;
	struct	pixrect *pr;

	if (!screen.pr_width)
	    if (pr = pr_open ("/dev/fb")) {
		screen = *pr;
		pr_close (pr);
	    } else {
		screen.pr_width = 1152;
		screen.pr_width = 900;
	    }

	return (&screen);
}


/* SETUP_PROC -- Toggle whether or not the setup panel is shown.
 */
static
setup_proc()
{
	if ((int) window_get (setup_frame, WIN_SHOW) == TRUE) {
	    setup_xoff = (int) window_get (setup_frame, WIN_X, 0);
	    setup_yoff = (int) window_get (setup_frame, WIN_Y, 0);
	    window_set (setup_frame, WIN_SHOW, FALSE, 0);
	} else {
	    window_set (setup_frame,
		WIN_X,	setup_xoff,
		WIN_Y,	setup_yoff,
		0);
	    window_set (setup_frame, WIN_SHOW, TRUE, 0);
	}
}


/* CREATE_FRAME_MENU -- Imtool uses a special frame menu which provides the
 * standard frame menu as a submenu.
 */
static
create_frame_menu (frame)
Frame	frame;
{
	extern	imagecopy_proc(), register_proc();
	extern	setup_proc(), setframe_proc(), fitframe_proc();
	extern	iclear_proc(), gclear_proc(), blinkenable_proc();
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
		"Register",
		register_proc,
	    MENU_ACTION_ITEM,
		"Blink on",
		blinkenable_proc,
	    MENU_ACTION_ITEM,
		"FitFrame",
		fitframe_proc,
	    MENU_ACTION_ITEM,
		"NextFrame",
		setframe_proc,
	    MENU_ACTION_ITEM,
		"Gclear",
		gclear_proc,
	    MENU_ACTION_ITEM,
		"Iclear",
		iclear_proc,
	    MENU_ACTION_ITEM,
		"Imcopy",
		imagecopy_proc,
	    0);

	blink_item = menu_find (new_menu, MENU_STRING, "Blink on", 0);

	/* Install the new menu. */
	window_set (frame, WIN_MENU, new_menu, 0);
}


/* INIT_FRAMEBUFFERS -- Allocate space for the indicated number and size of
 * framebuffers, initializing the framebuffer data structures accordingly.
 * If at least one frame buffer of the desired size can be allocated we
 * consider it a success, but set fb_nframes accordingly.  -1 is returned
 * if no frames of the desired size can be allocated.
 */
static
init_framebuffers (config, nframes)
int	config;			/* new frame buffer configuration */
int	nframes;		/* desired number of frames	  */
{
	register struct	framebuf *fb;
	register int	i;
	char	*calloc();

	nframes = min (MAX_FRAMES, nframes);

	/* If we are only changing the number of frames in the current
	 * configuration, keep the old frames.
	 */
	if (frames && config == fb_config_index) {
	    if (nframes == fb_config[config].nframes) {
		fb_nframes = nframes;
		return (nframes);

	    } else if (nframes < fb_config[config].nframes) {
		for (i=nframes;  i < fb_nframes;  i++)
		    pr_close (frames[i].fb_pr);
		fb_config[config].nframes = fb_nframes = nframes;
		if (reference_frame > nframes) {
		    reference_frame = nframes;
		    rf_p = frames + (reference_frame - 1);
		}
		if (display_frame > nframes)
		    set_frame (nframes);
		return (nframes);

	    } else if (nframes > fb_config[config].nframes)
		; /* fall through and add more frames */

	} else {
	    /* Deallocate old frame buffers, if any. */
	    if (frames) {
		pr_close (cb_pr);
		for (i=0;  i < fb_nframes;  i++)
		    pr_close (frames[i].fb_pr);
		free (frames);
	    }

	    fb_nframes = 0;
	    frames = (struct framebuf *) calloc (MAX_FRAMES,
		sizeof(struct framebuf));

	    df_p = rf_p = frames;
	    display_frame = reference_frame = 1;
	    strcpy (o_fname, COORDFILE);
	    wc_xoff = wc_yoff = 0;
	}

	/* Allocate and initialize the new frame buffers. */
	fb_config_index = config;
	Fb_width  = fb_config[config].width;
	Fb_height = fb_config[config].height;

	for (i=fb_nframes;  i < nframes;  i++) {
	    fb = frames + i;
	    fb->fb_pr = mem_create (Fb_width, Fb_height, fb_depth);
	    if (fb->fb_pr == NULL) {
		fprintf (stderr,
		    "attempt to allocate frame buffer %d (%dx%d) fails\n",
		    i + 1, Fb_width, Fb_height);
		if (i)
		    break;
		else
		    return (-1);
	    } else if (!fb_nframes++) {
		cb_pr = mem_create (Fb_width, cb_height, fb_depth);
		if (pw_rect.r_width)
		    init_colorbar (pw_rect.r_width);
	    }

	    fb->fb_xoff = 0;
	    fb->fb_yoff = 0;
	    fb->fb_xzoom = fb->fb_yzoom = 1;
	    fb->fb_center = fb_ngrey / 2.0;
	    fb->fb_slope = (float)white / (float)(fb_ngrey - 1);
	    fb->fb_maptype = maptype;
	    fb->fb_objno = 1;
	    fb->fb_imageno = 0;
	    fb->fb_frameno = i + 1;
	    fb->fb_ctran.valid = 0;
	    fb->fb_ctran.imtitle[0] = '\0';
	    strcpy (fb->fb_ctran.format, W_DEFFORMAT);
	}

	set_zoom (df_p, 0, 0, 1);
	return (fb_config[config].nframes = fb_nframes);
}


/* INIT_COLORBAR -- Write the colorbar into the frame buffer.  The length of
 * the colorbar should correspond to the current width of the display window,
 * but must not exceed the width of the frame buffer.
 */
init_colorbar (cb_width)
int	cb_width;
{
	register int	i;
	unsigned char *lp;
	Pixrect *pr;

	if (cb_height <= 0)
	    return;

	if ((pr = mem_create (cb_width, 1, fb_depth)) != NULL) {
	    lp = (unsigned char *) mpr_d(pr)->md_image;
	    /* Write colorbar. */
	    for (i=0;  i < cb_width;  i++)
		lp[i] = (CMS_DATARANGE-1) * i / (cb_width-1) +
		    CMS_DATASTART;
	    for (i=3;  i < cb_height;  i++)
		pr_rop (cb_pr, 0, i, cb_width, 1, PIX_SRC, pr, 0, 0);

	    /* Add a border between image and colorbar. */
	    for (i=0;  i < cb_width;  i++)
		lp[i] = NGREY-1;
	    for (i=0;  i < 3;  i++)
		pr_rop (cb_pr, 0, i, cb_width, 1, PIX_SRC, pr, 0, 0);

	    for (i=0;  i < cb_width;  i++)
		lp[i] = 0;
	    for (i=1;  i < 2;  i++)
		pr_rop (cb_pr, 0, i, cb_width, 1, PIX_SRC, pr, 0, 0);

	    pr_close (pr);
	}
}


/* INIT_COLORMAP -- Initialize the IMTOOL color map.
 */
static
init_colormap (pw)
struct	pixwin *pw;
{
	register unsigned char *r = red;
	register unsigned char *g = green;
	register unsigned char *b = blue;
	int	planes = NGREY-1, i;

	/* Initialize the IMTOOL colormap from the current fullscreen
	 * colormap, so that the (small) colormap entries of the other
	 * windows will be preserved, as far as possible.
	 */
	grab_colormap (r, g, b);

	/* Set a linear transfer function for the main part of the table. */
	for (i=CMS_DATASTART;  i <= CMS_DATAEND;  i++)
	    r[i] = g[i] = b[i] = i * (white + 1) / CMS_DATARANGE;

	/* Color table entry for the cursor. */
	r[CMS_CURSOR] = white;
	g[CMS_CURSOR] = white;
	b[CMS_CURSOR] = white;

	/* Set the background and graphics colors.
	 */
	i = CMS_BACKGROUND;
	r[i] =   0;  g[i] =   0;  b[i] =   0;	i++;	/* 202=black */

	i = CMS_GRAPHICSSTART;
	r[i] = 255;  g[i] = 255;  b[i] = 255;	i++;	/* 203=white */

	r[i] = 255;  g[i] =   0;  b[i] =   0;	i++;	/* 204=red */
	r[i] =   0;  g[i] = 255;  b[i] =   0;	i++;	/* 205=green */
	r[i] =   0;  g[i] =   0;  b[i] = 255;	i++;	/* 206=blue */
	r[i] = 255;  g[i] = 255;  b[i] =   0;	i++;	/* 207=yellow */
	r[i] =   0;  g[i] = 255;  b[i] = 255;	i++;	/* 208=cyan */
	r[i] = 255;  g[i] =   0;  b[i] = 255;	i++;	/* 209=magenta */
	r[i] = 255;  g[i] = 127;  b[i] =   0;	i++;	/* 210=coral */
	r[i] = 142;  g[i] =  35;  b[i] = 107;	i++;	/* 211=maroon */
	r[i] = 204;  g[i] =  50;  b[i] =  50;	i++;	/* 212=orange */
	r[i] = 159;  g[i] = 159;  b[i] =  95;	i++;	/* 213=khaki */
	r[i] = 219;  g[i] = 112;  b[i] = 219;	i++;	/* 214=orchid */
	r[i] = 112;  g[i] = 219;  b[i] = 219;	i++;	/* 215=turquoise */
	r[i] = 159;  g[i] =  95;  b[i] = 159;	i++;	/* 216=violet */
	r[i] = 216;  g[i] = 216;  b[i] = 191;	i++;	/* 217=wheat */

	pw_setcmsname (pw, CMMAPNAME);
	pw_putcolormap (pw, 0, NGREY, r, g, b);
	pw_putattributes (pw, &planes);
}


/* COMPUTE_TRANSFER_FUNCTION -- Compute the slope and offset of the transfer
 * function for the current display frame, given the coordinates of the
 * mouse in the frame.
 */
static
compute_transfer_function (event)
Event	*event;
{
	float	xsize, ysize;
	float	y, slope;
	int	neg;

	xsize = pw_rect.r_width;
	ysize = pw_rect.r_height;

	/* Compute the slope of the transfer function. */
	y = event_y(event) / ysize * 2.0 - 1.0;
	if (neg = (y < 0.0))
	    y = -y;
	if (y > 0.99)
	    y = 0.99;
	if ((slope = tan (y * M_PI_2)) > white)
	    slope = white;

	/* Record new transfer function in frame buffer descriptor. */
	df_p->fb_center = event_x(event) / xsize * (fb_ngrey-1);
	df_p->fb_slope  = neg ? -slope : slope;
}


/* SET_TRANSFER_FUNCTION -- Load the color map as necessary to implement the
 * given linear transfer function.
 */
static
set_transfer_function (pw, center, slope)
struct	pixwin *pw;	/* reference pixwin */
float	center;		/* greyscale value at half intensity	*/
float	slope;		/* delta-intensity per greyscale unit	*/
{
	register int	i;
	register float	z, zmin, zmax;
	unsigned char	o_red[NGREY], o_green[NGREY], o_blue[NGREY];
	unsigned char	*p_r, *p_g, *p_b;

	if (center < CMS_DATASTART)
	    center = CMS_DATASTART;
	else if (center > CMS_DATAEND)
	    center = CMS_DATAEND;

	zmin = 0.0;
	zmax = white;

	z = white / 2;
	for (i=center;  i <= CMS_DATAEND;  i++) {
	    o_red[i] = m_red[(unsigned char)(int)z];
	    o_green[i] = m_green[(unsigned char)(int)z];
	    o_blue[i] = m_blue[(unsigned char)(int)z];
	    z += slope;
	    if (z <= zmin)
		z = zmin;
	    else if (z >= zmax)
		z = zmax;
	}

	z = white / 2;
	for (i=center;  i >= CMS_DATASTART;  i--) {
	    o_red[i] = m_red[(unsigned char)(int)z];
	    o_green[i] = m_green[(unsigned char)(int)z];
	    o_blue[i] = m_blue[(unsigned char)(int)z];
	    z -= slope;
	    if (z <= zmin)
		z = zmin;
	    else if (z >= zmax)
		z = zmax;
	}

	p_r = &o_red[CMS_DATASTART];
	p_g = &o_green[CMS_DATASTART];
	p_b = &o_blue[CMS_DATASTART];

	/*pw_putcolormap (pw, CMS_DATASTART, CMS_DATARANGE, p_r, p_g, p_b);*/

	bcopy (p_r, &red[CMS_DATASTART], CMS_DATARANGE);
	bcopy (p_g, &green[CMS_DATASTART], CMS_DATARANGE);
	bcopy (p_b, &blue[CMS_DATASTART], CMS_DATARANGE);

	/* Reset the full 8 bit colormap for the pixwin, to pick up any
	 * changes made in the unused regions for other windows, picked up
	 * by edit_colortable below.
	 */
	pw_putcolormap (pw, 0, NGREY, red, green, blue);
}


/* SET_COLORTABLE -- Set up the RGB lookup tables used to map the windowed
 * monochrome output of a frame buffer into the hardware colormap.
 */
static Notify_value
set_colortable()
{
	register int	v, vsat, step, i;
	static	int seed = 0;
	int	knot[7];

	vsat = NGREY - 1;
	step = NGREY / 6;
	for (i=0;  i < 7;  i++)
	    knot[i] = i * step;
	knot[6] = vsat;

	switch (df_p->fb_maptype) {
	case MONO:
	    for (i=0;  i < NGREY;  i++)
		m_red[i] = m_green[i] = m_blue[i] = i;
	    break;

	case HEAT:
	    for (i=0;  i < NGREY;  i++) {
		m_red[i]   = heat.hue[i].red   * (NGREY - 1);
		m_green[i] = heat.hue[i].green * (NGREY - 1);
		m_blue[i]  = heat.hue[i].blue  * (NGREY - 1);
	    }
	    break;

	case RAMP1:
	    for (i=0;  i < NGREY;  i++) {
		m_red[i]   = heat.hue[i].red   * NGREY;
		m_green[i] = heat.hue[i].green * NGREY;
		m_blue[i]  = heat.hue[i].blue  * NGREY;
	    }
	    break;

	case RAMP2:
	    for (i=0;  i < NGREY;  i++) {
		m_red[i]   = heat.hue[i].red   * ((NGREY - 1) * 2);
		m_green[i] = heat.hue[i].green * ((NGREY - 1) * 2);
		m_blue[i]  = heat.hue[i].blue  * ((NGREY - 1) * 2);
	    }
	    break;

	case HALLEY:
	    for (i=0;  i < NGREY;  i++) {
		m_red[i]   = halley.hue[i].red   * (NGREY - 1);
		m_green[i] = halley.hue[i].green * (NGREY - 1);
		m_blue[i]  = halley.hue[i].blue  * (NGREY - 1);
	    }
	    break;

	case ULUT1:
	case ULUT2:
	    {   struct lut user;
		struct triplet *p;
		int i, j;
		FILE *fp;
		char *s;

		s = (df_p->fb_maptype == ULUT1) ? u_lut1 : u_lut2;
		if ((fp = fopen (getfname(s,0), "r")) == NULL) {
		    fprintf (stderr, "cannot open %s\n", s);
		    return;
		}

		for (user.lutlen=0;  user.lutlen < NGREY;  user.lutlen++) {
		    p = &user.hue[user.lutlen];
		    if (fscanf (fp, " %f %f %f",
			&p->red, &p->green, &p->blue) == EOF)
			break;
		}

		for (i=0;  i < NGREY;  i++) {
		    j = max(0, min(NGREY-1, (i * user.lutlen / NGREY)));
		    m_red[i]   = user.hue[j].red   * (NGREY - 1);
		    m_green[i] = user.hue[j].green * (NGREY - 1);
		    m_blue[i]  = user.hue[j].blue  * (NGREY - 1);
		}

		fclose (fp);
	    }
	    break;

	case LINEARPS:
	    for (i=0;  i < NGREY;  i++)
		m_red[i] = m_green[i] = m_blue[i] = 0;

	    for (i=knot[0];  i <= knot[1];  i++)
		m_blue[i] = vsat * (i - knot[0]) / step;
	    for (i=knot[1];  i <= knot[2];  i++)
		m_blue[i] = vsat;
	    for (i=knot[2];  i <= knot[3];  i++)
		m_blue[i] = vsat * (knot[3] - i) / step;

	    for (i=knot[1];  i <= knot[2];  i++)
		m_green[i] = vsat * (i - knot[1]) / step;
	    for (i=knot[2];  i <= knot[4];  i++)
		m_green[i] = vsat;
	    for (i=knot[4];  i <= knot[5];  i++)
		m_green[i] = vsat * (knot[5] - i) / step;

	    for (i=knot[3];  i <= knot[4];  i++)
		m_red[i] = vsat * (i - knot[3]) / step;
	    for (i=knot[4];  i <= knot[6];  i++)
		m_red[i] = vsat;

	    for (i=knot[5];  i <= knot[6];  i++) {
		if ((v = vsat * (i - knot[5]) / step) > vsat)
		    v = vsat;
		m_green[i] = m_blue[i] = v;
	    }
	    break;

	case CRANDOMPS:
	    set_transfer_function (gio_pw, df_p->fb_center, df_p->fb_slope);
	    imt_pause (cr_msec[crandom_blink_rate], set_colortable);
	    /* fall through */

	case RANDOMPS:
	    if (!seed)
		seed = time(0);
	    srand (seed++);
	    for (i=0;  i < NGREY;  i++) {
		m_red[i]   = ((rand() >> 4) % NGREY);
		m_green[i] = ((rand() >> 4) % NGREY);
		m_blue[i]  = ((rand() >> 4) % NGREY);
	    }
	    break;
	}
}


/* GRAB_COLORMAP -- Read the current physical full screen colormap.
 */
static
grab_colormap (red, green, blue)
unsigned char red[], green[], blue[];
{
	struct	pixrect *screen;

	if (window_open) {
	    screen = pr_open ("/dev/fb");
	    pr_getcolormap (screen, 0, NGREY, red, green, blue);
	    pr_close (screen);
	}
}


/* IMTOOL_CLOCK -- The main imtool clock.
 */
static
imtool_clock (client, event, arg)
Notify_client client;
Notify_event event;
Notify_arg arg;
{
	static	int delay=0, interval=0, toggle=0;
	Cursor	cursor;
	int	frame, n;

	/* Blink cursor (variable rate). */
	cursor = window_get (gio_canvas, WIN_CURSOR);
	toggle = !toggle;
	if (reading_imcursor) {
	    cursor_set (cursor,
		CURSOR_OP, toggle
		    ? PIX_NOT(PIX_SRC) & PIX_DST
		    : PIX_SRC | PIX_DST | PIX_COLOR(NGREY-1),
		0);
	} else {
	    cursor_set (cursor,
		CURSOR_CROSSHAIR_COLOR,
		    toggle ? CMS_BACKGROUND : CMS_CURSOR,
		0);
	}
	window_set (gio_canvas, WIN_CURSOR, cursor, 0);

	/* Things that happen at a fixed interval. */
	if ((delay += interval) >= CLOCK_INTERVAL) {
	    /* Keep imtool visible (if window open). */
	    edit_colormap();

	    /* Frame blink.  The frames in the blink frame list do not have to
	     * exist; if not, then advance through the list until either a valid
	     * frame is found, or the list has been traversed once.
	     */
	    if (blink && n_blink_frames > 0 && state != WINDOW && window_open) {
		if (--blink_timer <= 0) {
		    for (n=n_blink_frames;  --n >= 0;  ) {
			if (++blink_frame >= n_blink_frames)
			    blink_frame = 0;
			frame = blink_frames[blink_frame];
			if (frame >= 1 && frame <= fb_nframes) {
			    set_frame (frame);
			    if (display_coords && state == TRACK_CURSOR)
				update_coords (NULL);
			    break;
			}
		    }
		    blink_timer = blink_rate[blink_rate_index];
		}
	    }

	    delay = 0;
	}

	interval = reading_imcursor ? CLOCK_INTERVAL/4 : CLOCK_INTERVAL;
	imt_pause (interval, imtool_clock);
}


/* EDIT_COLORMAP -- Overwrite the portion of the full screen colormap used
 * by the display server.  This must be done in such a way that changes to
 * the region of the screen colortable used by other windows are preserved.
 */
static
edit_colormap()
{
	struct	pixrect *screen;
	unsigned char r[NGREY], g[NGREY], b[NGREY];

	if (state != WINDOW && window_open) {
	    /* Edit the physical colortable. */
	    screen = pr_open ("/dev/fb");
	    pr_getcolormap (screen, 0, NGREY, r, g, b);

	    /* Make the cursor blink between black and white for better vis.
	     * (>> now done with set_cursor in imtool_clock())
	    if (green[CMS_CURSOR] == white) {
		red[CMS_CURSOR]   = black;
		green[CMS_CURSOR] = black;
		blue[CMS_CURSOR]  = black;
	    } else {
		red[CMS_CURSOR]   = white;
		green[CMS_CURSOR] = white;
		blue[CMS_CURSOR]  = white;
	    }
	     */

	    bcopy (&red[CMS_FIRST], &r[CMS_FIRST], CMS_NGREY);
	    bcopy (&green[CMS_FIRST], &g[CMS_FIRST], CMS_NGREY);
	    bcopy (&blue[CMS_FIRST], &b[CMS_FIRST], CMS_NGREY);

	    pw_putcolormap (gio_pw, 0, NGREY, r, g, b);
	    if (global_colortable)
		pr_putcolormap (screen, 0, NGREY, r, g, b);
	    pr_close (screen);

	    /* Update the canvas pixwin colortable. */
	    bcopy (r, red, NGREY);
	    bcopy (g, green, NGREY);
	    bcopy (b, blue, NGREY);
	}
}


/* SHOW_COLORMAP -- Print the contents of the color map for a pixwin.
 */
static
show_colormap (pw, first, last)
struct	pixwin *pw;
int	first, last;
{
	unsigned char r[NGREY], g[NGREY], b[NGREY];
	char	cmsname[CMS_NAMESIZE];
	int	i, n;

	pw_getcmsname (pw, cmsname);
	pw_getcolormap (pw, 0, NGREY, r, g, b);

	printf ("color map segment = '%s'\n", cmsname);
	for (i=first, n=0;  i <= last;  i++) {
	    printf ("%3d %3d %3d", r[i], g[i], b[i]);
	    printf ((++n % 5) ? "  " : "\n");
	}
	printf ("\n");
}


/* LOAD_TESTPATTERN -- Load a test pattern into the reference frame buffer.
 */
static
load_testpattern (type)
int	type;			/* pattern type desired */
{
 	register unsigned char	*line;
	register int	i, j, color;
 	unsigned char	*fb, *oline;

	fb = (unsigned char *) mpr_d(df_p->fb_pr)->md_image;

	switch (type) {
	case 0:
	    /* Compute first line. */
	    oline = line = fb;
	    for (i=0, color=0;  i < Fb_width;  i++)
		if (((i+16) % 32) == 0) {
		    line[i] = 0;
		    color = ((i+j) % CMS_DATARANGE) / 32 * 32;
		} else
		    line[i] = color;

	    /* Compute remaining lines. */
	    for (j=1;  j < Fb_height;  j++) {
		line = fb + j * Fb_width;
		if (((j+16) % 32) == 0) {
		    for (i=0;  i < Fb_width;  i++)
			line[i] = 0;
		    if (++j >= Fb_height)
			break;
		    line = fb + j * Fb_width;
		    color = (j % CMS_DATARANGE) / 32 * 32;
		    for (i=0;  i < Fb_width;  i++)
			if (((i+16) % 32) == 0) {
			    line[i] = 0;
			    color = ((i+j) % CMS_DATARANGE) / 32 * 32;
			} else
			    line[i] = color;
		    oline = line;
		} else
		    bcopy (oline, line, Fb_width);
	    }
	    break;

	case 1:
	    for (j=1;  j < Fb_height;  j++) {
		line = fb + j * Fb_width;
		for (i=0;  i < Fb_width;  i++)
		    line[i] = (i % CMS_DATARANGE);
	    }
	    break;

	case 2:
	    for (j=1;  j < Fb_height;  j++) {
		line = fb + j * Fb_width;
		for (i=0;  i < Fb_width;  i++)
		    line[i] = ((i+j) % CMS_DATARANGE);
	    }
	    break;
	}
}


/* For the moment we take an IIS model 70 command/data stream as input; this
 * is used to load images into the image display.  This is a kludge interface
 * for the prototype, convenient since the high level software is written for
 * the IIS.
 */
#define	MEMORY		01		/* frame buffer i/o		*/
#define	LUT		02		/* lut i/o			*/
#define	FEEDBACK	05		/* used for frame clears	*/
#define	IMCURSOR	020		/* logical image cursor		*/
#define	WCS		021		/* used to set WCS		*/

#define	SZ_IMCURVAL	160
#define	PACKED		0040000
#define	COMMAND		0100000
#define	IIS_READ	0100000
#define	IMC_SAMPLE	0040000
#define	IMT_FBCONFIG	077
#define	XYMASK		077777

struct	iism70 {
	short	tid;
	short	thingct;
	short	subunit;
	short	checksum;
	short	x, y, z;
	short	t;
};

/* EV_CMDINPUT -- Called when command or data input has arrived via the
 * pseudodevice input stream from some applications process.
 */
static Notify_value
ev_cmdinput (frame, event, arg, type)
Frame	frame;
Event	*event;
Notify_arg arg;
Notify_event_type type;
{
	register unsigned char *cp;
	register int	sum, i;
	register short	*p;
	int	ndatabytes, nbytes, n, ntrys=0;
	static	int errmsg=0, bswap=0;
	struct	iism70 iis;
	char	buf[SZ_FIFOBUF];
	int	fb_index;

	/* Get the IIS header. */
	if (read (datain, (char *)&iis, sizeof(iis)) < sizeof(iis)) {
	    fprintf (stderr, "imtool: command input read error\n");
	    return (NOTIFY_DONE);
	} else if (bswap)
	    bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));

	/* Verify the checksum.  If it fails swap the bytes and try again.
	 */
	for (;;) {
	    for (i=0, sum=0, p=(short *)&iis;  i < 8;  i++)
		sum += *p++;
	    if ((sum & 0177777) == 0177777)
		break;

	    if (ntrys++) {
		if (!errmsg++) {
		    fprintf (stderr, "imtool: bad data header checksum\n");
		    if (bswap)
			bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		    fprintf (stderr, "noswap:");
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");

		    bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		    fprintf (stderr, "  swap:");
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");
		}
		break;

	    } else {
		bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		bswap = !bswap;
	    }
	}

	ndatabytes = -iis.thingct;
	if (!(iis.tid & PACKED))
	    ndatabytes *= 2;

	switch (iis.subunit & 077) {
	case FEEDBACK:
	    /* The feedback unit is used only to clear a frame.
	     */
	    set_reference_frame (decode_frameno (iis.z & 07777));
	    erase (rf_p);
	    break;

	case LUT:
	    /* Data mode writes to the frame lookup tables are not implemented.
	     * A command mode write to the LUT subunit is used to connect
	     * image memories up to the RGB channels, i.e., to select the frame
	     * to be displayed.  We ignore any attempt to assign multiple
	     * frames to multiple color channels, and just do a simple frame
	     * select.
	     */
	    if (iis.subunit & COMMAND) {
		int	frame, z, n;
		short	x[14];

		if (read (datain, (char *)x, ndatabytes) == ndatabytes) {
		    if (bswap)
			bswap2 ((char *)x, (char *)x, ndatabytes);

		    z = x[0];
		    if (!z) z = 1;
		    for (n=0;  !(z & 1);  z >>= 1)
			n++;

		    frame = max (1, n + 1);
		    if (frame > fb_nframes) {
			if (frame < MAX_FRAMES)
			    set_fbconfig (fb_config_index, frame);
			else {
			    fprintf (stderr, "imtool warning: ");
			    fprintf (stderr, 
			    "attempt to display nonexistent frame %d\n", frame);
			    frame = fb_nframes - 1;
			}
		    }

		    set_frame (frame);
		    return (NOTIFY_DONE);
		}
	    }

	case MEMORY:
	    /* Load data into the frame buffer.  Data is assumed to be byte
	     * packed.
	     */
	    if (iis.tid & IIS_READ) {
		/* Read from the display.
		 */
		unsigned char *fb, *ip;
		int     nbytes, nleft, n, x, y;
		long    starttime;

		/* Get the frame to be read from. */
		set_reference_frame (decode_frameno (iis.z & 07777));

		fb = (unsigned char *) mpr_d(rf_p->fb_pr)->md_image;
		nbytes = ndatabytes;
		x = iis.x & XYMASK;
		y = iis.y & XYMASK;

		ip = max (fb, min (fb + Fb_width * Fb_height - nbytes,
		    fb + y * Fb_width + x));
		if (ip != fb + y * Fb_width + x) {
		    fprintf (stderr,
			"imtool: attempted read out of bounds on framebuf\n");
		    fprintf (stderr,
			"read %d bytes at [%d,%d]\n", nbytes, x, y);
		}

		/* Return the data from the frame buffer. */
		starttime = time(0);
		for (nleft = nbytes;  nleft > 0;  nleft -= n) {
		    n = (nleft < SZ_FIFOBUF) ? nleft : SZ_FIFOBUF;
		    if ((n = write (dataout, ip, n)) <= 0) {
			if (n < 0 || (time(0) - starttime > IO_TIMEOUT)) {
			    fprintf (stderr, "IMTOOL: timeout on write\n");
			    break;
			}
		    } else
			ip += n;
		}

		return (NOTIFY_DONE);

	    } else {
		/* Write to the display.
		 */
		unsigned char *fb, *op;
		int     nbytes, nleft, n, x, y;
		long    starttime;

		/* Get the frame to be written into (encoded with a bit for
		 * each frame, 01 is frame 1, 02 is frame 2, 04 is frame 3,
		 * and so on).
		 */
		set_reference_frame (decode_frameno (iis.z & 07777));

		/* Get a pointer into the frame buffer where the data will
		 * be put.
		 */
		fb = (unsigned char *) mpr_d(rf_p->fb_pr)->md_image;
		nbytes = ndatabytes;
		x = iis.x & XYMASK;
		y = iis.y & XYMASK;

		op = max (fb, min (fb + Fb_width * Fb_height - nbytes,
		    fb + y * Fb_width + x));
		if (op != fb + y * Fb_width + x) {
		    fprintf (stderr,
			"imtool: attempted write out of bounds on framebuf\n");
		    fprintf (stderr,
			"write %d bytes to [%d,%d]\n", nbytes, x, y);
		}

		/* Read the data into the frame buffer.
		 */
		starttime = time(0);
		for (nleft = nbytes;  nleft > 0;  nleft -= n) {
		    n = (nleft < SZ_FIFOBUF) ? nleft : SZ_FIFOBUF;
		    if ((n = read (datain, op, n)) <= 0) {
			if (n < 0 || (time(0) - starttime > IO_TIMEOUT))
			    break;
		    } else {
			/* Set any zeroed pixels to the background color,
			 * if a special background color is specified.
			 */
			if (background)
			    for (cp=op, i=n;  --i >= 0;  cp++)
				if (!*cp)
				    *cp = background;
			op += n;
		    }
		}

		/* Refresh the display, if the current display frame is the
		 * same as the reference frame.
		 */
		if (rf_p == df_p) {
		    BRect    fb_r, pw_r;

		    fb_r.r_left   = x * zoom;
		    fb_r.r_top    = y * zoom;
		    fb_r.r_width  = min (nbytes * zoom, fb_width);
		    fb_r.r_height = ((nbytes*zoom*zoom + fb_width-1)/fb_width);

		    Bpw_get_region_rect (gio_pw, &pw_rect);
		    Bpw_lock (gio_pw, &pw_rect);

		    pw_rect.r_left = df_p->fb_xoff;
		    pw_rect.r_top  = df_p->fb_yoff;

		    if (maprect (&fb_rect, &fb_r, &pw_rect, &pw_r))
			if (maprect (&pw_rect, &pw_r, &fb_rect, &fb_r)) {
			    ds_write (gio_pw,
				pw_r.r_left, pw_r.r_top,
				pw_r.r_width, pw_r.r_height,
				PIX_SRC | PIX_COLOR(NGREY-1),
				df_p->fb_pr, fb_r.r_left, fb_r.r_top);

			    if (pw_r.r_top + pw_r.r_height >= pw_rect.r_height
				- cb_height)
				put_colorbar();
			}

		    Bpw_unlock (gio_pw);
		}

		return (NOTIFY_DONE);
	    }
	    break;

	case WCS:
	    /* Read or write the WCS for a frame.  The frame number to
	     * which the WCS applies is passed in Z and the frame buffer
	     * configuration in T.  The client changes the frame buffer
	     * configuration in a WCS set.  The WCS text follows the header
	     * as byte packed ASCII data.
	     */
	    if (iis.tid & IIS_READ) {
		/* Return the WCS for the referenced frame.
		 */
		char   emsg[SZ_FNAME];
		char   *text;
		int    frame;

		frame = decode_frameno (iis.z & 07777);
		if (frame > fb_nframes)
		    strcpy (text=emsg, "[NOSUCHFRAME]\n");
		else {
		    set_reference_frame (frame);
		    text = wcsbuf[reference_frame-1];
		}

		write (dataout, text, SZ_WCSBUF);

	    } else {
		/* Set the WCS for the referenced frame.
		 */
		char    buf[1024];
		int     fb_config, frame;

		frame = decode_frameno (iis.z & 07777);
		if (frame > fb_nframes)
		    if (frame < MAX_FRAMES)
			set_fbconfig (fb_config_index, frame);

		set_reference_frame (frame);
		if ((fb_config = iis.t & 077) != fb_config_index)
		    set_fbconfig (fb_config, reference_frame);

		/* Read in and set up the WCS. */
		if (read (datain, buf, ndatabytes) == ndatabytes)
		    strncpy (wcsbuf[reference_frame-1], buf, SZ_WCSBUF);

		strcpy (rf_p->fb_ctran.format, W_DEFFORMAT);
		rf_p->fb_ctran.imtitle[0] = '\0';
		rf_p->fb_ctran.valid = 0;
		rf_p->fb_imageno++;
		rf_p->fb_objno = 1;

		wcs_update (rf_p);
		if (rf_p == df_p)
		    window_set (gio_frame, FRAME_LABEL, framelabel(), 0);
	    }

	    return (NOTIFY_DONE);
	    break;

	case IMCURSOR:
	    /* Read or write the logical image cursor.  This is an extension
	     * added to provide a high level cursor read facility; this is
	     * not the same as a low level access to the IIS cursor subunit.
	     * Cursor reads may be either nonblocking (immediate) or blocking,
	     * using the keyboard or mouse to terminate the read, and
	     * coordinates may be returned in either image (world) or frame
	     * buffer pixel coordinates.
	     */
	    if (iis.tid & IIS_READ) {
		/* Read the logical image cursor.  In the case of a blocking
		 * read all we do is initiate a cursor read; completion occurs
		 * when the user hits a key or button.
		 */
		if (iis.tid & IMC_SAMPLE) {
		    /* Sample the cursor position. */
		    register struct ctran *ct;
		    int     wcs = iis.z;
		    int     sx, sy;
		    float   wx, wy;

		    wx = sx = last_sx + pw_rect.r_left;
		    wy = sy = last_sy + pw_rect.r_top;

		    if (wcs) {
			ct = wcs_update (df_p);
			if (ct->valid) {
			    if (abs(ct->a) > .001)
				wx = ct->a * sx + ct->c * sy + ct->tx;
			    if (abs(ct->d) > .001)
				wy = ct->b * sx + ct->d * sy + ct->ty;
			}
		    }

		    /* Return the cursor value on the output datastream encoded
		     * in a fixed size ascii buffer.
		     */
		    gio_retcursorval (wx, wy, display_frame*100+wcs, 0, "");

		} else {
		    /* Initiate a user triggered cursor read. */
		    gio_readcursor (iis.z);
		}

	    } else {
		/* Write (set) the logical image cursor position. */
		register struct ctran *ct;
		int     sx = iis.x,  sy = iis.y;
		float   wx = sx,  wy = sy;
		int     wcs = iis.z;

		if (wcs) {
		    ct = wcs_update (df_p);
		    if (ct->valid) {
			if (abs(ct->a) > .001)
			    sx = (wx - ct->tx) / ct->a;
			if (abs(ct->d) > .001)
			    sy = (wy - ct->ty) / ct->d;
		    }
		}

		gio_setcursorpos (sx - pw_rect.r_left, sy - pw_rect.r_top);
	    }

	    return (NOTIFY_DONE);
	    break;

	default:
	    /* Ignore unsupported command input.
	     */
	    break;
	}

	/* Discard any data following the header. */
	if (!(iis.tid & IIS_READ))
	    for (nbytes = ndatabytes;  nbytes > 0;  nbytes -= n) {
		n = (nbytes < SZ_FIFOBUF) ? nbytes : SZ_FIFOBUF;
		if ((n = read (datain, buf, n)) <= 0)
		    break;
	    }

	return (NOTIFY_DONE);
}


/* SET_REFERENCE_FRAME -- Set reference frame.  If the frame referenced is
 * greater than the current number of frames, attempt to increase the number
 * of frames.
 */
static
set_reference_frame (n)
register int	n;
{
	reference_frame = max (1, n);
	if (reference_frame > fb_nframes) {
	    if (reference_frame < MAX_FRAMES)
		set_fbconfig (fb_config_index, reference_frame);
	    else {
		fprintf (stderr, "imtool warning: ");
		fprintf (stderr, 
		    "attempt to reference nonexistent frame %d\n",
		    reference_frame);
		reference_frame = fb_nframes;
	    }
	}

	rf_p = frames + (reference_frame - 1);
}


/* DECODE_FRAMENO -- Decode encoded IIS register frame number.
 */
static
decode_frameno (z)
register int	z;
{
	register int	n;

	/* Get the frame number, encoded with a bit for each frame, 01 is
	 * frame 1, 02 is frame 2, 04 is frame 3, and so on.
	 */
	if (!z) z = 1;
	for (n=0;  !(z & 1);  z >>= 1)
	    n++;

	return (max (1, n + 1));
}


/* BSWAP2 - Move bytes from array "a" to array "b", swapping successive
 * pairs of bytes.  The two arrays may be the same but may not be offset
 * and overlapping.
 */
static
bswap2 (a, b, nbytes)
char 	*a, *b;		/* input array			*/
int	nbytes;		/* number of bytes to swap	*/
{
	register char *ip=a, *op=b, *otop;
	register unsigned temp;

	/* Swap successive pairs of bytes.
	 */
	for (otop = op + (nbytes & ~1);  op < otop;  ) {
	    temp  = *ip++;
	    *op++ = *ip++;
	    *op++ = temp;
	}

	/* If there is an odd byte left, move it to the output array.
	 */
	if (nbytes & 1)
	    *op = *ip;
}


/* Cursor and marker pixrects.
 */
static	short	p_imcursor[] = {
#include "imtool.cursor"
};
static	short	p_imcross[] = {
#include "imtool.cross"
};
static	short	p_imsquare[] = {
#include "imtool.square"
};
mpr_static (old_cursor, 16, 16, 1, NULL);
mpr_static (pr_cursor, 16, 16, 1, p_imcursor);
mpr_static (pr_cross, 16, 16, 1, p_imcross);
mpr_static (pr_square, 16, 16, 1, p_imsquare);
static struct pixrect *marker[] = { NULL, &pr_cursor, &pr_cross, &pr_square };

/* GIO_READCURSOR -- Initiate an image cursor read.  Save the current
 * mouse coordinates if outside the imtool window, restore the mouse to the
 * imtool window, and change the cursor shape to indicate that a cursor read
 * is in progress.  May be called while a cursor read is already in progress
 * to reset the cursor-read cursor pixrect.
 */
static
gio_readcursor (wcs)
int	wcs;
{
	Cursor cursor = window_get (gio_canvas, WIN_CURSOR);

	if (!reading_imcursor) {
	    /* Save cursor pixrect for later restore. */
	    old_cursor = *((Pixrect *) cursor_get (cursor, CURSOR_IMAGE));

	    /* Save the absolute mouse position so that we can restore it when
	     * the cursor read is completed.  Restore the mouse to the most
	     * recent position in the IMTOOL window.
	     */
	    get_absmousepos (gio_frame_fd, &save_sx, &save_sy);
	    gio_setcursorpos (last_sx, last_sy);

	    reading_imcursor++;
	    imcursor_wcs = wcs;
	}

	/* Change the cursor shape while the cursor read is in progress. */
	cursor_set (cursor,
	    CURSOR_IMAGE, &pr_cursor, 
	    CURSOR_SHOW_CURSOR, TRUE,
	    CURSOR_SHOW_CROSSHAIRS, FALSE,
	    CURSOR_OP, PIX_NOT(PIX_SRC) & PIX_DST | PIX_COLOR(CMS_CURSOR),
	    CURSOR_XHOT, 8,
	    CURSOR_YHOT, 8,
	    0);
	window_set (gio_canvas, WIN_CURSOR, cursor, 0);
}


/* GIO_RESTORECURSOR -- Restore the original cursor.
 */
static
gio_restorecursor()
{
	if (reading_imcursor) {
	    Cursor cursor;

	    /* Restore the mouse position to whatever it was before IMTOOL
	     * grabbed the mouse for the cursor read.
	     */
	    set_absmousepos (gio_frame_fd, save_sx, save_sy);

	    /* Restore the default IMTOOL cursor shape. */
	    cursor = window_get (gio_canvas, WIN_CURSOR);
	    cursor_set (cursor,
		CURSOR_IMAGE, &old_cursor, 
		CURSOR_SHOW_CURSOR, FALSE,
		CURSOR_SHOW_CROSSHAIRS, (cursor_show == CURSOR_ON),
		0);
	    window_set (gio_canvas, WIN_CURSOR, cursor, 0);

	    reading_imcursor = 0;
	}
}


/* GIO_RETCURSORVAL -- Return the cursor value on the output datastream to
 * the client which requested the cursor read.
 */
static
gio_retcursorval (wx, wy, wcs, key, strval)
float	wx, wy;			/* cursor coordinates */
int	wcs;			/* encoded WCS value */
int	key;			/* keystroke used as trigger */
char	*strval;		/* optional string value */
{
	char	curval[SZ_IMCURVAL];
	char	keystr[20];

	/* Encode the cursor value. */
	if (key == EOF)
	    sprintf (curval, "EOF\n");
	else {
	    if (isprint (key) && !isspace(key)) {
		keystr[0] = key;
		keystr[1] = '\0';
	    } else
		sprintf (keystr, "\\%03o", key);

	    sprintf (curval, "%10.3f %10.3f %d %s %s\n",
		wx, wy, wcs, keystr, strval);
	}

	/* Send it to the client program. */
	write (dataout, curval, sizeof(curval));
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

	value = notify_next_event_func (frame, event, arg, type);
	window_open = (((int) window_get (gio_frame, FRAME_CLOSED)) == 0);

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
	static	float	xsize, ysize;
	BRect	rect;
	char	ch;

	key = event_id (event);

	/* The following is to attempt to restore the image greyscale in the
	 * global color map, after the color map has been clobbered by the
	 * window manager when the mouse is moved to some other window.
	 */
	if (key == KBD_DONE || key == LOC_WINEXIT) {
	    edit_colormap();
	    return (NOTIFY_DONE);
	}

	/* Let frame operate upon the event. */
	if ((int)type != PANNER_EVENT)
	    value = notify_next_event_func (frame, event, arg, type);

	switch (key) {
	case WIN_RESIZE:
	    Bpw_get_region_rect (gio_pw, &pw_rect);
	    pw_rect.r_left = df_p->fb_xoff =
		max(0, min(fb_rect.r_width - pw_rect.r_width, df_p->fb_xoff));
	    pw_rect.r_top  = df_p->fb_yoff =
		max(0, min(fb_rect.r_height - pw_rect.r_height, df_p->fb_yoff));

	    gio_xsize = pw_rect.r_width;
	    gio_ysize = pw_rect.r_height;
	    init_colorbar (pw_rect.r_width);

	    wc_xoff = wc_yoff = 0;
	    if (display_coords && state == TRACK_CURSOR)
		update_coords (event);
	    break;

	case KEY_SNAP:
	    /* Imcopy. */
	    imagecopy_proc();
	    break;

	case KEY_PCOORDS:
	    /* Enable/disable continuous display of the cursor coordinates. */
	    toggle_displaycoords (event);
	    break;

	case KEY_SETUP:
	    /* Toggle display of the setup panel. */
	    setup_proc();
	    break;

	case KEY_REMARK:
	    /* Remark a list of objects. */
	    if (event_is_down(event)) {
		char    fname[SZ_FNAME];

		wcs_update (df_p);
		o_revtext = !o_revtext;
		sprintf (fname, o_fname, df_p->fb_frameno, df_p->fb_imageno);
		strcpy (fname, getfname(fname, 0));
		remark_objects (fname);
	    }
	    break;

	case MS_RIGHT:
	    if (event_is_down(event)) {
		last_sx = last_bx = event_x(event);
		last_sy = last_by = event_y(event);
	    }

	    /* If the cursor is moved while the right mouse button is
	     * depressed the image is windowed.  If the control key is also
	     * depressed the cursor is used to roam about in the frame buffer.
	     */
	    if (state == TRACK_CURSOR && event_is_down(event)) {
		if (event_ctrl_is_down (event))
		    state = ROAM;
		else
		    state = WINDOW;
		xsize = pw_rect.r_width;
		ysize = pw_rect.r_height;
		if (state == WINDOW) {
		    switch (df_p->fb_maptype) {
		    case MONO:
		    case HEAT:
		    case RAMP1:
		    case RAMP2:
		    case HALLEY:
		    case LINEARPS:
		    case ULUT1:
		    case ULUT2:
			compute_transfer_function (event);
			break;
		    case RANDOMPS:
		    case CRANDOMPS:
			set_colortable();
			break;
		    }
		    set_transfer_function (gio_pw,
			df_p->fb_center, df_p->fb_slope);
		}
	    } else if (state != TRACK_CURSOR && event_is_up(event))
		state = TRACK_CURSOR;
	    break;

	case LOC_DRAG:
	    last_sx = event_x (event);
	    last_sy = event_y (event);

	    if (panning)
		p_cursor_setback--;
	    if (state == WINDOW) {
		compute_transfer_function (event);
		set_transfer_function (gio_pw, df_p->fb_center, df_p->fb_slope);
	    }
	    break;

	case LOC_MOVE:
	    last_sx = event_x (event);
	    last_sy = event_y (event);

	    if (panning)
		p_cursor_setback--;
	    if (display_coords && state == TRACK_CURSOR)
		update_coords (event);
	    break;

	case MS_LEFT:
	    last_sx = event_x (event);
	    last_sy = event_y (event);

	    if (event_is_down (event)) {
		if (reading_imcursor) {
		    /* The left mouse button may be used to alias keyboard
		     * events.  Typing ctrl/button causes the last key to be
		     * aliased with the indicated button.  Thereafter, pressing
		     * that mouse button during a cursor read causes the cursor
		     * read to terminate, returning the aliased key just as if
		     * the key had been typed on the keyboard.
		     */
		    if (event_ctrl_is_down (event)) {
			if (key == MS_LEFT)
			    key_left = last_key;
		    } else if (reading_imcursor) {
			if (key == MS_LEFT)
			    key = key_left;
			if (key)
			    goto readcur;
		    }

		} else if (display_coords && state==TRACK_CURSOR) {
		    /* Add an object to a cursor list. */
		    mark_object (event);
		}
	    }
	    break;

	case MS_MIDDLE:
	    /* Pan - move the object under the cursor to the center of the
	     * display window (or as close as possible without wraparound).
	     */
	    if (event_is_up (event)) {
		int     fb_ex, fb_ey, fb_mx, fb_my, fb_xc, fb_yc, fb_nx, fb_ny;
		int     pw_xc, pw_yc, dx, dy, n_x, n_y;
		int     newzoom, close=1;

		/* Pressing the middle button without moving the mouse causes
		 * the zoom factor to be advanced.  In other words, placing
		 * the mouse on a feature and pressing the "zoom/pan" button
		 * causes that feature to be moved to the center of the
		 * display; pressing the button again causes the display to be
		 * zoomed about the centered feature.
		 */
		newzoom = zoom;
		n_x = event_x(event);
		n_y = event_y(event);

		if (abs(n_x-last_bx) <= close && abs(n_y-last_by) <= close) {
		    if (++zoom_index >= nzooms)
			zoom_index = 0;
		    newzoom = zooms[zoom_index];
		}

		last_sx = last_bx = n_x;
		last_sy = last_by = n_y;

		pw_xc = pw_rect.r_width / 2;	/* window center in pw */
		pw_yc = pw_rect.r_height / 2;
		fb_mx = pw_rect.r_left + n_x;	/* final mouse position */
		fb_my = pw_rect.r_top + n_y;
		fb_xc = pw_rect.r_left + pw_xc;	/* window center in fb */
		fb_yc = pw_rect.r_top + pw_yc;
		fb_nx = fb_ex = fb_mx;		/* next center */
		fb_ny = fb_ey = fb_my;

		dx = n_x - pw_xc;		/* step size */
		dy = n_y - pw_yc;

		/* Pan a long ways in the indicated direction, normally to the
		 * edge of the image.
		 */
		if (event_shift_is_down (event)) {
		    fb_ex = fb_nx = fb_mx =
			max(pw_xc, min(fb_rect.r_width - pw_xc, fb_xc + dx*5));
		    fb_ey = fb_ny = fb_my =
			max(pw_xc, min(fb_rect.r_height - pw_yc, fb_yc + dy*5));
		    if (!event_ctrl_is_down (event))
			dx = dy = 0;
		}

		/* Smooth pan to ex,ey.  May be combined with shift-pan. */
		if (event_ctrl_is_down (event)) {
		    dx = dx ? dx / abs(dx) : 0;
		    dy = dy ? dy / abs(dy) : 0;

		    /* Increase step size if window is large. */
		    if (pw_rect.r_width * pw_rect.r_height > 200000) {
			dx *= 2;
			dy *= 2;
		    }

		    fb_nx = max(0, min(fb_rect.r_width, fb_xc + dx));
		    fb_ny = max(0, min(fb_rect.r_height, fb_yc + dy));
		}

		/* Go to x,y starting at nx,ny stepping dx,dy per frame. */
		if (fb_nx != fb_xc || fb_ny != fb_yc || zoom != newzoom)
		    start_pan (fb_mx,fb_my, fb_nx,fb_ny, fb_ex,fb_ey, dx,dy,
			newzoom);
	    }
	    break;

	case INTERRUPT:
	    /* Abort any lengthy operation currently in progress. */
	    if (reading_imcursor)
		goto readcur;
	    else
		stop_pan();
	    break;

	case NEXT_SCREEN:
	    /* Display the next screen in numerical sequence. */
	    set_frame (0);
	    if (display_coords && state == TRACK_CURSOR)
		update_coords (event);
	    break;

	case PREV_SCREEN:
	    /* Display the previous screen in numerical sequence. */
	    set_frame (-1);
	    if (display_coords && state == TRACK_CURSOR)
		update_coords (event);
	    break;

	case CYCLE_BLINK:
	    /* Display the next screen from the blink frames list.
	     * The frames in the blink frame list do not have to exist;
	     * if not, then advance through the list until either a valid
	     * frame is found, or the list has been traversed once.
	     */
	    if (n_blink_frames > 0 && window_open) {
		int    frame, n;
		for (n=n_blink_frames;  --n >= 0;  ) {
		    if (++blink_frame >= n_blink_frames)
			blink_frame = 0;
		    frame = blink_frames[blink_frame];
		    if (frame >= 1 && frame <= fb_nframes) {
			set_frame (frame);
			if (display_coords && state == TRACK_CURSOR)
			    update_coords (NULL);
			break;
		    }
		}
	    }
	    break;

	default:
	    /* Terminate a cursor read, returning the encoded cursor value
	     * sequence to client program on the output datastream.
	     */
	    if (event_is_down(event) &&
	       (event_is_ascii(event) || event_is_key_right(event))) {
readcur:
		last_sx = event_x (event);
		last_sy = event_y (event);

		/* Terminate cursor read? */
		if (reading_imcursor) {
		    register struct ctran *ct;
		    int     wcs = imcursor_wcs;
		    int     sx, sy;
		    float   wx, wy;

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

		    last_x = event_x (event);
		    last_y = event_y (event);
		    last_key = key;

		    wx = sx = last_x + pw_rect.r_left;
		    wy = sy = last_y + pw_rect.r_top;

		    if (wcs) {
			ct = wcs_update (df_p);
			if (ct->valid) {
			    if (abs(ct->a) > .001)
				wx = sx * ct->a + ct->tx;
			    if (abs(ct->d) > .001)
				wy = sy * ct->d + ct->ty;
			}
		    }

		    /* Map ctrl/d and ctrl/z into EOF. */
		    if (key == '\004' || key == '\032')
			key = EOF;
		    else if (marktype) {
			/* Mark the cursor position? */
			edit_framebuffer (df_p, sx/zoom - 8, sy/zoom - 8,
			    marker[marktype],
			    PIX_NOT(PIX_SRC) & PIX_DST);
		    }

		    /* Return the cursor value on the output datastream encoded
		     * in a fixed size ascii buffer.
		     */
		    gio_retcursorval (wx, wy, display_frame*100+wcs, key, "");

		    /* Terminate the cursor read. */
		    gio_restorecursor();
		}
	    }
	}

	return (value);
}


static	int p_dx, p_dy;
static	int p_sx, p_sy;
static	int p_left, p_top;
static	int p_svdc;

/* START_PAN -- Pan the image smoothly to the indicated position.  This can
 * take a while, so we want to do it on a timer and provide for interrupt.
 * A new pan can be started to change the destination while an old pan is
 * still in progress.
 */
static
start_pan (fb_mx,fb_my, fb_nx,fb_ny, fb_ex,fb_ey, dx,dy, newzoom)
int	fb_mx, fb_my;		/* mouse position in frame buffer	*/
int	fb_nx, fb_ny;		/* center of window at next display	*/
int	fb_ex, fb_ey;		/* center of window at final display	*/
int	dx, dy;			/* step size for pan			*/
register int newzoom;		/* new zoom factor			*/
{
	register int	w, h;
	int	n_left, n_top;
	int	n_sx, n_sy, e_sx, e_sy;

	if (!panning) {
	    p_svdc = display_coords;
	    display_coords = 0;
	    panning++;
	}

	p_cursor_setback = 9;
	gio_setcursor (CURSOR_OFF, 0);

	Bpw_get_region_rect (gio_pw, &pw_rect);
	w = pw_rect.r_width;
	h = pw_rect.r_height;

	/* Scale zoomed frame buffer units to new zoom factor.
	 */
	p_sx = fb_mx * newzoom / zoom;
	p_sy = fb_my * newzoom / zoom;
	n_sx = fb_nx * newzoom / zoom;
	n_sy = fb_ny * newzoom / zoom;
	e_sx = fb_ex * newzoom / zoom;
	e_sy = fb_ey * newzoom / zoom;
	p_dx = dx * newzoom;
	p_dy = dy * newzoom;

	/* The following are the final left,top values. */
	p_left = max(0, min(Fb_width*newzoom - w,
	    (e_sx - w/2) / newzoom * newzoom));
	p_top  = max(0, min(Fb_height*newzoom - h,
	    (e_sy - h/2) / newzoom * newzoom));

	/* The following are the left,top values for the next display. */
	n_left = max(0, min(Fb_width*newzoom - w,
	    (n_sx - w/2) / newzoom * newzoom));
	n_top  = max(0, min(Fb_height*newzoom - h,
	    (n_sy - h/2) / newzoom * newzoom));

	/* Set the new zoom factor for the display. */
	set_zoom (df_p, n_left, n_top, newzoom);

	notify_post_event (ev_panner, NULL, NOTIFY_SAFE);
}


/* STOP_PAN -- Called to abort a pan. */
static
stop_pan()
{
	p_dx = p_dy = 0;
}


/* PANNER -- Called on a fast timer to do the panning. */
static Notify_value
ev_panner()
{
	register int	left, top;

	/* No clipping is done, so the imtool window must be exposed. */
	window_set (gio_frame, WIN_SHOW, TRUE, 0);

	/* repaint (P_IMAGE|P_DONTCLIP); */
	repaint (P_IMAGE);

	if (p_dx < 0 && pw_rect.r_left <= p_left ||
	    p_dx > 0 && pw_rect.r_left >= p_left)
	    p_dx = 0;
	else if (p_dx) {
	    left = pw_rect.r_left + p_dx;
	    if (p_dx < 0)
		left = max (p_left, left);
	    else
		left = min (p_left, left);
	    df_p->fb_xoff = pw_rect.r_left = left;
	}

	if (p_dy < 0 && pw_rect.r_top <= p_top ||
	    p_dy > 0 && pw_rect.r_top >= p_top)
	    p_dy = 0;
	else if (p_dy) {
	    top = pw_rect.r_top + p_dy;
	    if (p_dy < 0)
		top = max (p_top, top);
	    else
		top = min (p_top, top);
	    df_p->fb_yoff = pw_rect.r_top = top;
	}

	if (p_dx == 0 && p_dy == 0) {
	    if (panning) {
		panning = 0;
		display_coords = p_svdc;
		if (p_cursor_setback > 0) {
		    int sx = p_sx - pw_rect.r_left;
		    int sy = p_sy - pw_rect.r_top;
		    if (sx >= 0 && sx < pw_rect.r_width &&
		        sy >= 0 && sy < pw_rect.r_height)
			gio_setcursorpos (sx, sy);
			gio_setcursor (CURSOR_ON, 0);
			gio_events();
		}
		repaint (P_GRAPHICS|P_COLORBAR);
	    }
	} else {
	    /* Allow the window to process any pending events. */
	    gio_events();

	    /* Post another call to the panner. */
	    notify_post_event (ev_panner, NULL, NOTIFY_SAFE);
	}

	return (NOTIFY_DONE);
}


/* EDIT_FRAMEBUFFER -- Edit a frame buffer by operating upon it with the
 * given pixrect and rasterop at the given location, clipping as necessary
 * at the boundaries of the frame.  Update the display window as well, if
 * the frame being edited is the display frame.
 */
static
edit_framebuffer (fb, x, y, pr, rop)
struct	framebuf *fb;		/* frame to be edited */
int	x, y;			/* left,top coords of rect to be edited */
struct	pixrect *pr;		/* pixrect to be used */
int	rop;			/* rasterop defining operation */
{
	int     width = pr->pr_width, height = pr->pr_height;
	int     s_left = 0, s_top = 0;
	int     d_left = x, d_top = y;

	/* Clip to the frame boundary. */
	while (d_left < 0) {
	    s_left++;
	    width--;
	    d_left++;
	}

	while (d_left + width-1 > Fb_width)
	    width--;

	while (d_top < 0) {
	    s_top++;
	    height--;
	    d_top++;
	}

	while (d_top + height-1 > Fb_height)
	    height--;

	/* All done if there is nothing left after clipping. */
	if (width*height <= 0)
	    return;

	/* Edit the frame buffer (clobbers the display pixels).  */
	pr_rop (fb->fb_pr, d_left, d_top, width, height,
	    PIX_NOT(PIX_SRC) & PIX_DST, pr, s_left, s_top);

	/* Refresh the display, if the current display frame is the
	 * same as the reference frame.
	 */
	if (fb == df_p) {
	    BRect    fb_r, pw_r;

	    fb_r.r_left   = d_left * zoom;
	    fb_r.r_top    = d_top * zoom;
	    fb_r.r_width  = width * zoom;
	    fb_r.r_height = height * zoom;

	    Bpw_get_region_rect (gio_pw, &pw_rect);
	    Bpw_lock (gio_pw, &pw_rect);

	    pw_rect.r_left = df_p->fb_xoff;
	    pw_rect.r_top  = df_p->fb_yoff;

	    if (maprect (&fb_rect, &fb_r, &pw_rect, &pw_r))
		if (maprect (&pw_rect, &pw_r, &fb_rect, &fb_r)) {
		    ds_write (gio_pw,
			pw_r.r_left, pw_r.r_top,
			pw_r.r_width, pw_r.r_height,
			PIX_SRC | PIX_COLOR(NGREY-1),
			df_p->fb_pr, fb_r.r_left, fb_r.r_top);

		    if (pw_r.r_top + pw_r.r_height >= pw_rect.r_height
			- cb_height)
			put_colorbar();
		}

	    Bpw_unlock (gio_pw);
	}
}


/* GIO_EVENTS -- Have the image window process any queued input events.
 */
static
gio_events()
{
	Event    event;
	int      fd, flags;

	/* Allow the window to process any pending events. */
	fd = (int) window_get (gio_canvas, WIN_FD);
	flags = fcntl (fd, F_GETFL, 0);
	fcntl (fd, F_SETFL, O_NDELAY);
	while (window_read_event (gio_canvas, &event) != -1)
	    ev_gioinput (gio_canvas,
		canvas_event(gio_canvas, &event), NULL, PANNER_EVENT);
	fcntl (fd, F_SETFL, flags);
}


/* ICLEAR_PROC -- Clear the main (image) window.
 */
static
iclear_proc()
{
	erase (df_p);
}


/* GCLEAR_PROC -- Clear the graphics overlay.
 */
static
gclear_proc()
{
	df_p->fb_objno = 1;
	repaint (P_IMAGE|P_COLORBAR);
}


/* SETFRAME_PROC -- Select the next frame for viewing.
 */
static
setframe_proc()
{
	set_frame (0);
}


/* SET_FRAME -- Set the display frame.  Call with frameno=0 to advance to
 * the next frame, -N will yield the previous frame in sequence, and
 * anything else is actual frame number.
 */
static
set_frame (frameno)
int	frameno;
{
	if (frameno < 0) {
	    frameno = display_frame - 1;
	    if (frameno < 1)
		frameno = fb_nframes;
	} else if (frameno == 0) {
	    frameno = display_frame + 1;
	    if (frameno > fb_nframes)
		frameno = 1;
	} else {
	    if (frameno < 1)
		frameno = 1;
	    else if (frameno > fb_nframes)
		frameno = fb_nframes;
	}

	display_frame = frameno;
	df_p = frames + (frameno - 1);
	set_zoom (df_p, df_p->fb_xoff, df_p->fb_yoff, df_p->fb_xzoom);

	set_colortable();
	set_transfer_function (gio_pw, df_p->fb_center, df_p->fb_slope);
	window_set (gio_frame, FRAME_LABEL, framelabel(), 0);
	panel_set_value (pan_set_maptype, df_p->fb_maptype);

	repaint (P_IMAGE|P_COLORBAR|P_GRAPHICS);
}


/* SET_ZOOM -- Change the zoom factor for the referenced frame to the given
 * value.  If the referenced frame is the display frame, update the global
 * display zoom factors as well.
 */
static
set_zoom (fr, left, top, newzoom)
register struct framebuf *fr;	/* frame to be zoomed			*/
int	left, top;		/* new left and top for frame		*/
int	newzoom;		/* new zoom factor			*/
{
	register struct	ctran *ct;
	int	fb_zoom, i;

	/* Verify valid zoom factor. */
	newzoom = max(zooms[0], min(zooms[nzooms-1], newzoom));
	for (i=0;  i < nzooms;  i++)
	    if (zooms[i] == newzoom) {
		zoom_index = i;
		break;
	    }

	/* Set the new frame buffer zoom factor. */
	if ((fb_zoom = fr->fb_xzoom) != newzoom) {
	    ct = &fr->fb_ctran;
	    ct->a = ct->a * fb_zoom / newzoom;
	    ct->d = ct->d * fb_zoom / newzoom;

	    /* For Apply a 0.5 pixel correction when zooming, to make the
	     * center of the pixel have integral coordinates (coord X,Y where
	     * X and Y are integral will always be the center of a pixel).
	     * This should be turned off for zoom=1, since there is no
	     * subpixel resolution, or if the image has already been zoomed
	     * at the host level.
	     */
	    if (abs(ct->a * newzoom) < 1.01) {
		if (fb_zoom == 1)
		    ct->tx += (ct->a > 0) ? -0.5 :  0.5;
		else if (newzoom == 1)
		    ct->tx += (ct->a > 0) ?  0.5 : -0.5;
	    }
	    if (abs(ct->d * newzoom) < 1.01) {
		if (fb_zoom == 1)
		    ct->ty += (ct->d > 0) ? -0.5 :  0.5;
		else if (newzoom == 1)
		    ct->ty += (ct->d > 0) ?  0.5 : -0.5;
	    }

	    fr->fb_xzoom = fr->fb_yzoom = newzoom;
	}

	/* Offsets must be aligned to an unzoomed frame buffer pixel
	 * boundary since this constraint is applied in ds_write.
	 */
	fr->fb_xoff = max(0, min(Fb_width*newzoom - pw_rect.r_width,
	    left / newzoom * newzoom));
	fr->fb_yoff = max(0, min(Fb_height*newzoom - pw_rect.r_height,
	    top  / newzoom * newzoom));

	/* If the referenced frame is the display frame, make the new zoom
	 * factor global.
	 */
	if (fr == df_p) {
	    fb_width = Fb_width * newzoom;
	    fb_height = Fb_height * newzoom;

	    fb_rect.r_top = 0;
	    fb_rect.r_left = 0;
	    fb_rect.r_width = fb_width;
	    fb_rect.r_height = fb_height;

	    pw_rect.r_left = df_p->fb_xoff;
	    pw_rect.r_top  = df_p->fb_yoff;

	    zoom = newzoom;
	}
}


/* ERASE -- Clear a frame.
 */
static
erase (fr)
struct framebuf *fr;
{
	register int *op, v, n;
	unsigned char *cp;
	int	val;

	for (val=0, n=sizeof(int), cp = (unsigned char *)&val;  --n >= 0;  )
	    *cp++ = background;

	if (val) {
	    op = (int *) mpr_d(fr->fb_pr)->md_image;
	    n = Fb_width * Fb_height / sizeof(int);
	    for (v=val;  --n >= 0;  )
		*op++ = v;
	} else
	    bzero ((char *)mpr_d(fr->fb_pr)->md_image, Fb_width * Fb_height);

	if (fr == df_p)
	    repaint (P_IMAGE|P_COLORBAR);
}


/* REPAINT -- Repaint the display window.
 */
static
repaint (what)
int	what;
{
	if (what & P_IMAGE) {
	    BRect  fb_r, pw_r;
	    int    rop;

	    pw_r = pw_rect;
	    pw_r.r_left = pw_r.r_top = 0;
	    if (what & P_COLORBAR)
		pw_r.r_height -= cb_height;

	    rop = PIX_SRC | PIX_COLOR(NGREY-1);
	    if (what & P_DONTCLIP)
		rop |= PIX_DONTCLIP;

	    if (maprect (&pw_rect, &pw_r, &fb_rect, &fb_r))
		if (maprect (&fb_rect, &fb_r, &pw_rect, &pw_r))
		    ds_write (gio_pw,
			pw_r.r_left, pw_r.r_top, pw_r.r_width, pw_r.r_height,
			rop, df_p->fb_pr, fb_r.r_left, fb_r.r_top);

	    if (display_coords && state == TRACK_CURSOR) {
		set_wcsboxpos();
		pw_text (gio_pw, wc_xoff, wc_yoff + wc_font->pf_defaultsize.y,
		    PIX_NOT(PIX_SRC), wc_font, wc_text);
	    }
	}

	if (what & P_COLORBAR)
	    put_colorbar();

	if ((what & P_GRAPHICS) && df_p->fb_objno > 1) {
	    char    fname[SZ_FNAME];

	    wcs_update (df_p);
	    sprintf (fname, o_fname, df_p->fb_frameno, df_p->fb_imageno);
	    strcpy (fname, getfname(fname, 0));
	    remark_objects (fname);
	}
}


/* REFRESH_DISPLAY -- Called by the windowing system when the display needs
 * to be refreshed from the frame buffer.
 */
static
refresh_display (canvas, pw, rl)
Canvas	canvas;
Pixwin	*pw;
Rectlist *rl;
{
	register struct	rectnode *rn;
	BRect	fb_r, pw_r;
	Rect	rect, r;

	/* See if any damage has occurred and fix it. */
	rl_rectoffset (rl, &rl->rl_bound, &rect);
	pw_lock (pw, &rect);

	/* Now fix all the damage.  Regions of the display window which are
	 * not mapped onto the frame buffer are not fixed up at present.
	 * Scale changes (zoom/dezoom) are not currently immplemented.
	 */
	for (rn = rl->rl_head;  rn;  rn = rn->rn_next) {
	    rl_rectoffset (rl, &rn->rn_rect, &r);
	    pw_r.r_left   = r.r_left;
	    pw_r.r_top    = r.r_top;
	    pw_r.r_width  = r.r_width;
	    pw_r.r_height = r.r_height;

	    if (maprect (&pw_rect, &pw_r, &fb_rect, &fb_r))
		if (maprect (&fb_rect, &fb_r, &pw_rect, &pw_r))
		    ds_write (pw,
			pw_r.r_left, pw_r.r_top, pw_r.r_width, pw_r.r_height,
			PIX_SRC | PIX_COLOR(NGREY-1),
			df_p->fb_pr, fb_r.r_left, fb_r.r_top);
	}

	put_colorbar();
	pw_unlock (pw);
}


/* DS_WRITE -- Write to the display.  This is analogous to pw_write, except
 * that pixel replication or subsampling is performed as indicated by the
 * zoom factors for the current display frame.  At present, the zoom factor
 * may not be specified independently for x and y.
 */
static
ds_write (pw, left, top, width, height, rop, fb_pr, fb_left, fb_top)
Pixwin	*pw;
int	left, top;
int	width, height;
int	rop;
Pixrect	*fb_pr;
int	fb_left, fb_top;		/* zoomed frame buffer coords */
{
	register unsigned char pix, *ip, *op;
	register int n, p;
	unsigned char *otop, *fb, *lp;
	int	pr_left, pr_top, i, j;
	Pixrect	*mpr_line, *pr;
	struct	rect pw_r;

	if (width <= 0 || height <= 0)
	    return;

	/* If no zoom, just copy the frame buffer rect to the screen. */
	if (zoom <= 1) {
	    pw_write (pw, left,top, width,height, rop, fb_pr, fb_left,fb_top);
	    return;
	}

	/* Zoom - magnify the image by pixel replication.  (This assumes an
	 * 8 bit frame buffer and screen pixrect).
	 */
	mpr_line = mem_create (width, 1, pw->pw_pixrect->pr_depth);
	fb = (unsigned char *)mpr_d(df_p->fb_pr)->md_image;
	lp = (unsigned char *)mpr_d(mpr_line)->md_image;
	otop = lp + width;

	/* Lock the frame buffer to avoid scribbling on other windows
	 * during write to raw screen..
	 */
	pw_get_region_rect (pw, &pw_r);
	pw_lock (pw, &pw_r);

	pr = pw->pw_pixrect;
	pr_left = left + (int)window_get(gio_frame, WIN_X) + TOOL_BORDERWIDTH;
	pr_top  = top  + (int)window_get(gio_frame, WIN_Y) +
	    HEIGHTADJUST - TOOL_BORDERWIDTH;

	for (j=0, i=(fb_top/zoom);  j < height;  j += zoom) {
	    ip = fb + (i++ * Fb_width) + (fb_left/zoom);
	    op = lp;

	    /* Replicate a block of pixels. */
	    switch (zoom) {
	    case 2:
		for (n = (width/2);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 3:
		for (n = (width/3);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		}
		break;
	    case 4:
		for (n = (width/4);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 5:
		for (n = (width/5);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 6:
		for (n = (width/6);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		}
		break;
	    case 7:
		for (n = (width/7);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    case 8:
		for (n = (width/8);  --n >= 0;  ) {
		    pix = *ip++;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		    *op++ = pix;  *op++ = pix;
		}
		break;
	    default:
		for (n = (width/zoom);  --n >= 0;  ) {
		    pix = *ip++;
		    for (p=zoom;  --p >= 0;  )
			*op++ = pix;
		}
		break;
	    }

	    /* Fill the last partial pixel. */
	    pix = *ip++;
	    while (op < otop)
		*op++ = pix;

	    pr_replrop (pr, pr_left,pr_top+j,
		width, min (height-j, zoom), rop, mpr_line, 0,0);
	}

	pw_unlock (pw);
	pr_close (mpr_line);
}


/* PUT_COLORBAR -- Refresh the colorbar on the screen.
 */
static
put_colorbar()
{
	if (cb_height)
	    pw_write (gio_pw,
		0, pw_rect.r_height - cb_height,
		min (pw_rect.r_width, Fb_width), cb_height,
		PIX_SRC, cb_pr, 0, 0);
}


/* TOGGLE_DISPLAYCOORDS -- Enable/disable continuous display of the cursor
 * coordinates.
 */
static
toggle_displaycoords (event)
Event	*event;
{
	BRect	fb_r, pw_r;

	if (display_coords) {
	    /* Enable mouse moved input events. */
	    window_set (gio_canvas, WIN_IGNORE_PICK_EVENTS, LOC_MOVE, 0,
		0);
	    display_coords = 0;

	    /* Refresh the region of the screen used to output coordinates
	     * from the frame buffer, erasing the coordinate output box.
	     */
	    pw_r.r_left   = wc_xoff;
	    pw_r.r_top    = wc_yoff;
	    pw_r.r_width  = wc_width;
	    pw_r.r_height = wc_height;

	    if (maprect (&pw_rect, &pw_r, &fb_rect, &fb_r))
		ds_write (gio_pw,
		    wc_xoff, wc_yoff, wc_width, wc_height,
		    PIX_SRC | PIX_COLOR(NGREY-1),
		    df_p->fb_pr, fb_r.r_left, fb_r.r_top);

	} else {
	    /* Disable mouse moved input events. */
	    window_set (gio_canvas, WIN_CONSUME_PICK_EVENTS, LOC_MOVE, 0,
		0);
	    display_coords = 1;
	    update_coords (event);
	}
}


/* UPDATE_COORDS -- Compute and output the world coordinates of the given
 * event, using the WCS specified for the current display window.  If called
 * with event=NULL the most recent locator position is used.
 */
static
update_coords (event)
Event	*event;
{
	register struct	ctran *ct;
	static	struct timeval o_tv;
	unsigned char *fb, *ip;
	struct	timeval n_tv;
	int	sx, sy, sz, fb_x, fb_y, delta_msec;
	char	buf[1024], ch;
	float	wx, wy, wz;

	/* Get frame buffer x,y; ignore events that occur faster than we
	 * can update the coordinate readout.
	 */
	if (event) {
	    /* Ignore event if it comes too soon. */
	    n_tv = event_time(event);
	    if (o_tv.tv_sec) {
		delta_msec = ((n_tv.tv_sec  - o_tv.tv_sec)  * 1000 +
			      (n_tv.tv_usec - o_tv.tv_usec) / 1000);
		if (delta_msec < 50)
		     return;
	    }

	    /* Get the screen (window relative) coordinates of the event. */
	    sx = event_x(event) + pw_rect.r_left;
	    sy = event_y(event) + pw_rect.r_top;

	} else {
	    sx = last_sx + pw_rect.r_left;
	    sy = last_sy + pw_rect.r_top;
	}

	/* Get frame buffer pixel value. */
	fb = (unsigned char *) mpr_d (df_p->fb_pr)->md_image;
	fb_x = max(0, min(Fb_width,  sx/zoom));
	fb_y = max(0, min(Fb_height, sy/zoom));
	sz = fb[fb_y*Fb_width+fb_x];
	if (sz < CMS_DATASTART || sz > CMS_DATAEND)
	    sz = 0;

	/* Compute the world coordinates of the event. */
	ct = wcs_update (df_p);

	if (ct->valid) {
	    wx = ct->a * sx + ct->c * sy + ct->tx;
	    wy = ct->b * sx + ct->d * sy + ct->ty;

	    if (sz == 0)
		wz = 0.0;
	    else {
		switch (ct->zt) {
		case W_LINEAR:
		    wz = ((sz - CMS_DATASTART) * (ct->z2 - ct->z1) /
			  (CMS_DATARANGE-1)) + ct->z1;
		    break;
		default:
		    wz = sz;
		    break;
		}
	    }

	} else {
	    wx = sx;
	    wy = sy;
	    wz = sz;
	}

	/* Get the font to be used. */
	if (wc_font == NULL) {
	    static char fontname[] = TEXT_FONT;
	    if ((wc_font = pf_open (fontname)) == NULL)
		fprintf (stderr, "cannot open %s\n", fontname);
	    wc_xoff = wc_yoff = 0;
	}

	ch = ' ';
	if (sz && ct->valid) {
	    if (ct->z1 < ct->z2) {
		if (wz < (ct->z1 + 0.01))
		    ch = '-';
		else if (wz > (ct->z2 - 0.01))
		    ch = '+';
	    } else if (ct->z1 > ct->z2) {
		if (wz < (ct->z2 + 0.01))
		    ch = '-';
		else if (wz > (ct->z1 - 0.01))
		    ch = '+';
	    }
	}

	set_wcsboxpos();
	sprintf (buf, ct->format, wx, wy, wz, ch);
	strncpy (wc_text, buf, SZ_WCTEXT);
	pw_text (gio_pw, wc_xoff, wc_yoff + wc_font->pf_defaultsize.y,
	    PIX_NOT(PIX_SRC), wc_font, wc_text);

	if (event)
	    o_tv = n_tv;
}


/* MARK_OBJECT -- Called when the user has clicked on the position of an
 * object to be marked and/or added to the output coordinate list.
 */
static
mark_object (event)
Event	*event;
{
	register struct	ctran *ct;
	char	tx_buf[SZ_FNAME];
	char	fname[SZ_FNAME];
	int	sx, sy, newset;
	float	wx, wy;
	FILE	*fp;

	ct = wcs_update (df_p);

	/* Get name of coordinate output file for the current frame. */
	sprintf (tx_buf, o_fname, df_p->fb_frameno, df_p->fb_imageno);
	strcpy (fname, getfname(tx_buf, 0));

	/* Append to the existing coordinate list, if any.  If appending
	 * to an existing coordinate list, the existing list is displayed
	 * and the objno counter is left set to the next object to be added.
	 */
	if (newset = (df_p->fb_objno <= 1))
	    remark_objects (fname);
	if ((fp = fopen (fname, "a")) == NULL) {
	    fprintf (stderr, "cannot open %s for appending\n", fname);
	    return;
	}

	/* Timestamp the first entry in the output file. */
	if (newset)
	    timestamp (fp);

	/* Get the screen (window relative) coordinates of the event. */
	sx = event_x(event) + pw_rect.r_left;
	sy = event_y(event) + pw_rect.r_top;

	/* Compute the world coordinates of the event. */
	if (ct->valid) {
	    wx = ct->a * sx + ct->c * sy + ct->tx;
	    wy = ct->b * sx + ct->d * sy + ct->ty;
	} else {
	    wx = sx;
	    wy = sy;
	}

	/* Mark the object position on the screen. */
	sprintf (tx_buf, "%d", df_p->fb_objno++);
	draw_text (sx - pw_rect.r_left, sy - pw_rect.r_top, tx_buf);

	fprintf (fp, "%g %g\n", wx, wy);
	fclose (fp);
}


/* REMARK_OBJECTS -- Read a object list file and mark the numbered objects
 * therein.
 */
static
remark_objects (fname)
char	*fname;
{
	register struct	ctran *ct;
	register char	*ip;
	char	lbuf[SZ_LINE], tx_buf[SZ_FNAME];
	int	sx, sy, objno=1;
	float	wx, wy;
	char	*fgets();
	FILE	*fp;

	gclear_proc();
	window_set (gio_frame, FRAME_LABEL, framelabel(), 0);
	if ((fp = fopen (fname, "r")) == NULL)
	    return;

	while (fgets (lbuf, SZ_LINE, fp) != NULL) {
	    /* Skip comment lines and blank lines. */
	    for (ip=lbuf;  *ip == ' ' || *ip == '\t';  ip++)
		;
	    if (*ip == '\n' || *ip == '#')
		continue;
	    if (!isdigit (*ip) && *ip != '-')
		continue;
	    if (sscanf (ip, "%f%f", &wx, &wy) < 2)
		continue;

	    sx = wx;
	    sy = wy;

	    /* Compute the world coordinates of the event if we have a valid
	     * WCS transform (rotations not permitted)/
	     */
	    ct = wcs_update (df_p);
	    if (ct->valid) {
		if (abs(ct->a) > .001)
		    sx = (wx - ct->tx) / ct->a;
		if (abs(ct->d) > .001)
		    sy = (wy - ct->ty) / ct->d;
	    }

	    /* Mark the object position on the screen. */
	    sprintf (tx_buf, "%d", objno++);
	    draw_text (sx - pw_rect.r_left, sy - pw_rect.r_top, tx_buf);
	}

	fclose (fp);

	/* If we are updating an existing coordinate list from a newly
	 * displayed frame, timestamp the new section of the list.
	 */
	if (df_p->fb_objno <= 1)
	    if ((fp = fopen (fname, "a")) != NULL) {
		timestamp (fp);
		fclose (fp);
	    }

	df_p->fb_objno = objno;
}


/* TIMESTAMP -- Timestamp the output stream.
 */
static
timestamp (fp)
FILE	*fp;
{
	register char	*op;
	char	obuf[SZ_LINE];
	long	clock;

	clock = time(0);
	fprintf (fp, "# %s", asctime(localtime(&clock)));

	sprintf (obuf, "# %s", df_p->fb_label);
	for (op=obuf;  *op && *op != '\n';  op++)
	    ;
	*op++ = '\n';
	*op = '\0';
	fputs (obuf, fp);
}


/* DRAW_TEXT -- Draw some text on the frame at the indicated position.
 */
static
draw_text (x, y, text)
int	x, y;			/* position where text is to be drawn */
char	*text;			/* the text */
{
	static	struct pixfont *font = NULL;

	/* Get the screen font to be used. */
	if (font == NULL) {
	    static char fontname[] = MARK_FONT;
	    if ((font = pf_open (fontname)) == NULL)
		fprintf (stderr, "cannot open %s\n", fontname);
	}

	/* Draw the text. */
	if (o_revtext)
	    pw_text (gio_pw, x, y, PIX_NOT(PIX_SRC) & PIX_DST, font, text);
	else
	    pw_text (gio_pw, x, y, PIX_SRC | PIX_DST, font, text);
}


/* WCS_UPDATE -- Load the screen WCS, if not yet validated, from the user
 * wcs file, if any.
 *
 * File format (two lines):
 *
 *	image title (imtool header label string)\n
 *	a b c d tx ty z1 z2 zt
 *
 * NOTE: the WCS text is now passed in via the data stream as a write to the
 * subunit WCS and left in the buffer "wcsbuf", rather than being passed via
 * a text file.
 */
static struct ctran *
wcs_update (fr)
struct	framebuf *fr;
{
	register struct ctran *ct = &fr->fb_ctran;
	char	buf[1024], *format;

	/* Get the new WCS. */
	if (!ct->valid) {
	    fr->fb_label[0] = '\0';
	    ct->zt = W_UNITARY;

	    /* Attempt to read the WCS file and set up a unitary transformation
	     * if the file cannot be read.
	     */
	    if (sscanf (wcsbuf[fr->fb_frameno-1], "%[^\n]\n%f%f%f%f%f%f%f%f%d",
		buf, &ct->a, &ct->b, &ct->c, &ct->d, &ct->tx, &ct->ty,
		&ct->z1, &ct->z2, &ct->zt) < 7) {

		if (wcsbuf[fr->fb_frameno-1][0])
		    fprintf (stderr, "imtool: error reading WCS file\n");

		strncpy (ct->imtitle, "[NO WCS]\n", SZ_IMTITLE);
		ct->a  = ct->d  = 1;
		ct->b  = ct->c  = 0;
		ct->tx = ct->ty = 0;
		ct->zt = W_UNITARY;

	    } else
		strncpy (ct->imtitle, buf, SZ_IMTITLE);

	    /* Correct for the current zoom factor, if any. */
	    if (fr->fb_xzoom > 1) {
		if (abs(ct->a) < 1.01)
		    ct->tx += (ct->a > 0) ? -0.5 : 0.5;
		if (abs(ct->d) < 1.01)
		    ct->ty += (ct->d > 0) ? -0.5 : 0.5;
		ct->a = ct->a / fr->fb_xzoom;
		ct->d = ct->d / fr->fb_xzoom;
	    }

	    window_set (gio_frame, FRAME_LABEL, framelabel(), 0);
	    ct->valid++;
	}

	/* Determine best format for wcs output. */
	if (ct->valid && ct->zt == W_LINEAR) {
	    float   z1, z2, zrange;
	    z1 = ct->z1;
	    z2 = ct->z2;
	    zrange = (z1 > z2) ? z1 - z2 : z2 - z1;
	    if (zrange < 100.0 && (abs(z1) + abs(z2)) / 2.0 < 200.0)
		format = " %7.2f %7.2f %7.3f%c";
	    else if (zrange > 99999.0 || (abs(z1) + abs(z2)) / 2.0 > 99999.0)
		format = " %7.2f %7.2f %7.3g%c";
	    else
		format = W_DEFFORMAT;
	} else
	    format = " %7.2f %7.2f %7.0f%c";

	strcpy (ct->format, format);
	return (ct);
}


/* SET_WCSBOXPOS -- Set the position of the WCS output box.
 */
static
set_wcsboxpos()
{
	/* Compute offset to coordinate output box. */
	if ((wc_xoff + wc_yoff) == 0) {
	    wc_width  = wc_font->pf_defaultsize.x * 25;
	    wc_height = wc_font->pf_defaultsize.y + 5;
	    wc_xoff   = max (0, min (Fb_width, pw_rect.r_width) - wc_width
			    - TOOL_BORDERWIDTH);
	    wc_yoff   = max (0, pw_rect.r_height - cb_height - wc_height
			    - TOOL_BORDERWIDTH);
	}
}


/* FRAMELABEL -- Return a pointer to the frame label string for the current
 * frame.
 */
static char *
framelabel()
{
	char	fname[SZ_FNAME];
	char	label[SZ_LABEL*2];

	sprintf (fname, o_fname, df_p->fb_frameno, df_p->fb_imageno);
	sprintf (label, "[%d] %s: %s", df_p->fb_frameno, fname,
	    df_p->fb_ctran.imtitle);
	strncpy (df_p->fb_label, label, SZ_LABEL);

	return (df_p->fb_label);
}


/* GETFNAME -- Construct the pathname of a user datafile.  One optional
 * integer argument is permitted.
 */
static char *
getfname (rootname, arg)
char	*rootname;		/* root filename (printf style format) */
int	arg;
{
	static	char pathname[SZ_FNAME];
	char	fmt[SZ_LINE], *udir;

	/* Were we passed an absolute pathname as input? */
	if (*rootname == '/') {
	    strcpy (pathname, rootname);
	    return (pathname);
	}

	if ((udir = getenv ("WCSDIR")) == NULL)
	    if ((udir = getenv ("wcsdir")) == NULL)
		if ((udir = getenv ("HOME")) == NULL)
		    udir = "/tmp";

	sprintf (fmt, "%s/%s", udir, rootname);
	sprintf (pathname, fmt, arg);

	return (pathname);
}


/* GIO_SETCURSOR -- Set graphics frame cursor options.
 */
static
gio_setcursor (op1, op2)
int	op1, op2;
{
	Cursor	cursor;
	int	option[2], i;
	int	blink=cursor_blink, show=cursor_show;

	/* Normalize the argument list. */
	for (option[0]=op1, option[1]=op2, i=0;  i < 2;  i++)
	    switch (option[i]) {
	    case BLINK_OFF:
	    case BLINK_ON:
		blink = option[i];
		break;
	    case CURSOR_OFF:
	    case CURSOR_ON:
		show = option[i];
		break;
	    }

	/* Do we need to change anything? */
	if (blink == cursor_blink && show == cursor_show)
	    return;

	/* Modify the cursor attributes. */
	if (show == CURSOR_ON && reading_imcursor)
	    gio_readcursor (imcursor_wcs);
	else {
	    cursor = window_get (gio_canvas, WIN_CURSOR);
	    cursor_set (cursor,
		CURSOR_SHOW_CURSOR, FALSE,
		CURSOR_SHOW_CROSSHAIRS, (show == CURSOR_ON),
		CURSOR_CROSSHAIR_THICKNESS, 1,
		CURSOR_CROSSHAIR_LENGTH, 20,
		CURSOR_CROSSHAIR_GAP, 6,

#ifdef sparc
		/* This is a kludge to work around a bug with the
		 * sparcstation 1 under 4.0.3. */
		CURSOR_CROSSHAIR_OP, PIX_SRC ^ PIX_DST,
#else
		CURSOR_CROSSHAIR_OP, PIX_SRC,
#endif

		CURSOR_CROSSHAIR_COLOR, CMS_CURSOR,
		0);
	    window_set (gio_canvas, WIN_CURSOR, cursor, 0);
	}

	cursor_blink = blink;
	cursor_show = show;
}


/* GIO_SETCURSORPOS -- Set the position of the graphics cursor within the
 * graphics frame.
 */
static
gio_setcursorpos (x, y)
int	x, y;			/* pixwin pixel coords */
{
	if (window_open)
	    window_set (gio_canvas, WIN_MOUSE_XY, last_bx=x, last_by=y, 0);
}


/* RESET_PROC -- Called from the setup panel to reset the state of the
 * display.
 */
static
reset_proc()
{
	register struct pixrect *pr = get_screen_rect();
	register struct framebuf *fb;
	register int	i;

	stop_pan();
	blink = 0;
	display_coords = 0;
	setup_xoff = 4;
	setup_yoff = 18;
	wc_xoff = wc_yoff = 0;
	state = TRACK_CURSOR;

	for (i=0;  i < fb_nframes;  i++) {
	    fb = &frames[i];
	    fb->fb_xoff = 0;
	    fb->fb_yoff = 0;
	    fb->fb_xzoom = fb->fb_yzoom = 1;
	    fb->fb_center = fb_ngrey / 2.0;
	    fb->fb_slope = (float)white / (float)(fb_ngrey - 1);
	    fb->fb_maptype = MONO;
	    fb->fb_objno = 1;
	    fb->fb_imageno = 0;
	    fb->fb_frameno = i + 1;
	}

	gio_xsize = initial_gio_xsize;
	gio_ysize = initial_gio_ysize;

	gio_xsize = min (pr->pr_width - TOOL_BORDERWIDTH * 2, gio_xsize);
	gio_ysize = min (pr->pr_height
	    - tool_headerheight ((int)window_get(gio_frame,FRAME_SHOW_LABEL))
	    - TOOL_BORDERWIDTH, gio_ysize);

	window_set (gio_canvas,
	    WIN_WIDTH,	gio_xsize,
	    WIN_HEIGHT,	gio_ysize,
	    0);

	window_fit (gio_canvas);
	window_fit (gio_frame);

	set_frame (1);
}


/* MAPRECT -- Compute the intersection of the given subrect of the first rect
 * with the second rect, in the coordinate system of the second.  The rects
 * are defined in screen coordinates, the subrects relative to their parent
 * rects.
 */
maprect (r1, s1, r2, s2)
BRect	*r1, *s1;		/* source rect and subrect	*/
BRect	*r2, *s2;		/* destination rect and subrect	*/
{
	int	xoff, yoff;
	int	x0, y0, x1, y1;

	/* Compute offset of second rect from the first. */
	xoff = r2->r_left - r1->r_left;
	yoff = r2->r_top  - r1->r_top;

	/* Translate the first subrect into the coordinate system of the
	 * second rect.
	 */
	x0 = s1->r_left - xoff;
	y0 = s1->r_top  - yoff;
	x1 = x0 + s1->r_width - 1;
	y1 = y0 + s1->r_height - 1;

	/* Does the new subrect totally miss the second rect?
	 */
	if (x1 < 0 || x0 >= r2->r_width || y1 < 0 || y0 >= r2->r_height) {
	    *s2 = *r2;
	    s2->r_width = s2->r_height = 0;
	} else {
	    /* Clip the new subrect to the boundary of the second rect.
	     */
	    if (x0 < 0)
		x0 = 0;
	    else if (x0 >= r2->r_width)
		x0 = r2->r_width - 1;

	    if (x1 < 0)
		x1 = 0;
	    else if (x1 >= r2->r_width)
		x1 = r2->r_width - 1;

	    if (y0 < 0)
		y0 = 0;
	    else if (y0 >= r2->r_height)
		y0 = r2->r_height - 1;

	    if (y1 < 0)
		y1 = 0;
	    else if (y1 >= r2->r_height)
		y1 = r2->r_height - 1;

	    /* Compute the new subrect.
	     */
	    s2->r_left    = x0;
	    s2->r_top     = y0;
	    s2->r_width   = x1 - x0 + 1;
	    s2->r_height  = y1 - y0 + 1;
	}

	return (s2->r_width > 0 && s2->r_height > 0);
}


/* BPW_GET_REGION_RECT -- Get pw_rect, transforming a Rect to a BRect.
 */
static
Bpw_get_region_rect (pw, br)
Pixwin	*pw;
BRect	*br;
{
	Rect	r;

	pw_get_region_rect (pw, &r);
	br->r_left   = r.r_left;
	br->r_top    = r.r_top;
	br->r_width  = r.r_width;
	br->r_height = r.r_height;
}


/* BPW_LOCK -- Lock a big pixwin.
 */
static
Bpw_lock (pw, br)
Pixwin	*pw;
BRect	*br;
{
	Rect	r;

	r.r_left   = br->r_left;
	r.r_top    = br->r_top;
	r.r_width  = br->r_width;
	r.r_height = br->r_height;

	pw_lock (pw, &r);
}


/* BPW_UNLOCK -- Unlock a big pixwin.
 */
static
Bpw_unlock (pw)
Pixwin	*pw;
{
	pw_unlock (pw);
}


/* IMAGECOPY_PROC -- Make a hardcopy of the image window on the laserwriter.
 * We don't do this immediately, but rather after a delay of a few milliseconds
 * to allow the window system to restore the imtool window lookup table after
 * the mouse button is released.
 */
static
imagecopy_proc()
{
	static	Notify_value ev_screendump();

	window_set (gio_frame, WIN_SHOW, TRUE, 0);
	imt_pause (100, ev_screendump);
}


/* EV_SCREENDUMP -- Called after the specified interval has passed to carry out
 * the actual screendump operation.
 */
static Notify_value
ev_screendump()
{
	int	depth = 8;

	edit_colormap();

	if (snap_frame_too) {
	    screendump (
		(int) window_get (gio_canvas, WIN_FD),
		win_get_pixwin (gio_canvas),
		(int) window_get (gio_frame, WIN_WIDTH),
		(int) window_get (gio_frame, WIN_HEIGHT),
		(int) window_get (gio_frame, WIN_X),
		(int) window_get (gio_frame, WIN_Y),
		depth);
	} else {
	    screendump (
		(int) window_get (gio_canvas, WIN_FD),
		win_get_pixwin (gio_canvas),
		(int) window_get (gio_frame, WIN_WIDTH) - TOOL_BORDERWIDTH * 2,
		(int) window_get (gio_frame, WIN_HEIGHT) -
		    HEIGHTADJUST - cb_height,
		(int) window_get (gio_frame, WIN_X) + TOOL_BORDERWIDTH,
		(int) window_get (gio_frame, WIN_Y) + HEIGHTADJUST -
		    TOOL_BORDERWIDTH,
		depth);
	}

	return (NOTIFY_DONE);
}


/* IMT_PAUSE -- Suspend output for the indicated number of milliseconds, to
 * allow other event processing to catch up.
 */
imt_pause (msec, ufcn)
int	msec;
Notify_value (*ufcn)();
{
	static	struct itimerval itimer_delay;

	itimer_delay.it_interval.tv_usec = 0;
	itimer_delay.it_interval.tv_sec  = 0;
	itimer_delay.it_value.tv_usec = (msec % 1000) * 1000;
	itimer_delay.it_value.tv_sec  = (msec / 1000);

	notify_set_itimer_func ((int)ufcn, ufcn, ITIMER_REAL,
	    &itimer_delay, NULL);
}


/* PRINT_USAGE -- Print instructions on how to use this window tool.
 */
static
print_usage (toolname)
char	*toolname;
{
	printf ("no on-line help text yet for IMTOOL\n");
}
