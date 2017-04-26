#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Obm.h>
#include <ObmW/Gterm.h>
#include "ximtool.h"
#include "iis.h"

/*  Slackware/RedHat4.2 compatibility hack. */
#if defined(linux) && defined(isalnum)
#undef isalnum
#define isalnum(c) (isalpha(c)||isdigit(c))
#endif


#define	DBG_RASTER	0


/*
 * RASTER.C -- Raster pixel (frame buffer) routines.  These are the routines
 * which create and manipulate the frame buffers and the graphics pipeline
 * to the screen.
 *
 *	     xim_initialize (xim, config, nframes, hardreset)
 *		  xim_reset (xim, w)
 *	         xim_resize (xim, w)
 *	        xim_refresh (xim)
 *
 *	       xim_setFrame (xim, frame)
 *    xim_setReferenceFrame (chan, frame)
 *	xim_setDisplayFrame (xim, frame)
 *	      xim_initFrame (xim, frame, nframes, config, memModel)
 *	       xim_delFrame (xim, frame)
 *	    xim_matchFrames (xim, frames, reference_frame)
 *	 xim_registerFrames (xim, frames, reference_frame)
 *	       xim_fitFrame (xim)
 *	     xim_tileFrames (xim, frame_list)
 *
 *	     xim_cursorMode (xim, state)
 *
 *	     xim_setMapping (xim, fb, frame, mapping, src, dst, fill_mode)
 *		xim_setZoom (xim, fb, frame, mapping, src, dst,
 *			     xcen,ycen, xmag,ymag, xoff,yoff, absolute)
 *		xim_setFlip (xim, fb, flip_x, flip_y)
 *	    xim_setColormap (name, dirs, red, green, blue, ncolors)
 *		 xim_setRop (xim, fb, rop)
 *   rop = xim_getAntialias (xim, s)
 *
 *	      xim_getScreen (xim, frame, sx, sy, width, height, depth)
 *      bool = xim_onScreen (xim, frame)
 *
 *         xim_setCursorPos (xim, sx, sy)
 *         xim_getCursorPos (xim, sx, sy, raster, frame)
 *
 * pixels = xim_readDisplay (xim, x0,y0,nx,ny, w,h, r,g,b, ncolors)
 *  stat = xim_writeDisplay (xim, frame, mapname, pixels, w,h, r,g,b, ncolors)
 *
 *		xim_message (xim, object, message)
 *		   xim_msgi (xim, object, intval)
 *		  xim_alert (xim, text, ok_action, cancel_action)
 */

/* Define some builtin colormaps. */
static Lut aips0 = {
#include "data/aips0.lut"
};
static Lut blue = {
#include "data/blue.lut"
};
static Lut color = {
#include "data/color.lut"
};
static Lut green = {
#include "data/green.lut"
};
static Lut halley = {
#include "data/halley.lut"
};
static Lut heat = {
#include "data/heat.lut"
};
static Lut rainbow = {
#include "data/rainbow.lut"
};
static Lut red = {
#include "data/red.lut"
};
static Lut staircase = {
#include "data/staircase.lut"
};
static Lut standard = {
#include "data/standard.lut"
};

static void get_fbconfig();
static void set_colorbar();
static int get_dirfile();
static void load_testpattern();
static void set_nframes();
static void xim_frameRegion();
static void xim_colortables();
extern char *getenv();
double strtod();

#define	TOL	0.0001


/* XIM_INITIALIZE -- Initialize the imaging subsystem.  Read the config file
 * and create the frame buffers, mappings, and colormaps.
 */
void
xim_initialize (xim, config, nframes, hardreset)
register XimDataPtr xim;
int config;
int nframes;
int hardreset;
{
	register Widget gt = xim->gt;
	register FrameBufPtr fb;
	register int i;

	unsigned short m_red[MAX_COLORS];
	unsigned short m_green[MAX_COLORS];
	unsigned short m_blue[MAX_COLORS];
	char *fname, *ip, *op, sbuf[8192];
	String maps[MAX_COLORMAPS], dirs[4];
	int max_cmaps, first, ngray, rgb_len, nfiles, n;
	static int nbuiltin_cmaps = 0;
	int display_frame = 1;
	char cmapname[SZ_NAME];
	char buf[SZ_LINE];
	FbConfigPtr cf;
	ColorMapPtr cm;
	struct dir *dp;
	int startup;
	DIR *dir;

	/* The gterm widget handle should already have been set by the
	 * GUI during startup.
	 */
	if (!gt) {
	    fprintf (stderr, "xim_initialize:  no gterm-image widget!!\n");
	    exit (1);
	}

	/* Inform the GUI that the frame buffers are being initialized.
	 * The initialize parameter is set to "startup" during program
	 * startup, "restart" at the start of an initialization during
	 * execution, and "done" when initialization is completed.
	 */
	startup = (xim->nframes == 0);
	xim_message (xim, "initialize", startup ? "startup" : "restart");

	if (hardreset || startup || config != xim->fb_configno) {
	    xim_msgi (xim, "frame", 0);
	    xim_message (xim, "frameTitle", "");
	    set_nframes (xim, 0);
	    cm = &colormaps[DEF_COLORMAP-1];
	    strcpy (cmapname, cm->name);
	    hardreset = 1;

	    /* Get frame buffer configurations. */
	    get_fbconfig (xim);

	    /* Remove any existing border highlight marker. */
	    if (xim->gm_border) {
		GmDestroy (xim->gm_border);
		xim->gm_border = NULL;
	    }

	    /* Initialize the frame buffers. */
	    if (DBG_RASTER)
		fprintf (stderr, "xim_initialize: Raster init.....\n");
	    GtRasterInit (gt);
	    if (DBG_RASTER)
		fprintf (stderr, "xim_initialize: Raster init.....DONE\n");
	    for (i=1;  i <= MAX_FRAMES;  i++)
		xim->frames[i].frameno = 0;

	    cf = &xim->fb_config[config-1];
	    nframes = (nframes <= 0) ? cf->nframes : nframes;

	    if (startup) {
		/* Set initial tile frames list to be all frames if starting
		 * up in tile frames mode.
		 */
		if (xim->tileFrames && nframes > 1) {
		    xim->tileFramesList = 0;
		    xim->nTileFrames = 0;
		    for (i=0;  i < nframes;  i++) {
			xim->tileFramesList |= (1 << i);
			xim->nTileFrames++;
		    }
		}
	    } else {
		xim->display_frame = 0;
		xim->df_p = NULL;
		for (i=0;  i < XtNumber(xim->chan);  i++) {
		    IoChanPtr chan = &xim->chan[i];
		    if (chan->type)
			xim_setReferenceFrame (chan, 1);
		}
		if (xim->cursor_chan)
		    xim->cursor_chan = &xim->chan[0];
	    }

	    xim->rop = xim->antialias ?
		xim_getAntialias (xim, xim->antialiasType) : 0x00;
	    xim->fb_configno = config = max(1, min(MAX_FBCONFIG, config));
	    if (DBG_RASTER)
		fprintf (stderr,
		    "xim_initialize: ........................SETNFRAME\n");
	    set_nframes (xim, nframes);
	    if (DBG_RASTER)
		fprintf (stderr,
		    "xim_initialize: ........................SETNFRAME DONE\n");

	    /* Initialize the tile framing options. */
	    xim->tileByRows = 1;
	    xim->tileTopDown = 1;
	    xim->tileLabels = 0;

	    /* Set the new frame size. */
	    sprintf (buf, "%d %d %d", cf->width, cf->height, 8);
	    xim_message (xim, "frameSize", buf);

	    /* Create the frames. */
	    for (i=1;  i <= nframes;  i++) {
		if (DBG_RASTER)
		    fprintf (stderr,
			"xim_initialize: ...............INITFRAME %d\n",i);
		xim_initFrame (xim, i, nframes, cf, xim->memModel);
		if (DBG_RASTER)
		    fprintf (stderr,
			"xim_initialize: ...............INITFRAME %d DONE\n",i);
	    }

            /* Reinitialize the tile framing. */
            if (xim->tileFrames) {
                xim->tileFramesList = 0;
                xim->nTileFrames = 0;
                for (i=0;  i < nframes;  i++) {
                    xim->tileFramesList |= (1 << i);
                    xim->nTileFrames++;
                }
            }

	    xim->width = cf->width;
	    xim->height = cf->height;
	    GtSetLogRes (gt, cf->width, cf->height);

	} else {
	    /* Soft reset. */
	    display_frame = xim->display_frame;
	    fb = &xim->frames[display_frame-1];
	    cm = &colormaps[fb->colormap-1];
	    strcpy (cmapname, cm->name);

	    for (i=0;  i < ncolormaps;  i++) {
		cm = &colormaps[i];
		GtFreeColormap (gt, cm->mapno);
	    }
	}

	if (DBG_RASTER)
	    fprintf (stderr,
		"xim_initialize: ........................CMAP SETUP\n");
	/* Set up the colormap to emulate the IIS/imtool colormap.  Set
	 * xim->ncolors to the length of the grayscale region of the colormap
	 * returned by iiscolormap so that our other colormaps will have the
	 * same size.
	 */
	if (DBG_RASTER)
	    fprintf (stderr,
		"xim_initialize: xim->ncolors = %d\n", xim->ncolors);
	xim_iiscolormap (gt, m_red,m_green,m_blue, &first, &ngray, &rgb_len);
	if (DBG_RASTER)
	    fprintf (stderr,
		"xim_initialize: writeColormap:  first=%d ngray=%d len=%d\n",
		first, ngray, rgb_len);
	GtWriteColormap (gt, 0, first, rgb_len, m_red, m_green, m_blue);
	if ((xim->ncolors = ngray) < 0) {
	    fprintf (stderr, "ERROR: No colormap cells available.\n");
	    exit (1);
	}


	/* Initialize the color tables.
	*/
	max_cmaps = nbuiltin_cmaps ? nbuiltin_cmaps : MAX_COLORMAPS;
	if (DBG_RASTER)
	    fprintf (stderr,
		"xim_initialize: init colortables:  xim->ncolors = %d\n",
		xim->ncolors);
	for (ncolormaps=0;  ncolormaps < max_cmaps;  ncolormaps++) {
	    cm = &colormaps[ncolormaps];
	    if (!cm->name[0])
		break;
	    cm->mapno = GtNextColormap (gt);
	    xim_setColormap (cm->name, NULL, m_red, m_green, m_blue,
		xim->ncolors);
	    GtWriteColormap (gt, cm->mapno, first, xim->ncolors,
		m_red, m_green, m_blue);
	}
	if (DBG_RASTER)
	    fprintf (stderr, "xim_initialize: init colortables:  DONE\n");

	/* The first time this is called count the number of builtin cmaps. */
	if (nbuiltin_cmaps == 0)
	    nbuiltin_cmaps = ncolormaps;

	/* Get the names of the two user defined colormaps, if any. */
	nfiles = 0;
	maps[nfiles++] = xim->userCMap1;
	maps[nfiles++] = xim->userCMap2;

	/* If any user colormap directories are specified get the names
	 * of the colormap files in these directories.
	 */
	dirs[0] = "./";
	dirs[1] = xim->userCMapDir1;
	dirs[2] = xim->userCMapDir2;
	dirs[3] = NULL;

	for (i=1, op=sbuf;  dirs[i];  i++) {
	    if (strcmp (dirs[i], "none") != 0) {
		if (dir = opendir (dirs[i])) {
		    while ((n = get_dirfile (dir, op, SZ_FNAME)) > 0) {
			maps[nfiles++] = op;
			op += n + 1;
			if (nfiles >= MAX_COLORMAPS)
			    break;
		    }
		    closedir (dir);
		}
	    }
	}

	/* Add all the user specified colormaps to the colormap table.
	 */
	for (i=0;  i < nfiles && ncolormaps < MAX_COLORMAPS;  i++) {
	    fname = maps[i];
	    if (strcmp (fname, "none") != 0) {
		if (xim_setColormap (fname, dirs,
		    m_red, m_green, m_blue, xim->ncolors) == OK) {

		    cm = &colormaps[ncolormaps++];
		    cm->mapno = GtNextColormap (gt);

		    /* Use root portion of filename as the colormap name. */
		    for (ip=fname;  *ip;  ip++)
			if (*ip == '/')
			    fname = ip + 1;
		    for (ip=fname, op=cm->name;  *ip && *ip != '.';  ip++) {
			*op++ = *ip;
			if (op - cm->name >= SZ_CMAPNAME)
			    break;
		    }
		    *op = '\0';

		    GtWriteColormap (gt, cm->mapno, first, xim->ncolors,
			m_red, m_green, m_blue);
		}
	    }
	}

	/* Pass the list of colortables on to the user interface. */
	xim_colortables (xim);

	/* Set initial display frame. */
	xim_setDisplayFrame (xim, display_frame);

	/* Force the initial default colormap to be loaded. */
	fb = &xim->frames[display_frame-1];
	for (i=0;  i < ncolormaps;  i++) {
	    cm = &colormaps[i];
	    if (strcmp (cm->name, cmapname) == 0)
		break;
	}
	if (i >= ncolormaps)
	    cm = &colormaps[DEF_COLORMAP-1];

	GtLoadColormap (gt, cm->mapno, fb->offset, fb->scale);
	xim_enhancement (xim, fb);

	/* Set options. */
/*	xim_message (xim, "cmfocus", xim->cm_focus ? "True" : "False");*/
	xim_message (xim, "autoscale", xim->autoscale ? "True" : "False");
	xim_message (xim, "antialias", xim->antialias ? "True" : "False");
	xim_message (xim, "tileFrames", xim->tileFrames ? "True" : "False");

	xim_message (xim, "initialize", "done");

	if (DBG_RASTER)
	    fprintf (stderr,
		"xim_initialize: ................................RETURNING\n");
}


/* XIM_RESET -- Called when the active gterm widget is set or changed.
 */
void
xim_reset (xim, w)
XimDataPtr xim;
Widget w;
{
	unsigned short iomap[MAX_COLORS];
	int iomap_len;

	if (!w)
	    return;

	/* The following assumes that the GUI does a setGterm first for the
	 * colorbar and second for the main image window.  The first thing we
	 * do is set the iomap so that all ximtool color and pixel i/o will
	 * see the emulated IIS color model.
	 */
	if (xim->cb == NULL) {
	    xim->cb = w;
	    xim_iisiomap (w, iomap, &iomap_len);
	    GtPostResizeProc (w, set_colorbar, xim);
	    GtWriteIomap (w, iomap, 0, iomap_len);
	} else {
	    xim->gt = w;
	    xim_iisiomap (w, iomap, &iomap_len);
	    GtPostResizeProc (w, xim_resize, xim);
	    GtWriteIomap (w, iomap, 0, iomap_len);
	}
}


/* XIM_RESIZE -- Called when the active gterm widget is resized.  We need to
 * resize the zoom raster and modify the zoom mapping for each frame to
 * reflect the new window size.
 */
void
xim_resize (xim, w)
XimDataPtr xim;
Widget w;
{
	register FrameBufPtr fb;
	int junk, sx, sy, width, height, depth;
	int i, active, frame, mapping, zoomtype;
	float xscale, yscale, scale;
	char buf[SZ_LINE];


	/* This case is true during startup. */
	if (xim->nframes <= 0)
	    return;

	/* Get new screen size.  In tile frame mode select the first frame
	 * in the list for the size, which may not always be frame 1.
	 */
	if (xim->tileFrames && !(xim->tileFramesList & 1)) {
	    if (xim->nframes == 0)
		return;
	    for (i=1;  i <= xim->nframes;  i++)
                if (xim->tileFramesList & (1 << (i-1))) {
	    	    xim_getScreen (xim, i, &sx, &sy, &width, &height, &depth);
		    break;
		}
	} else
	    xim_getScreen (xim, 1, &sx, &sy, &width, &height, &depth);

	/* Compute the new scale factor required to scale the source to the
	 * destination at magnification 1.0.
	 */
	if (xim->autoscale) {
	    xscale = (float)width / (float)xim->width;
	    yscale = (float)height / (float)xim->height;
	    scale = min (xscale, yscale);
	} else {
	    xscale = 1.0;
	    yscale = 1.0;
	    scale = 1.0;
	}

	/* Adjust the mapping for each frame. */
	for (frame=1;  frame <= xim->nframes;  frame++) {
	    fb = &xim->frames[frame-1];
	    mapping = fb->zoommap;

	    /* For rapid frame blink, split screen, etc. the zoom raster
	     * must be the same size as the screen so we must resize this
	     * raster when the display window is resized.  The CreateRaster
	     * call will delete the old raster and all its mappings and
	     * create a new one.  We must then restore the mappings, and
	     * if this is an active display frame, refresh the mapping.
	     */
	    active = xim_onScreen (xim, frame);

	    if (fb->zoomras) {
		GtQueryRaster (w, fb->zoomras, &zoomtype, &junk, &junk, &junk);
		GtCreateRaster (w, fb->zoomras, zoomtype, width, height, depth);
		xim_setMapping (xim, NULL, frame, fb->dispmap,
		    fb->zoomras, 0, M_FILL);
		if (!active) {
		    GtDisableMapping (w, fb->dispmap, 0);
		    xim_setMapping (xim, NULL, frame, fb->zoommap,
			fb->raster, fb->zoomras, M_FILL);
		    GtDisableMapping (w, fb->zoommap, 0);
		}
	    } else {
		if (active) {
		    GtEnableMapping (w, fb->dispmap, 0);
		    GtSetDisplayRaster (w, xim->display_frame);
		} else
		    GtDisableMapping (w, fb->dispmap, 0);
	    }

	    /* Set the new mapping. */
	    fb->xscale = fb->yscale = scale;
	    xim_setZoom (xim, fb, frame, mapping, fb->raster, fb->zoomras,
		fb->xcen, fb->ycen, fb->xmag, fb->ymag, 
		fb->xoff, fb->yoff, False);
	}

	/* Refresh the screen for any active mappings.  Any mappings
	 * defined on any visible frame should be refreshed, since a
	 * redraw clears the display window.
	 */
	for (frame=1;  frame <= xim->nframes;  frame++) {
	    if (xim_onScreen (xim, frame)) {
		int junk, width, height, depth;
		fb = &xim->frames[frame-1];
		GtQueryRaster (w, fb->raster, &junk, &width, &height, &depth);
		GtRefreshPixels (w, fb->raster, GtPixel, 0, 0, width, height);
	    }
	}

	/* Highlight the current frame. */
	xim_highlightFrame (xim, xim->display_frame);

	sprintf (buf, "%d %d %d %d", sx, sy, width, height);
	xim_message (xim, "resize", buf);
}


/* XIM_REFRESH -- Refresh the current display frame.
 */
void
xim_refresh (xim)
XimDataPtr xim;
{
	register FrameBufPtr fb = xim->df_p;
	int junk, width, height, depth;

	GtQueryRaster (xim->gt, fb->raster, &junk, &width, &height, &depth);
	GtRefreshPixels (xim->gt, fb->raster, GtPixel, 0, 0, width, height);
}


/* XIM_CLOSE -- Free any raster specific resources.
 */
void
xim_close (xim)
register XimDataPtr xim;
{
}


/* XIM_SETFRAME -- Configure a frame to be both the display and reference
 * frame.
 */
void
xim_setFrame (xim, frame)
register XimDataPtr xim;
int frame;
{
	xim_setDisplayFrame (xim, frame);
}


/* XIM_SETREFERENCEFRAME -- Set the frame used for frame buffer i/o.
 */
void
xim_setReferenceFrame (chan, frame)
register IoChanPtr chan;
int frame;
{
	register XimDataPtr xim = (XimDataPtr) chan->xim;
	register FrameBufPtr fb;
	int frameno;

	/* Ignore request if channel not active. */
	if (!chan->type)
	    return;

	frameno = max(1, min(MAX_FRAMES, frame));
	fb = &xim->frames[frameno-1];

	/* Ignore request if not a valid frame. */
	if (fb->frameno > 0) {
	    chan->reference_frame = frameno;
	    chan->rf_p = fb;
	}
}


/* XIM_SETDISPLAYFRAME -- Set the frame which is displayed.
 */
void
xim_setDisplayFrame (xim, frame)
register XimDataPtr xim;
int frame;
{
	register FrameBufPtr fb;
	register Widget gt = xim->gt;
	FrameBufPtr old_fb = xim->df_p;
	int frameno, old_frameno;
	char buf[256];

	old_frameno = old_fb ? old_fb->frameno : 0;
	frameno = max(1, min(MAX_FRAMES, frame));
	fb = &xim->frames[frameno-1];

	/* Ignore request if not a valid frame. */
	if (fb->frameno > 0 && (!old_fb || frameno != old_frameno)) {
	    /* Special case: if tile frame mode is in effect and we are
	     * displaying a frame not in the tile list, clear the screen
	     * before displaying the frame.  Likewise, if displaying an
	     * untiled frame while in tile frame mode and then reverting
	     * to display of a tiled frame, clear the screen and display
	     * all the tiled frames.
	     */
	    if (xim->tileFrames && old_fb) {
		int tile_old = (xim->tileFramesList & (1 << old_frameno-1));
		int tile_new = (xim->tileFramesList & (1 <<     frameno-1));

		if (tile_old && !tile_new)
		    GtClearScreen (gt);
		else if (!tile_old && tile_new) {
		    GtClearScreen (gt);
/*		    xim_resize (xim, gt);*/
		}
	    }

	    /* Map the new frame. */
	    xim->df_p = fb;
	    xim->display_frame = frameno;
	    GtEnableMapping (gt, fb->dispmap, 0);

	    /* Unmap the old frame. */
	    if (old_fb && !xim_onScreen (xim, old_frameno)) {
		FrameBufPtr old_fb = &xim->frames[old_frameno-1];
		GtDisableMapping (gt, old_fb->dispmap, 0);
	    }

	    /* Display the new frame. */
	    if (GtActiveMapping (gt, fb->zoommap)) {
		GtRefreshMapping (gt, fb->dispmap);
	    } else {
		GtEnableMapping (gt, fb->zoommap, 0);
		GtRefreshMapping (gt, fb->zoommap);
	    }
	    GtSetDisplayRaster (gt, frameno);


	    /* Highlight the new frame if in tileFrames mode. */
	    xim_highlightFrame (xim, frameno);

	    /* Load the colormap only if it differs from what we already
	     * have.  This avoids the waste of repeatedly reloading the
	     * colormap when blinking frames that have matched colormaps.
	     */
	    if (!old_fb || fb->colormap != old_fb->colormap ||
		abs (fb->offset - old_fb->offset) > 0.001 ||
		abs (fb->scale - old_fb->scale) > 0.001) {

	    	GtSetColormapFocus (-1);	/* force full update	*/
		GtLoadColormap (gt, fb->colormap, fb->offset, fb->scale);
	    	GtSetColormapFocus (xim->cm_focus);
	    }

	    xim_msgi (xim, "frame", frameno);
	    xim_message (xim, "frameTitle", fb->ctran.imtitle);
	    xim_message (xim, "xflip", fb->xflip ? "true" : "false");
	    xim_message (xim, "yflip", fb->yflip ? "true" : "false");
	    xim_frameRegion (xim, fb);

	    sprintf (buf, "%g %g %g %g %g %g %g %g",
		fb->xmag, fb->ymag, fb->xcen, fb->ycen, 
		fb->xscale, fb->yscale, fb->xoff, fb->yoff);
	    xim_message (xim, "frameView", buf);
	}
}


/* XIM_INITFRAME -- Initialize a frame buffer.
 */
void
xim_initFrame (xim, frame, nframes, config, memModel)
register XimDataPtr xim;
int frame, nframes;
FbConfigPtr config;
char *memModel;
{
	register FrameBufPtr fb = &xim->frames[frame-1];
	register Widget gt = xim->gt;
	int sx, sy, width, height, depth;
	char buf[SZ_LINE];

	if (frame < 1 || frame > MAX_FRAMES)
	    return;

	xim_delFrame (xim, frame);
	memset ((void *)fb, 0, sizeof(FrameBuf));

	/* Create the frame buffer. */
	fb->frameno = frame;
	fb->raster = GtNextRaster (gt);

	if (DBG_RASTER) {
	    fprintf (stderr, 
  		"xim_initFrame: Creating CLIENT raster for frame=%d raster=%d (%dx%dx%d)\n",
  		frame, fb->raster, 
  		config->width, config->height, DEF_FRAME_DEPTH);
	    fprintf (stderr, 
  		"xim_initFrame: Creating memModel=%s frame=%d raster=%d  (%d x %d)\n", 
  		memModel, frame, fb->raster, config->width, config->height);
	}

	if (GtCreateRaster (gt, fb->raster, GtClient,
		config->width, config->height, DEF_FRAME_DEPTH) < 0) {

	    	    fprintf (stderr, "cannot create %dx%d frame buffer #%d\n",
			config->width, config->height, frame);
	    return;
	}

	/* Get region of screen to be written into. */
	xim_getScreen (xim, frame, &sx, &sy, &width, &height, &depth);

	if (DBG_RASTER)
	    fprintf (stderr,
		"xim_initFrame: [%d of %d] screen size -- %d x %d x %d\n",
		frame, nframes, width, height, depth);

	/* Set up the graphics pipeline to the screen.  The "memory model"
	 * determines memory is used, trading off memory usage against speed
	 * for operations like zoom/pan and blink.
	 */

	if (strcmp (memModel, "fast") == 0) {
	    /* Use both client and server memory to achieve the best
	     * possible performance.  The frame buffer raster (type ximage)
	     * is mapped to a server pixmap with the zoom/pan mapping, and
	     * the server pixmap is mapped to the screen with the one-to-one
	     * frame display mapping.
	     */
fast:	    fb->zoomras = GtNextRaster (gt);
	    if (GtCreateRaster (gt,
		    fb->zoomras, GtServer, width, height, depth) < 0)
		goto nice;

	    xim_setMapping (xim, fb, frame, fb->zoommap = GtNextMapping(gt),
		fb->raster, fb->zoomras, xim->autoscale ? M_ASPECT : M_UNITARY);
	    xim_setMapping (xim, fb, frame, fb->dispmap = GtNextMapping(gt),
		fb->zoomras, 0, M_FILL);

	} else if (strcmp (memModel, "beNiceToServer") == 0) {
	    /* Minimize use of X server memory, but keep zoom raster to speed
	     * up blinks.  Everything is the same as with the "fast" memory
	     * model except that the zoom raster is an ximage rather than
	     * server pixmap raster.
	     */
nice: 	    fb->zoomras = GtNextRaster (gt);
	    if (DBG_RASTER)
		fprintf (stderr,
		    "xim_initFrame: creating 'nice' model (0x%x)\n", 
		    fb->zoomras);
	    if (GtCreateRaster (gt,
		    fb->zoomras, GtClient, width, height, depth) < 0)
		goto small;

	    xim_setMapping (xim, fb, frame, fb->zoommap = GtNextMapping(gt),
		fb->raster, fb->zoomras, xim->autoscale ? M_ASPECT : M_UNITARY);
	    xim_setMapping (xim, fb, frame, fb->dispmap = GtNextMapping(gt),
		fb->zoomras, 0, M_FILL);

	} else if (strcmp (memModel, "small") == 0) {
	    /* Minimize both client and server memory as far as possible.
	     * Only the frame buffer raster is used, the zoom/pan mapping
	     * maps directly to the screen and is also used as the screen
	     * mapping.
	     *
	     * FIXME:  On TrueColor visuals we can have an issue here where
	     * the display pixmap is 24-bits and various things (e.g. pixel
	     * readback) are expecting 8-bits.
	     */
small:	    fb->zoomras = 0;
	    if (DBG_RASTER)
		fprintf (stderr,
		    "xim_initFrame: creating 'small' model (0x%x)\n", 
		    fb->zoomras);

	    xim_setMapping (xim, fb, frame, fb->zoommap = GtNextMapping(gt),
		fb->raster, fb->zoomras, xim->autoscale ? M_ASPECT : M_UNITARY);
	    fb->dispmap = fb->zoommap;

	} else {
	    fprintf (stderr,
		"unrecognized memModel `%s', default to `fast'\n", memModel);
	    goto fast;
	}

	/* Set up the default colormap. */
	fb->colormap = DEF_COLORMAP;
	fb->offset = 0.5;
	fb->scale = (xim->invert ? -1.0 : 1.0);
	xim_enhancement (xim, fb);

	/* Disable frame display initially if not mapped to screen. */
	if (xim_onScreen (xim, frame)) {
	    GtEnableMapping (gt, fb->dispmap, 0);
	    GtSetDisplayRaster (gt, frame);
	} else
	    GtDisableMapping (gt, fb->dispmap, 0);

	set_nframes (xim, max (xim->nframes, frame));
}


/* XIM_DELFRAME -- Delete a frame.
 */
void
xim_delFrame (xim, frame)
register XimDataPtr xim;
int frame;
{
	register FrameBufPtr fb = &xim->frames[frame-1];
	register Widget gt = xim->gt;

	if (frame < 1 || frame > MAX_FRAMES)
	    return;
	if (fb->frameno <= 0)
	    return;

	GtFreeMapping (gt, fb->dispmap);
	if (fb->zoommap != fb->dispmap)
	    GtFreeMapping (gt, fb->zoommap);
	if (fb->zoomras)
	    GtDestroyRaster (gt, fb->zoomras);
	if (fb->raster)
	    GtDestroyRaster (gt, fb->raster);

	fb->frameno = 0;
	if (xim->nframes == frame)
	    set_nframes (xim, frame - 1);
}


/* XIM_ERASEFRAME -- Erase a frame.
 */
void
xim_eraseFrame (xim, frame)
register XimDataPtr xim;
int frame;
{
	FrameBufPtr fb = &xim->frames[frame-1];
	Widget gt = xim->gt;
	int    Z = 0;

	GtSetPixels (gt, fb->raster, GtPixel, Z,Z,Z,Z, CMS_BACKGROUND, 0);
}


/* XIM_FITFRAME -- Attempt to resize the display window to be the same
 * size as the frame buffer.
 */
void
xim_fitFrame (xim)
register XimDataPtr xim;
{
	register Widget gt = xim->gt;

	GtCreateRaster (gt, 0, GtServer, xim->width, xim->height, 8);
}


/* XIM_TILEFRAMES -- Set or clear tile frame mode.
 */
void
xim_tileFrames (xim, frame_list)
register XimDataPtr xim;
int frame_list;
{
	register int i;
	register Widget w = xim->gt;
	register FrameBufPtr fb;
	char buf[SZ_LINE];
	int mapping;


	/* Get list of frames to be tiled. */
	xim->tileFramesList = frame_list;
	for (i=0, xim->nTileFrames = 0;  i < xim->nframes;  i++)
	    if (frame_list & (1 << i))
		xim->nTileFrames++;

	/* Remove any existing border highlight marker. */
	if (xim->gm_border) {
	    GmDestroy (xim->gm_border);
	    xim->gm_border = NULL;
	}

	xim->tileFrames = (frame_list != 0);
	GtClearScreen (w);
	initialize_shadow_pixmap (w, 0);
	xim_resize (xim, w);

	/* Entering tile frame mode.
	 */
	if (xim->tileFrames) {
			
	    /* Ensure that the display frame is set to one of the
	     * tiled frames.
	     */
	    if (!(xim->tileFramesList & (1 << xim->display_frame-1))) {
		for (i=1;  i <= xim->nframes;  i++)
		    if (xim->tileFramesList & (1 << (i-1))) {
			xim->df_p = NULL;
			xim->display_frame = 0;
			xim_setFrame (xim, i);
			break;
		    }
	    }


	    /* Change the stacking order of the frame mappings if
	     * necessary so that any frames not in the tile list are 
	     * displayed above the tiled frames.  If this is not
	     * done the untiled frames may be obscured by the tiled
	     * frames.
	     */
	    for (i=0, mapping=0;  i < xim->nframes;  i++)
		if (xim->tileFramesList & (1 << i)) {
		    fb = &xim->frames[i];
		    if (!mapping)
			mapping = fb->dispmap;
		    else if (GtCompareMappings (w, mapping, fb->dispmap) < 0)
			mapping = fb->dispmap;
		}
	    for (i=0;  i < xim->nframes;  i++)
		if (!(xim->tileFramesList & (1 << i))) {
		    fb = &xim->frames[i];
		    GtRaiseMapping (w, fb->dispmap, mapping);
		}
	}

	/* Highlight the current frame. */
	xim_highlightFrame (xim, xim->display_frame);

	sprintf (buf, "%s", xim->tileFrames ? "True" : "False");
	xim_message (xim, "tileFrames", buf);
}


/* XIM_HIGHLIGHTFRAME -- If in tile frames mode, highlight the current frame
 * by coloring the border of the frame.
 */
xim_highlightFrame (xim, frame)
register XimDataPtr xim;
int frame;
{
	/* If we are tiling frames highlight the new display frame. */
	if (xim->gm_border) {
	    GmDestroy (xim->gm_border);
	    xim->gm_border = NULL;
	}

	if (xim->tileFrames && xim->highlightFrames) {
	    int sx, sy, width, height, depth;
	    int interactive, erase;
	    XtPointer gm;
	    Arg args[20];
	    int nargs = 0;

	    xim_getScreen (xim, frame, &sx, &sy, &width, &height, &depth);
	    if (sx > 0 && sy > 0) {
		gm = (XtPointer) GmCreate (xim->gt, Gm_Box,
		    interactive=False);

		XtSetArg (args[nargs], GmX, sx + (width-1)/2);        nargs++;
		XtSetArg (args[nargs], GmY, sy + (height-1)/2);       nargs++;
		XtSetArg (args[nargs], GmWidth, (width-1)/2+1);       nargs++;
		XtSetArg (args[nargs], GmHeight, (height-1)/2+1);     nargs++;
		XtSetArg (args[nargs], GmLineWidth, xim->tileBorder); nargs++;
		XtSetArg (args[nargs], GmSensitive, False);           nargs++;
		XtSetArg (args[nargs], GmVisible, True);              nargs++;
		XtSetArg (args[nargs], GmActivated, True);            nargs++;

		GmLower (gm, NULL);
		GmSetAttribute (gm, GmLineColor, xim->borderColor, XtRString);
		GmSetAttributes (gm, args, nargs, XtRInt);

		xim->gm_border = gm;
	    }
	    xim_labelTiles (xim);
	}
}


/* XIM_LABELTILES -- Label the tile with the frame number.
 */
xim_labelTiles (xim)
register XimDataPtr xim;
{
	FrameBufPtr  fb;
	MappingPtr   mp;
	Widget       gt = xim->gt;
	XtPointer    gm;
	Arg 	     args[10];
	char	     text[256], tw[16];
	register int i, j, len;
	int 	     sx, sy, width, height, depth, nargs=0;

	static XtPointer labels[MAX_FRAMES];
	static int label_init = 1;
	    
	if (!(xim->tileFrames && xim->tileLabels > 0))
	    return;

	/* Initialize the label markers. */
	if (label_init) {
	    /* First time through make sure we're all NULL.  */
	    for (i=0;  i < MAX_FRAMES; i++)  
		labels[i] = (XtPointer) NULL;
	    label_init = 0;
	} else {
	    /* Free up the existing labels.  */
	    for (i=0;  labels[i] && i < MAX_FRAMES; i++) {
	    	GmDestroy (labels[i]);
	    	labels[i] = (XtPointer) NULL;
	    }
	}


	for (i=0;  i < xim->nframes;  i++) {
	    if (xim->tileFramesList & (1 << i)) {
		xim_getScreen (xim, i+1, &sx, &sy, &width, &height, &depth);
		fb = &xim->frames[i];
		switch (xim->tileLabels) {
		case 1: 				/* frame number */
		    sprintf (text, " %d ", i+1);
		    break;
		case 2: 				/* image name 	*/
		    if (fb->nmaps == 0) {
			strcpy (text, " Blank ");
		    } else {
		        mp = &fb->mapping[0];
		        len = strlen (mp->ref);
		        for (j=len-1; mp->ref[j] != '/' && j > 0; j--) ;
		        sprintf (text, " %s ", &mp->ref[++j]);
		    }
		    break;
		case 3: 				/* image title	*/
		    if (fb->nmaps == 0) {
			strcpy (text, " Blank ");
		    } else {
		        len = strlen (fb->ctran.imtitle);
		        for (j=0; fb->ctran.imtitle[j] != ' ' && j < len; j++) ;
		        j += 3;
		        sprintf (text, " %s ", &fb->ctran.imtitle[j]);
		    }
		    break;
		}


		/* Now draw the label as a text marker on the tile.  We use
		 * markers since they can be placed as needed in the frame
		 * and provide a background which lets them be read despite
		 * whatever image scaling is in place.
		 */
		gm = (XtPointer) GmCreate (xim->gt, Gm_Text, False);

		nargs = 0;			/* initialize		*/
		len = strlen (text);
		sprintf (tw, "%dch", len);

		XtSetArg (args[nargs], GmX, sx + 10);            nargs++;
		XtSetArg (args[nargs], GmY, sy + height - 20);   nargs++;
		XtSetArg (args[nargs], GmLineWidth, 0);          nargs++;
		XtSetArg (args[nargs], GmSensitive, True);       nargs++;
		XtSetArg (args[nargs], GmVisible, True);         nargs++;
		XtSetArg (args[nargs], GmActivated, True);       nargs++;
		XtSetArg (args[nargs], GmImageText, True);       nargs++;

		GmSetAttribute (gm, GmWidth, tw, XtRString);
		GmSetAttribute (gm, GmHeight, "1ch", XtRString);
		GmSetAttribute (gm, GmTextBgColor, "black", XtRString);
		GmSetAttribute (gm, GmTextColor, "yellow", XtRString);
		GmSetAttribute (gm, GmText, text, XtRString);
		GmSetAttributes (gm, args, nargs, XtRInt);
        	GmMarkpos (gm);
        	GmRedraw (gm, GXcopy, True);

		labels[i] = gm;			/* save the marker ptr 	*/
	    }
	}
}


/* XIM_MATCHFRAMES -- Make the color enhancement of the listed frames match
 * that of the indicated frame.
 */
void
xim_matchFrames (xim, frames, reference_frame)
XimDataPtr xim;
int *frames;
int reference_frame;
{
	register FrameBufPtr fr, fb = &xim->frames[reference_frame-1];
	register int *ip, i;
	Widget gt = xim->gt;
	char buf[256];
	int bits;

	/* If frames is NULL match all frames.  Set one bit in BITS for
	 * each frame to be modified.  This ensures that each frame is
	 * only modified once, even if the same frame is listed more than
	 * once.
	 */
	if (frames) {
	    for (ip=frames, bits=0;  *ip;  ip++)
		bits |= (1 << (*ip - 1));
	} else {
	    for (i=1, bits=0;  i <= xim->nframes;  i++)
		bits |= (1 << (i - 1));
	}

	for (i=0;  i < xim->nframes;  i++) {
	    fr = &xim->frames[i];
	    if (fr != fb && (bits & (1 << (fr->frameno - 1)))) {
		fr->colormap = fb->colormap;
		fr->offset = fb->offset;
		fr->scale = fb->scale;
		xim_enhancement (xim, fr);
	    }
	}
}


/* XIM_REGISTERFRAMES -- Register the listed frames with the given reference
 * frame, i.e.  set the field center and scale of each frame to match the
 * reference frame.
 */
void
xim_registerFrames (xim, frames, reference_frame, offsets)
XimDataPtr xim;
int *frames;
int reference_frame;
int offsets;
{
	register int *ip, i;
	register FrameBufPtr fr, fb = &xim->frames[reference_frame-1];
	int src, st, sx, sy, snx, sny;
	int dst, dt, dx, dy, dnx, dny;
	Widget gt = xim->gt;
	int bits, rop;

	/* If frames is NULL register all frames.  Set one bit in BITS for
	 * each frame to be modified.  This ensures that each frame is
	 * only modified once, even if the same frame is listed more than
	 * once.
	 */
	if (frames) {
	    for (ip=frames, bits=0;  *ip;  ip++)
		bits |= (1 << (*ip - 1));
	} else {
	    for (i=1, bits=0;  i <= xim->nframes;  i++)
		bits |= (1 << (i - 1));
	}

	GtGetMapping (gt, fb->zoommap,
	    &rop, &src,&st,&sx,&sy,&snx,&sny, &dst,&dt,&dx,&dy,&dnx,&dny);

	for (i=0;  i < xim->nframes;  i++) {
	    fr = &xim->frames[i];

/*	    if (fr != fb && (bits & (1 << (fr->frameno - 1)))) {*/
	    if ((bits & (1 << (fr->frameno - 1)))) {
		fr->xcen = fb->xcen;  fr->ycen = fb->ycen;
		fr->xmag = fb->xmag;  fr->ymag = fb->ymag;
		fr->xflip = fb->xflip;  fr->yflip = fb->yflip;

		if (!xim_onScreen (xim, fb->frameno))
		    GtDisableMapping (gt, fr->zoommap, 0);

		if (offsets) {
		    /* fb is the current display buffer, fr is some other
		     * frame in the list.  To do the offsets we must
		     * first subtract the offset of the current display
		     * and then add the offset peculiar to the frame.
		     */

		    int nsx, nsy;

		    nsx = (int)(sx - fb->xoff + fr->xoff);
		    nsy = (int)(sy - fb->yoff + fr->yoff);

		    GtSetMapping (gt, fr->zoommap, xim->rop,
		        fr->raster, st,nsx,nsy, snx,sny, 
		        fr->zoomras,dt,dx,dy,dnx,dny);
		    GtRefreshMapping (gt, fr->zoommap);

		} else {
		    GtSetMapping (gt, fr->zoommap, xim->rop,
		        fr->raster, st,sx,sy, snx,sny, 
		        fr->zoomras,dt,dx,dy,dnx,dny);
		}
	    }
	}
	    
	/* A normal registration zeroes the offsets for each frame.  */
	if (!offsets) {
	    for (i=0;  i < xim->nframes;  i++) {
	        fr = &xim->frames[i];
	        fr->xoff = fr->yoff = 0;
	    }
	}
	xim_labelTiles (xim);
}


/* XIM_CURSORMODE -- Enter or leave cursor mode.  The remote client issues
 * a cursor read request via the datastream, then blocks until a cursor
 * value is returned.  The GUI is responsible for signalling to the user
 * that the application is waiting for cursor input, and for returning the
 * cursor value when the cursor read finishes.
 */
void
xim_cursorMode (xim, state)
register XimDataPtr xim;
int state;
{
	/* The GUI is responsible for implementing cursor reads. */
	xim_message (xim, "cursorMode", state ? "on" : "off");
}


/* XIM_SETMAPPING -- Set up a mapping between two rasters.
 */
void
xim_setMapping (xim, fb, frame, mapping, src, dst, fill_mode)
register FrameBufPtr fb;
register XimDataPtr xim;
int frame;
int mapping;
int src, dst;
int fill_mode;
{
	register Widget gt = xim->gt;
	int src_type, src_width, src_height, src_depth;
	int dst_type, dst_width, dst_height, dst_depth, dst_x, dst_y;
	float xscale, yscale, scale;
	int dx, dy, dnx, dny;
	int sx, sy, snx, sny;

	/* Get dimensions of source and destination rasters. */
	if (GtQueryRaster (gt, src,
		&src_type, &src_width, &src_height, &src_depth) == 0)
	    return;

	/* Get region of destination to be written into. */
	if (dst == 0) {
	    xim_getScreen (xim, frame,
		&dst_x, &dst_y, &dst_width, &dst_height, &dst_depth);
	} else {
	    if (GtQueryRaster (gt, dst,
		    &dst_type, &dst_width, &dst_height, &dst_depth) == 0)
		return;
	    dst_x = dst_y = 0;
	}

	xscale = (float)dst_width / (float)src_width;
	yscale = (float)dst_height / (float)src_height;

	switch (fill_mode) {
	case M_UNITARY:
	    /* Set up a unitary (one-to-one) mapping, i.e. one pixel in the
	     * source is mapped to one pixel in the destination.  The mapped
	     * region is the center of both rasters.
	     */
	    xscale = yscale = 1.0;
	    /* fall through */

	case M_ASPECT:
	    /* Scale the source to fit the destination, preserving the
	     * aspect ratio and displaying all source pixels.
	     */
	    scale = min (xscale, yscale);
	    if (abs(scale - (int)(scale+0.5)) < TOL)
		scale = (int)(scale+0.5);

	    dnx = src_width * scale;
	    if (dnx > dst_width) {
		/* Clip source rect. */
		dnx = dst_width;
		snx = dnx;
		sx = max (0, (src_width + 1) / 2 - snx / 2);
		dx = 0;
	    } else {
		/* Clip destination rect. */
		snx = src_width;
		sx = 0;
		dx = max (0, (dst_width + 1) / 2 - dnx / 2);
	    }

	    dny = src_height * scale;
	    if (dny > dst_height) {
		/* Clip source rect. */
		dny = dst_height;
		sny = dny;
		sy = max (0, (src_height + 1) / 2 - sny / 2);
		dy = 0;
	    } else {
		/* Clip destination rect. */
		sny = src_height;
		sy = 0;
		dy = max (0, (dst_height + 1) / 2 - dny / 2);
	    }

	    GtSetMapping (gt, mapping, xim->rop,
		src, GtPixel, sx, sy, snx, sny,
		dst, GtPixel, dst_x + dx, dst_y + dy, dnx, dny);
	    break;

	case M_FILL:
	    /* Scale the source to fill the destination window.
	     */
	    GtSetMapping (gt, mapping, xim->rop,
		src, GtPixel, 0, 0, src_width, src_height,
		dst, GtPixel, dst_x, dst_y, dst_width, dst_height);
	    break;
	}

	if (fb && mapping == fb->zoommap) {
	    fb->xmag = 1.0;
	    fb->ymag = 1.0;
	    fb->xscale = xscale;
	    fb->yscale = yscale;
	    fb->xcen = (src_width + 1) / 2.0;
	    fb->ycen = (src_height + 1) / 2.0;
	    fb->xflip = fb->yflip = 0;
	}
}


/* XIM_SETZOOM -- Modify a mapping between two rasters given the specified
 * view center and zoom factors.
 */
void
xim_setZoom (xim, fb, frame, mapping, src, dst, xcen,ycen,xmag,ymag, xoff,yoff, absolute)
register XimDataPtr xim;
register FrameBufPtr fb;
int frame;
int mapping;
int src, dst;
float xcen, ycen;	/* center of source raster region to be mapped */
float xmag, ymag;	/* magnification in each axis */
float xoff, yoff;	/* offset in each axis */
Boolean absolute;	/* ignore xscale/yscale */
{
	register Widget gt = xim->gt;
	int src_type, src_width, src_height, src_depth;
	int dst_type, dst_width, dst_height, dst_depth, dst_x, dst_y;
	int sx1, sx2, sy1, sy2, snx, sny;
	int dx1, dx2, dy1, dy2, dnx, dny;
	int fx1, fx2, fy1, fy2, shift;
	int src_recenter_allowed = 1;
	int dst_recenter_allowed = 1;
	int xreplicate, yreplicate;
	int scale, max_x, max_y, bias;
	float xscale, yscale;
	char buf[256];


	/* Get dimensions of source and destination rasters. */
	if (GtQueryRaster (gt, src,
		&src_type, &src_width, &src_height, &src_depth) == 0)
	    return;

	/* Get region of destination to be written into. */
	if (dst == 0) {
	    xim_getScreen (xim, frame,
		&dst_x, &dst_y, &dst_width, &dst_height, &dst_depth);
	} else {
	    if (GtQueryRaster (gt, dst,
		    &dst_type, &dst_width, &dst_height, &dst_depth) == 0)
		return;
	    dst_x = dst_y = 0;
	}

	/* Use old center if none given. */
	if (xcen <= 0)
	    xcen = fb->xcen;
	if (ycen <= 0)
	    ycen = fb->ycen;

	/* Get scaling ratios. */
	if (absolute) {
	    xscale = xmag;
	    yscale = ymag;
	} else {
	    xscale = xmag * fb->xscale;
	    yscale = ymag * fb->yscale;
	}

	/* Avoid roundoff if integer scaling. */
	if (xreplicate = (abs(xscale - (int)(xscale+0.5)) < TOL))
	    xscale = (int)(xscale+0.5);
	if (yreplicate = (abs(yscale - (int)(yscale+0.5)) < TOL))
	    yscale = (int)(yscale+0.5);

	/* Sanity check, return if we're getting too small. */
	if (xscale < TOL || yscale < TOL)
	    return;


	/* Map the destination rect back into the source with the given
	 * zoom factor and center, clipping the source rect at the source
	 * raster boundary.
	 */
src_recenter:
	max_x = src_width - 1;
	max_y = src_height - 1;

	snx = (int) (dst_width / xscale) + 1;
	sny = (int) (dst_height / yscale) + 1;

	sx1 = (int) (xcen - snx / 2.0 + 0.5 + xoff);		/*********/
	sx2 = (int) (xcen + snx / 2.0 + 0.5);
	sy1 = (int) (ycen - sny / 2.0 + 0.5 + yoff);		/*********/
	sy2 = (int) (ycen + sny / 2.0 + 0.5);

	/* Fiddle the source rect slightly if necessary to achieve the
	 * requested snx,sny.  This corrects for any pixel selection errors
	 * introduced in the float to integer truncation.  This is a small
	 * effect but it can be apparent at high pixel magnifications.
	 */
	sx2 -= ((sx2 - sx1 + 1) - snx);
	sy2 -= ((sy2 - sy1 + 1) - sny);

	if (src_recenter_allowed) {
	    /* Adjust the center of the source rect to make it fit on the
	     * source raster, but only if only one edge extends beyond the
	     * raster boundary.  If both edges are out of bounds we just
	     * clip below.
	     *
	     * MJF 9/29  - The changes to fx2/fy2 below are to fix an off-by-one
	     * error in the center pixel computed by an integer zoom.  For
	     * example, display a 256sq image, invert the cmap and zoom by
	     * two, the center is different than a straight centering op done
	     * by the pan function.  The effect is that hardcopy has an extra
	     * border on two sides.
	     */
	    fx1 = (sx1 < 0) ? -sx1 : 0;
	    fx2 = (sx2 > max_x) ? (max_x - sx2 + 1) : 0;
	    if (fx1 && !fx2 || !fx1 && fx2)
		xcen += (fx1 + fx2);

	    fy1 = (sy1 < 0) ? -sy1 : 0;
	    fy2 = (sy2 > max_y) ? (max_y - sy2 + 1) : 0;
	    if (fy1 && !fy2 || !fy1 && fy2)
		ycen += (fy1 + fy2);

	    src_recenter_allowed = 0;
	    goto src_recenter;
	}

	/* Clip the source rect to the raster boundary. */
	sx1 = max(0, min(max_x, sx1));
	sx2 = max(0, min(max_x, sx2));
	sy1 = max(0, min(max_y, sy1));
	sy2 = max(0, min(max_y, sy2));

	snx = sx2 - sx1 + 1;
	sny = sy2 - sy1 + 1;


	/* Map the zoomed, possibly centered, and clipped source rect back
	 * to the destination.
	 */
	max_x = dst_width - 1;
	max_y = dst_height - 1;

	dx1 = (int) ((sx1-0.5 - xcen) * xscale + dst_width / 2.0 + 0.5);
	dx2 = (int) ((sx2+0.5 - xcen) * xscale + dst_width / 2.0 + 0.5);
	dy1 = (int) ((sy1-0.5 - ycen) * yscale + dst_height / 2.0 + 0.5);
	dy2 = (int) ((sy2+0.5 - ycen) * yscale + dst_height / 2.0 + 0.5);

	if (dst_recenter_allowed) {
	    /* If the unclipped destination rect extends out of bounds on
	     * one side or the other (but not both), shift it slightly to 
	     * try to recenter it on the destination raster.  This tends
	     * to cancel out rounding errors occurring in the calculation
	     * of the destination rect.
	     */
	    fx1 = (dx1 < 0) ? -dx1 : 0;
	    fx2 = (dx2 > max_x) ? (max_x - dx2) : 0;
	    if (fx1 && !fx2 || !fx1 && fx2) {
		shift = (fx1 + fx2);
		dx1 += shift;
		dx2 += shift;
	    }

	    fy1 = (dy1 < 0) ? -dy1 : 0;
	    fy2 = (dy2 > max_y) ? (max_y - dy2) : 0;
	    if (fy1 && !fy2 || !fy1 && fy2) {
		shift = (fy1 + fy2);
		dy1 += shift;
		dy2 += shift;
	    }

	    /* If the destination rect does not fill the destination raster,
	     * center it in the raster.
	     */
	    if ((dx2 - dx1 + 1) < dst_width) {
		shift = (dx1 + (max_x - dx2)) / 2 - dx1;
		dx1 += shift;
		dx2 += shift;
	    }

	    if ((dy2 - dy1 + 1) < dst_height) {
		shift = (dy1 + (max_y - dy2)) / 2 - dy1;
		dy1 += shift;
		dy2 += shift;
	    }
	}

	/* Clip to the edge of the destination raster. */
	dx1 = max(0, min(max_x, dx1));
	dx2 = max(0, min(max_x, dx2));
	dy1 = max(0, min(max_y, dy1));
	dy2 = max(0, min(max_y, dy2));

	dnx = dx2 - dx1 + 1;
	dny = dy2 - dy1 + 1;

	/* Attempt to integerize things if integer scaling is selected.  This
	 * can leave a blank pixel or two at the edge depending upon the
	 * window size and scaling.
	 */
	scale = (int)xscale;
	if (xreplicate && dnx != snx * scale) {
	    if (snx < src_width) {
		snx = max(1, min(src_width, dnx / scale));
		dnx = max(1, min(dst_width, snx * scale));
	    } else if (dnx < dst_width) {
		dnx = max(1, min(dst_width, snx * scale));
		snx = max(1, min(src_width, dnx / scale));
	    }
	    if ((bias = ((dx2 - dx1 + 1) - dnx) / 2) > 0) {
		dx1 += bias;
		dx2 += bias;
	    }
	}
	scale = (int)yscale;
	if (yreplicate && dny != sny * scale) {
	    if (sny < src_height) {
		sny = max(1, min(src_height, dny / scale));
		dny = max(1, min(dst_height, sny * scale));
	    } else if (dny < dst_height) {
		dny = max(1, min(dst_height, sny * scale));
		sny = max(1, min(src_height, dny / scale));
	    }
	    if ((bias = ((dy2 - dy1 + 1) - dny) / 2) > 0) {
		dy1 += bias;
		dy2 += bias;
	    }
	}



	/* Set the mapping. */
        initialize_shadow_pixmap (gt, dst);
	GtSetMapping (gt, mapping, xim->rop,
	    src, GtPixel, sx1, sy1, snx, sny,
	    dst, GtPixel, dst_x + dx1, dst_y + dy1,
		fb->xflip ? -dnx : dnx,
		fb->yflip ? -dny : dny);

	/* Update the frame buffer zoom/pan/flip parameters. */
	fb->xcen = xcen;     fb->ycen = ycen;
	fb->xoff = xoff;     fb->yoff = yoff;

	if (absolute) {
	    fb->xmag = xmag / fb->xscale;
	    fb->ymag = ymag / fb->yscale;
	} else {
	    fb->xmag = xmag;
	    fb->ymag = ymag;
	}

	xim_frameRegion (xim, fb);
	sprintf (buf, "%g %g %g %g %g %g %g %g",
	    fb->xmag, fb->ymag, fb->xcen, fb->ycen, 
	    fb->xscale, fb->yscale, fb->xoff, fb->yoff);
	xim_message (xim, "frameView", buf);
}


/* XIM_GETSCREEN -- Determine the region of the display window to which
 * the given frame is mapped.
 */
xim_getScreen (xim, frame, sx, sy, width, height, depth)
register XimDataPtr xim;
int frame;
int *sx, *sy;
int *width, *height, *depth;
{
	register int i;
	int border = xim->tileBorder;
	int rtype, scr_width, scr_height;
	int twidth, theight, tileno, frameno;
	int tilex, tiley;
	int nrows = xim->tileRows, ncols = xim->tileCols;

	if (GtQueryRaster (xim->gt, 0,
		&rtype, &scr_width, &scr_height, depth) == 0)
	    return;

	/* Use the entire display window if we are not tiling frames, if
	 * there aren't enough frames to tile, or if the given frame is
	 * not in the list of tile frames.
	 */
	if (!xim->tileFrames || xim->nframes <= 1 ||
		!(xim->tileFramesList & (1 << frame-1))) {
	    *sx = *sy = 0;
	    *width = scr_width;
	    *height = scr_height;
	    return;
	}

	/* Get index of frame in tile Frames list. */
	for (i=1, frameno=0;  i <= xim->nframes;  i++) {
	    if (xim->tileFramesList & (1 << (i-1))) {
		frameno++;
		if (frame == i)
		    break;
	    }
	}

	/* Now compute the size of each tile. */
	tileno  = frameno - 1;
	twidth  = (scr_width  - (ncols * 2 * border)) / ncols;
	theight = (scr_height - (nrows * 2 * border)) / nrows;

	/* Get the "coordinates" of the tile in the mosaic. */
	if (xim->tileByRows) {
	    tilex = tileno % ncols;
	    tiley = tileno / ncols;
	} else {
	    tilex = tileno / nrows;
	    tiley = tileno % nrows;
	}
	if (!xim->tileTopDown)
	    tiley = (nrows - tiley - 1);

	/* Finally, get the placement of the tile on the screen. */
	*sx = tilex * twidth  + ((tilex*2+1) * border);
	*sy = tiley * theight + ((tiley*2+1) * border);

	*width = twidth;
	*height = theight;
}


/* XIM_ONSCREEN -- Test whether the given frame is visible onscreen.
 */
xim_onScreen (xim, frame)
register XimDataPtr xim;
int frame;
{
	if (xim->tileFrames)
	    return ((xim->tileFramesList & (1 << (frame-1))) != 0);
	else
	    return (frame == xim->display_frame);
}


/* XIM_SETFLIP -- Modify a mapping to flip the frame in X and/or Y.
 */
void
xim_setFlip (xim, fb, flip_x, flip_y)
register XimDataPtr xim;
register FrameBufPtr fb;
int flip_x, flip_y;
{
	register Widget gt = xim->gt;
	int src, st, sx, sy, snx, sny;
	int dst, dt, dx, dy, dnx, dny;
	int rop;

	if (!flip_x && !flip_y)
	    return;

	if (flip_x)
	    fb->xflip = !fb->xflip;
	if (flip_y)
	    fb->yflip = !fb->yflip;

	GtGetMapping (gt, fb->zoommap,
	    &rop, &src,&st,&sx,&sy,&snx,&sny, &dst,&dt,&dx,&dy,&dnx,&dny);

	dnx = abs (dnx);
	dnx = fb->xflip ? -dnx : dnx;
	dny = abs (dny);
	dny = fb->yflip ? -dny : dny;

	GtSetMapping (gt, fb->zoommap,
	    rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny);

	xim_message (xim, "xflip", fb->xflip ? "true" : "false");
	xim_message (xim, "yflip", fb->yflip ? "true" : "false");
}


/* XIM_SETROP -- Modify the rasterop portion of a mapping.
 */
void
xim_setRop (xim, fb, rop)
register XimDataPtr xim;
register FrameBufPtr fb;
int rop;
{
	register Widget gt = xim->gt;
	int src, st, sx, sy, snx, sny;
	int dst, dt, dx, dy, dnx, dny;
	int oldrop;

	GtGetMapping (gt, fb->zoommap,
	    &oldrop, &src,&st,&sx,&sy,&snx,&sny, &dst,&dt,&dx,&dy,&dnx,&dny);

	GtSetMapping (gt, fb->zoommap,
	    rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny);
}


/* XIM_SETCURSORPOS -- Set the cursor position.
 */
void
xim_setCursorPos (xim, sx, sy)
register XimDataPtr xim;
float sx, sy;			/* raster coordinates */
{
	GtSetRaster (xim->gt, xim->df_p->frameno);
	GtSetCursorPos (xim->gt, (int)sx, (int)sy);
	GtSetRaster (xim->gt, 0);
}


/* XIM_GETCURSORPOS -- Get the cursor position.  This is done to a fractional
 * pixel precision if the image is zoomed.
 */
void
xim_getCursorPos (xim, sx, sy, raster, frame)
register XimDataPtr xim;
float *sx, *sy;
int *raster, *frame;
{
	register FrameBufPtr fb;
	DPoint pv1, pv2;
	int rx, ry, rmap;
	int src, x, y, i;

	GtGetCursorPos (xim->gt, &x, &y);
	src = GtSelectRaster (xim->gt, 0, GtPixel, x,y, GtNDC, &rx, &ry, &rmap);

	pv1.x = rx;  pv1.y = ry;
	GtNDCToPixel (xim->gt, src, &pv1, &pv2, 1);
	*sx = pv2.x;
	*sy = pv2.y;

	if (raster)
	    *raster = src;

	if (frame) {
	    *frame = xim->display_frame;
	    for (i=1;  i < xim->nframes;  i++) {
		fb = &xim->frames[i-1];
		if (fb->raster == src || fb->zoomras == src) {
		    *frame = fb->frameno;
		    break;
		}
	    }
	}
}


/* XIM_READDISPLAY -- Read a region of the current display frame and return
 * the pixels and corresponding RGB colormap.  We allocate a pointer to the
 * pixels that needs to be freed later on.  Note this routine returns an
 * 8 bit RGB colormap.
 */
unsigned char *
xim_readDisplay (xim, x0,y0,nx,ny, w,h, r,g,b, ncolors) 
register XimDataPtr xim;
int x0,y0,nx,ny;			/* region to extract (input) */
int *w, *h;				/* size of output region (output). */
unsigned char *r, *g, *b;		/* colortable (output) */
int *ncolors;				/* size of colortable (output) */
{
	register Widget gt = xim->gt;
	register int i, j;

	int raster, x1, y1, nc;
	int first, nelem, maxelem;
	char buf[SZ_LINE], *tmpfile;
	unsigned short *rs, *gs, *bs;
	int junk, width, height, depth;
	unsigned short *cmap=NULL, *iomap=NULL;
	unsigned char *pixels=NULL;
	static int debug = 0;

	/* The display is raster zero. */
	raster = 0;

	/* Get the size of the raster we're going to extract.  If nx or ny
	 * is zero use the entire raster, else use the indicated region.
	 * If a region is given clip it to the raster boundary.
	 */
	if (GtQueryRaster (gt, raster, &junk, &width, &height, &depth) == 0)
	    goto error;


	if (nx <= 0 || ny <= 0) {
	    x0 = y0 = 0;
	    nx = width;
	    ny = height;
	} else {
	    x1 = min (width-1, x0 + nx - 1);
	    y1 = min (height-1, y0 + ny - 1);
	    x0 = max (0, x0);
	    y0 = max (0, y0);
	    nx = x1 - x0 + 1;
	    ny = y1 - y0 + 1;
	}

	/* Get a pixel buffer and read the pixels. */
	if (nx*ny <= 0 || !(pixels = (unsigned char *) malloc (nx * ny)))
	    goto error;
	if (GtReadPixels (gt, raster, pixels, 8, x0, y0, nx, ny) < 0)
	    goto error;


	/* Get Gterm widget colormap. */
	if (!(cmap = (ushort *) malloc (3 * MAX_COLORS * sizeof(ushort))))
	    goto error;

	rs = cmap;
	gs = rs + MAX_COLORS;
	bs = gs + MAX_COLORS;

	if (depth == 8) {
	    nc = GtReadColormap (gt, raster, 0, MAX_COLORS, rs,gs,bs);
	    if (nc <= 0)
	        goto error;


	    /* Get the RGB color associated with each pixel.  To determine
	    ** this we must consider both the Gterm widget colormap and iomap.
	    ** The iomap maps client pixels to Gterm widget pixels, and the 
	    ** colormap maps Gterm widget pixels to RGB values.  GtReadPixels
	    ** returns client pixels, so to get the RGB value associated with
	    ** each client pixel we must run the client pixel through the iomap
	    ** to get the Gterm widget pixel, and then use this to look up the
	    ** RGB value in the widget's colormap.  When reading the display
	    ** (raster=0) we get the colormap corresponding to the image as
	    ** currently windowed, hence the output image will be rendered as
	    ** in the current display. Note: the Gterm widget maps are 16 bits,
	    ** while we return an 8 bit RGB colormap to our caller.
	    */
	    /* Get iomap. */
	    if (!(iomap = (ushort *) malloc (MAX_COLORS * sizeof(ushort))))
	        goto error;
	    GtReadIomap (gt, iomap, 0, MAX_COLORS);

	    if (debug) {
	        register short pmin = MAX_COLORS, pmax = 0, i, j;
                fprintf (stderr, "iomap\n");
	        for (i=0; i < MAX_COLORS; ) {
                    for (j=0; j < 8 && i < MAX_COLORS; j++) {
		        pmin = (iomap[i] < pmin ? iomap[i] : pmin);
		        pmax = (iomap[i] > pmax ? iomap[i] : pmax);
                        fprintf (stderr, "%3d(%3d) ", i, iomap[i++]);
		    } 
                    fprintf (stderr, "\n");

	        }
	        fprintf (stderr, "iomap min = %d max = %d\n", pmin, pmax);
                fprintf (stderr, "Gterm Colormap\n");
                for (i=0; i < MAX_COLORS; ) {
                    for (j=1; j < 4 && i < MAX_COLORS; j++)
                        fprintf (stderr, "    %3d(%3d,%3d,%3d)",i,
			    (rs[i]>>8),(gs[i]>>8),(bs[i++]>>8));
                    fprintf (stderr, "\n");
                }
	        fflush (stderr);
	    }

	    /* Output the colormap. */
	    for (i=0;  i < nc;  i++) {
	        j = iomap[i];
	        r[i] = (rs[j] >> 8);
	        g[i] = (gs[j] >> 8);
	        b[i] = (bs[j] >> 8);
	    }
	    for (i=nc;  i < MAX_COLORS;  i++) {
	        j = iomap[i];
	        r[i] = (rs[j] >> 8);
	        g[i] = (gs[j] >> 8);
	        b[i] = (bs[j] >> 8);
	    }

	} else {
	    unsigned long lut[MAX_COLORS];
	    int   start = 10;			/* static colors offset  */

	    GtReadLUT (gt, lut, 0, MAX_COLORS);

	    nc = 216;
	    for (i=0; i < nc; i++) {
	        r[i] = (lut[i] >> 16) & 0xff;
	        g[i] = (lut[i] >>  8) & 0xff;
	        b[i] = (lut[i]      ) & 0xff;
	    }
	}

	*w = nx;
	*h = ny;
	*ncolors = nc;			/* includes static colors */

	if (debug) {
	    fprintf (stderr,
		"xim_readDisplay:  w=%d  h=%d  d=%d\n", width, height, depth);
	    for (i=0; i < nc; i++)
  	        fprintf (stderr, "xim_readDisplay[%3d]:  %3d %3d %3d\n",
		i, r[i], g[i], b[i]);
	}

	free ((char *) cmap);
	free ((char *) iomap);

	return (pixels);

error:
	if (cmap)
	    free ((char *) cmap);
	if (iomap)
	    free ((char *) iomap);
	if (pixels)
	    free ((char *) pixels);
	return (NULL);
}


/* XIM_WRITEDISPLAY -- Load the current display frame from the pixel array
 * and RGB colormap passed in the input arguments.  The data is assumed to
 * be 8 bit pseudocolor.
 */
int
xim_writeDisplay (xim, frame, mapname, pixels, w,h, r,g,b, ncolors)
register XimDataPtr xim;
int frame;				/* display frame to be written */
char *mapname;				/* colormap name to be written */
unsigned char *pixels;
int w, h;
unsigned char *r, *g, *b;
int ncolors;
{
	register int i, j;
	register FrameBufPtr fb;
	register Widget gt = xim->gt;
	unsigned short rs[MAX_COLORS], gs[MAX_COLORS], bs[MAX_COLORS];
	unsigned short iomap[MAX_COLORS], sv_iomap[MAX_COLORS];
	unsigned short invmap[MAX_COLORS];
	int want, npix, nx, ny, sx0, sy0, dx0, dy0;
	int sx, sy, width, height, depth;
	float xzoom, yzoom, zoom;
	int debug=0, autoscale=xim->autoscale;
	unsigned char *gtpix;
	ColorMapPtr cm;


	if (DBG_RASTER)
	    fprintf (stderr, "raster writeDisplay: .......................");

	/* If frame=0 use the current display frame. */
	if (frame < 1) frame = xim->display_frame;

	/* Create additional frames if needed. */
	if (frame > xim->nframes) {
            for (i=1;  i <= frame;  i++) {
                fb = &xim->frames[i-1];
                if (fb->frameno != i) {
                    xim_initFrame (xim, i, frame,
                        &xim->fb_config[xim->fb_configno-1], xim->memModel);

                    /* If we're in tile mode, add the frame to the tile list
                     * and if needed resize the tile frames.
                     */
                    if (xim->tileFrames) {
                        xim->tileFramesList |= (1 << (i-1));
                        xim->nTileFrames++;
                        xim_tileFrames (xim, xim->tileFramesList);
                    }
                }
            }
	}


	/* Erase the existing frame before disabling the mapping, otherwise
	 * we get a white background when the new image is loaded.
	 */
	xim_eraseFrame (xim, frame);

	fb = &xim->frames[frame-1];
	GtDisableMapping (gt, fb->dispmap, 0);

	/* Substitute a one-to-one iomap so that we don't confuse colors for
	 * a non-continuous tone (grayscale) image.
	 */
	GtReadIomap (gt, sv_iomap, 0, MAX_COLORS);
	for (i=0;  i < MAX_COLORS;  i++)
	    iomap[i] = FIRST_COLOR + min (xim->ncolors, i);
	GtWriteIomap (gt, iomap, 0, MAX_COLORS);

	/* If debug mode is enabled compute and print the minimum and maximum
	 * pixel values and a histogram or count of the number of pixels at
	 * each value.
	 */
	if (debug) {
	    int count[MAX_COLORS];
	    int v, lo, hi;

	    memset ((void *)count, 0, sizeof(count));
	    for (i=0, lo=MAX_COLORS, hi=0;  i < w*h;  i++) {
		v = pixels[i];
		count[v]++;
		if (v < lo)
		    lo = v;
		else if (v > hi)
		    hi = v;
	    }
	    fprintf (stderr, "%s: w=%d h=%d min=%d max=%d\n",
		mapname, w, h, lo, hi);

	    for (i=0, j=0;  i < MAX_COLORS;  i++)
		if (count[i]) {
		    fprintf (stderr, "  %3d(%4d)", i, min(9999,count[i]));
		    if (++j >= 7) {
			fprintf (stderr, "\n");
			j = 0;
		    }
		}
	    if (j)
		fprintf (stderr, "\n");

	    fprintf (stderr, "zeros: ");
	    for (i=0;  i < MAX_COLORS;  i++)
		if (!count[i])
		    fprintf (stderr, " %d", i);
	    fprintf (stderr, "\n");
	}

	/* Invert the iomap.  We want to map standard pixels 0-N to the range
	 * FC-FC+N in Gterm color space (FC is shorthand for FIRST_COLOR).
	 * GtWritePixels will process pixels through the ximtool iomap so we
	 * need to compute invmap, which tells us the input value needed to
	 * get the desired value of out the iomap.
	GtReadIomap (gt, iomap, 0, MAX_COLORS);
	memset ((void *)invmap, 0, sizeof(invmap));
	for (i=0;  i < MAX_COLORS;  i++)
	    invmap[iomap[i]] = i;
	if (!(gtpix = (unsigned char *) malloc (npix = w * h)))
	    return (-1);
	for (i=0;  i < npix;  i++) {
	    want = FIRST_COLOR + pixels[i];
	    gtpix[i] = invmap[want];
	}
	free ((char *) gtpix);
	 */

	/* If the image is too large to fit in the existing frame clip it
	 * to fit (we need to add an auto-resize option here later).
	 */
	if (w > xim->width) {
	    nx = xim->width;
	    sx0 = (w - nx) / 2;
	    dx0 = 0;
	} else {
	    nx = w;
	    sx0 = 0;
	    dx0 = (xim->width - w) / 2;
	}

	if (h > xim->height) {
	    ny = xim->height;
	    sy0 = (h - ny) / 2;
	    dy0 = 0;
	} else {
	    ny = h;
	    sy0 = 0;
	    dy0 = (xim->height - h) / 2;
	}

	/* Load the frame.  The GtWritePixels call loads to the raster 
	 * associated with the frame, not the frame number directly.
	 */
	if (nx <= xim->width && ny <= xim->height) {
	    GtWritePixels (gt, fb->raster, pixels, 8, dx0, dy0, nx, ny);
	} else {
	    unsigned char *ip = pixels + sy0 * w + sx0;
	    for (i=0;  i < ny;  i++, ip += w)
		GtWritePixels (gt, fb->raster, ip, 8, dx0, dy0+i, nx, 1);
	}

	/* Get screen size. */
	xim_getScreen (xim, 1, &sx, &sy, &width, &height, &depth);

	/* Compute the new scale factor required to scale the source to the
	 * destination at magnification 1.0.
	 */
	if (autoscale) {
	    xzoom = (float)width / (float)nx;
	    yzoom = (float)height / (float)ny;
	    zoom = min (xzoom, yzoom);
	} else {
	    xzoom = 1.0;
	    yzoom = 1.0;
	    zoom = 1.0;
	}

	/* Set the new mapping. */
	fb->xmag = fb->ymag = zoom;
	fb->xcen = xim->width / 2;
	fb->ycen = xim->height / 2;

	/* Zoom it to fill the display window. */
	xim_setZoom (xim, fb, frame, fb->zoommap, fb->raster, fb->zoomras,
	    fb->xcen, fb->ycen, fb->xmag, fb->ymag, 
	    fb->xoff, fb->yoff, True);

	/* Check if the named colortable is already defined.  If not, add
	 * another one.  If ncolors=0 omit the colortable handling, e.g.
	 * when loading a raster that doesn't have a colormap.
	 */
	if (ncolors > 0) {
	    for (i=0, cm=NULL;  i < ncolormaps;  i++)
		if (strcmp (colormaps[i].name, mapname) == 0) {
		    cm = &colormaps[i];
		    break;
		}
	    if (!cm) {
		if (ncolormaps >= MAX_COLORMAPS - 1)
		    cm = &colormaps[ncolormaps];
		else {
		    cm = &colormaps[ncolormaps++];
		    cm->mapno = GtNextColormap (gt);
		}
		strcpy (cm->name, mapname);
		ncolormaps = min (MAX_COLORMAPS-1, ncolormaps);

		/* Pass the list of colortables on to the user interface. */
		xim_colortables (xim);
	    }

	    for (i=0;  i < ncolors;  i++) {
		rs[i] = (r[i] << 8);
		gs[i] = (g[i] << 8);
		bs[i] = (b[i] << 8);
	    }

	    if (DBG_RASTER)
		fprintf (stderr,"raster writeDisplay: writing colormap.......");
	    GtWriteColormap (gt, cm->mapno,
		FIRST_COLOR, min(ncolors,xim->ncolors), rs, gs, bs);
	    if (DBG_RASTER)
		fprintf (stderr,
		    "raster writeDisplay: writing colormap.......DONE");

	    fb->offset = 0.5;
	    fb->scale = (xim->invert ? -1.0 : 1.0);
	    fb->colormap = cm->mapno;
	} else 
	    cm = &colormaps[fb->colormap-1];

	/* Display the frame. */
	GtEnableMapping (gt, fb->dispmap, 1);
	GtSetDisplayRaster (gt, xim->display_frame);
	xim_setDisplayFrame (xim, frame);

	GtSetColormapFocus (-1);	/* force full update	*/
	GtLoadColormap (gt, cm->mapno, fb->offset, fb->scale);
	GtSetColormapFocus (xim->cm_focus);
	xim_enhancement (xim, fb);

	GtWriteIomap (gt, sv_iomap, 0, MAX_COLORS);

	if (DBG_RASTER)
	    fprintf (stderr, "raster writeDisplay: ......................DONE");
	return (0);
}


/* XIM_MESSAGE -- Send a message to the user interface.
 */
void
xim_message (xim, object, message)
register XimDataPtr xim;
char *object;
char *message;
{
	char msgbuf[SZ_MSGBUF];

	sprintf (msgbuf, "setValue {%s}", message);
	ObmDeliverMsg (xim->obm, object, msgbuf);
}


/* XIM_MSGI -- Like xim_message, but the message is an integer value.
 */
void
xim_msgi (xim, object, value)
register XimDataPtr xim;
char *object;
int value;
{
	char msgbuf[SZ_LINE];
	sprintf (msgbuf, "setValue {%d}", value);
	ObmDeliverMsg (xim->obm, object, msgbuf);
}


/* XIM_ALERT -- Issue an alert to the server.  The message text input will
 * be displayed and either the ok (proceed) or cancel action will be taken,
 * causing the action text input to be sent back to the client to be
 * executed as a command.  This is used to alert the server (i.e. user) of
 * unusual circumstances and determine whether or not the server wants to
 * proceed.  An alert with no actions is a warning.
 */
void
xim_alert (xim, text, ok_action, cancel_action)
register XimDataPtr xim;
char *text;			/* message text */
char *ok_action;		/* command sent back to client for "ok" */
char *cancel_action;		/* command sent back to client for "cancel" */
{
	char msgbuf[SZ_LINE];
	sprintf (msgbuf, "setValue {{%s} {%s} {%s}}", text,
	    ok_action ? ok_action : "", cancel_action ? cancel_action : "");
	ObmDeliverMsg (xim->obm, "alert", msgbuf);
}


/*
 * Internal Routines.
 * -------------------
 */

/* XIM_FRAMEREGION -- Called when the value of the frameRegion UI parameter
 * needs to be updated.
 */
static void
xim_frameRegion (xim, fb)
register XimDataPtr xim;
register FrameBufPtr fb;
{
	int rop, src, dst;
	int st, sx, sy, snx, sny;
	int dt, dx, dy, dnx, dny;
	char buf[SZ_LINE];

	if (GtGetMapping (xim->gt, fb->zoommap, &rop,
	    &src, &st, &sx, &sy, &snx, &sny,
	    &dst, &dt, &dx, &dy, &dnx, &dny) == -1)
		return;

	/* args: frame x y width height */
	sprintf (buf, "%d %d %d %d %d", fb->frameno, sx, sy, snx, sny);
	xim_message (xim, "frameRegion", buf);
}


/* XIM_COLORTABLES -- Called when the "colortables" UI parameter needs to be
 * updated, i.e., when some change to the list of colormaps has occurred.
 */
static void
xim_colortables (xim)
XimDataPtr xim;
{
	register char *ip, *op;
	char buf[MAX_COLORMAPS*40];
	int i;

	for (i=0, op=buf;  i < ncolormaps;  i++) {
	    *op++ = '"';
	    for (ip = colormaps[i].name;  *op = *ip++;  op++)
		;
	    *op++ = '"';
	    *op++ = '\n';
	}
	*op++ = '\0';

	xim_message (xim, "colortables", buf);
}


/* XIM_ENHANCEMENT -- Called when the "enhancement" UI parameter needs to
 * be updated for a frame.
 */
xim_enhancement (xim, fb)
register XimDataPtr xim;
register FrameBufPtr fb;
{
	char buf[SZ_LINE];

	sprintf (buf, "%d \"%s\" %0.3f %0.3f", fb->frameno,
	    colormaps[fb->colormap-1].name, fb->offset, fb->scale);
	xim_message (xim, "enhancement", buf);


	/* Force an update of the colorbar.  On TrueColor visuals this
	** is required since the colormap doesn't automatically update 
	** the display.
	*/
	set_colorbar (xim, xim->cb);
}


/* GET_FBCONFIG -- Read the XIMTOOL startup file to get the set of possible
 * frame buffer sizes.
 *
 * File format:		configno nframes width height [extra fields]
 *	e.g.,			1  2  512  512
 *				2  2  800  800
 *				3  1 1024 1024		# comment
 */
static void
get_fbconfig (xim)
register XimDataPtr xim;
{
	register char	*ip;
	register FILE	*fp = NULL;
	int	config, nframes, width, height, i;
	char	lbuf[SZ_LINE+1], *fname;
	static char *fb_paths[] = {
		"/usr/local/lib/imtoolrc",
		"/opt/local/lib/imtoolrc",
		"/iraf/iraf/dev/imtoolrc",
		"/local/lib/imtoolrc",
		"/usr/iraf/dev/imtoolrc",
		"/usr/local/iraf/dev/imtoolrc",
		NULL};

	/* Initialize the config table. */
	xim->fb_configno = 1;
	for (i=0;  i < MAX_FBCONFIG;  i++) {
	    xim->fb_config[i].nframes = 1;
	    xim->fb_config[i].width = DEF_FRAME_WIDTH;
	    xim->fb_config[i].height = DEF_FRAME_HEIGHT;
	}

	/* Now add in some defaults for commonly used sizes based on the
 	 * standard IRAF imtoolrc file, we'll avoid any instrument specific
	 * configurations.
	 */
	xim->fb_config[0].width = xim->fb_config[0].height =  512;
	xim->fb_config[1].width = xim->fb_config[1].height =  800;
	xim->fb_config[2].width = xim->fb_config[2].height = 1024;
	xim->fb_config[3].width = xim->fb_config[3].height = 1600;
	xim->fb_config[4].width = xim->fb_config[4].height = 2048;
	xim->fb_config[5].width = xim->fb_config[5].height = 4096;

	/* Attempt to open the config file. */
	if ((fname=getenv(FBCONFIG_ENV1)) || (fname=getenv(FBCONFIG_ENV2)))
	    fp = fopen (fname, "r");
	if (!fp && (fname = getenv ("HOME"))) {
	    sprintf (lbuf, "%s/%s", fname, FBCONFIG_1);
	    fp = fopen (fname = lbuf, "r");
	    if (fp) {
	        xim->imtoolrc = (char *) XtCalloc (SZ_LINE, sizeof(char));
		strncpy (xim->imtoolrc, fname, strlen(fname));
	    }
	}
	if (!fp)
	    fp = fopen (fname = xim->imtoolrc, "r");
 	for (i=0; !fp && fb_paths[i]; i++) {
	    if ((fp = fopen (fname = fb_paths[i], "r"))) {
	        xim->imtoolrc = XtCalloc(strlen(fb_paths[i]+1),sizeof(char));
		strncpy (xim->imtoolrc, fb_paths[i],strlen(fb_paths[i]));
		break;
	    }
	}
	if (!fp) {
	    fprintf (stderr, 
		"Warning: No frame buffer configuration file found.\n");
	    return;
	}


	/* Scan the frame buffer configuration file.
	 */
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
		fprintf (stderr, "ximtool: bad config `%s'\n", ip);
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
	    xim->fb_config[config].nframes = nframes;
	    xim->fb_config[config].width   = width;
	    xim->fb_config[config].height  = height;
	}

	if (fp) fclose (fp);
}


/* XIM_GETANTIALIAS -- Convert a antialias algorithm expressed as a string
 * into a Gterm rasterop code.
 */
xim_getAntialias (xim, s)
XimDataPtr xim;
char *s;
{
    register char *ip, *op;
    char word[SZ_NAME];
    int rop = 0;

    for (ip=s;  *ip && isspace(*ip);  ip++)
	;

    while (*ip) {
	for (op=word;  *ip && isalnum(*ip);  ip++)
	    *op++ = isupper(*ip) ? tolower(*ip) : *ip;
	*op++ = '\0';

	if (strcmp (word, "nearest") == 0)
	    rop |= MF_NEAREST;
	else if (strcmp (word, "bilinear") == 0)
	    rop |= MF_BILINEAR;
	else if (strcmp (word, "area") == 0)
	    rop |= MF_AREA;
	else if (strcmp (word, "blkavg") == 0)
	    rop |= MF_BLKAVG;
	else if (strcmp (word, "boxcar") == 0)
	    rop |= MF_BOXCAR;
	else if (strcmp (word, "lowpass") == 0)
	    rop |= MF_LOWPASS;
	else if (strcmp (word, "gaussian") == 0)
	    rop |= MF_GAUSSIAN;

	while (*ip && !isalnum(*ip))
	    ip++;
    }

    return (rop);
}


/* XIM_SETCOLORMAP -- Set up the RGB lookup tables used to map the windowed
 * monochrome output of a frame buffer into the hardware colormap.
 */
xim_setColormap (function, dirs, m_red, m_green, m_blue, nelem)
char *function;			/* type of colormap */
String *dirs;
unsigned short *m_red;
unsigned short *m_green;
unsigned short *m_blue;
int nelem;
{
	register int i, j;
	register char *ip, *op;
	static int seed = 0;
	int v, vsat, step;
	int knot[7];
	float frac;

	
	if (DBG_RASTER)
	    fprintf (stderr,
		"xim_setColormap: name='%s'  nelem=%d\n", function, nelem);

	vsat = MAX_COLORS - 1;
	step = MAX_COLORS / 6;
	for (i=0;  i < 7;  i++)
	    knot[i] = i * step;
	knot[6] = vsat;

	if (dirs) {
	    /* Load the colormap from a file. */
	    char fname[SZ_FNAME];
	    char lbuf[SZ_LINE];
	    TripletPtr p;
	    Lut user;
	    FILE *fp;

	    /* Get full file pathname.  If the same file is found in multiple
	     * directories the first version found is used.
	     */
	    strcpy (fname, function);
	    for (i=0;  function[0] != '/' && dirs[i];  i++) {
		for (ip=dirs[i], op=fname;  *ip;  )
		    *op++ = *ip++;
		if (op > fname && *(op-1) != '/')
		    *op++ = '/';
		for (ip=function;  *ip;  )
		    *op++ = *ip++;
		*op = '\0';
		if (access (fname, 0) == 0)
		    break;
	    }

	    /* Try to open the file. */
	    if ((fp = fopen (fname, "r")) == NULL) {
		fprintf (stderr, "cannot open %s\n", fname);
		return (ERR);
	    }

	    /* Scan the file.  The user colormap may have any number of
	     * RGB pairs up to a maximum of MAX_COLORS (256).
	     */
	    user.lutlen = 0;
	    while (fgets (lbuf, SZ_LINE, fp)) {
		char *np, *next;
		float v[3];

		/* Ignore blank lines and comment lines. */
		for (ip=lbuf;  isspace(*ip);  ip++)
		    ;
		if (*ip == '\0' || *ip == '#')
		    continue;
		if (user.lutlen+1 >= MAX_COLORS)
		    break;

		/* Get an RGB entry. */
		p = &user.hue[user.lutlen++];
		for (i=0, np=ip;  i < 3;  i++, np=next) {
		    v[i] = strtod (np, &next);
		    if (next == np) {
			fclose (fp);
			return (ERR);
		    }
		}

		p->red = v[0];
		p->green = v[1];
		p->blue = v[2];
	    }

	    fclose (fp);
	    if (user.lutlen <= 0)
		return (ERR);

	    /* Scale the user colormap to fit the NELEMS colors required
	     * for the output colormap.  nelem and lutlen must be the same
	     * to disable scaling and preserve the exact colortable values.
	     */
	    for (i=0;  i < nelem;  i++) {
		float x, w1, w2;
		float r, g, b;

		x = (float)i / (float)(nelem - 1) * (user.lutlen - 1);
		j = max(0, min(user.lutlen-1, (int)x));
		w1 = 1.0 - (x - j);
		w2 = 1.0 - w1;

		r = user.hue[j].red;
		g = user.hue[j].green;
		b = user.hue[j].blue;

		if (w2 > 0.0001 && j+1 < user.lutlen) {
		    r = r * w1 + user.hue[j+1].red * w2;
		    g = g * w1 + user.hue[j+1].green * w2;
		    b = b * w1 + user.hue[j+1].blue * w2;
		}

		m_red[i]   = r * vsat;
		m_green[i] = g * vsat;
		m_blue[i]  = b * vsat;
	    }

	} else if (strncmp (function, "Grayscale", 9) == 0) {
	    for (i=0;  i < nelem;  i++)
		m_red[i] = m_green[i] = m_blue[i] =
		    (float)i / (float)(nelem - 1) * vsat;

	} else if (strncmp (function, "HSV", 3) == 0) {
	    /* HSV: hue varies uniformly from 270 to 360 and back to 270.
	     * Value varies from zero to one using a cube root relation
	     * which causes the value to approach 1.0 rapidly away from zero.
	     * Saturation is zero near the endpoints, causing the curve
	     * to range from black to white at the endpoints, but ranges
	     * to 1.0 at the halfway point, causing nearly saturated colors
	     * in the middle of the range.
	     */
	    for (i=0;  i < nelem;  i++) {
		float h, s, v, r, g, b;
		double pow(), sin();

		frac = 1.0 - ((float)i / (float)(nelem - 1));
		h = frac * 360.0 + 270.0;
		s = abs (sin (frac * 3.1416));
		v = pow ((1.0 - frac), (1.0 / 3.0));

		hsv_to_rgb (h, s, v, &r, &g, &b);
		m_red[i]   = r * vsat;
		m_green[i] = g * vsat;
		m_blue[i]  = b * vsat;
	    }

	} else if (strncmp (function, "Heat", 4) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = heat.hue[j].red   * vsat;
		m_green[i] = heat.hue[j].green * vsat;
		m_blue[i]  = heat.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Ramp", 4) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = heat.hue[j].red   * (frac * (vsat * 2));
		m_green[i] = heat.hue[j].green * (frac * (vsat * 2));
		m_blue[i]  = heat.hue[j].blue  * (frac * (vsat * 2));
	    }

	} else if (strncmp (function, "AIPS0", 5) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = aips0.hue[j].red   * vsat;
		m_green[i] = aips0.hue[j].green * vsat;
		m_blue[i]  = aips0.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Color", 5) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = color.hue[j].red   * vsat;
		m_green[i] = color.hue[j].green * vsat;
		m_blue[i]  = color.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Staircase", 9) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = staircase.hue[j].red   * vsat;
		m_green[i] = staircase.hue[j].green * vsat;
		m_blue[i]  = staircase.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Standard", 8) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = standard.hue[j].red   * vsat;
		m_green[i] = standard.hue[j].green * vsat;
		m_blue[i]  = standard.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Red", 3) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = red.hue[j].red   * vsat;
		m_green[i] = red.hue[j].green * vsat;
		m_blue[i]  = red.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Green", 5) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = green.hue[j].red   * vsat;
		m_green[i] = green.hue[j].green * vsat;
		m_blue[i]  = green.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Blue", 4) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = blue.hue[j].red   * vsat;
		m_green[i] = blue.hue[j].green * vsat;
		m_blue[i]  = blue.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Halley", 6) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = halley.hue[j].red   * vsat;
		m_green[i] = halley.hue[j].green * vsat;
		m_blue[i]  = halley.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Rainbow1", 8) == 0) {
	    for (i=0;  i < nelem;  i++)
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

	} else if (strncmp (function, "Rainbow2", 8) == 0) {
	    for (i=0;  i < nelem;  i++) {
		frac = (float)i / (float)(nelem - 1);
		j = frac * (heat.lutlen - 1);
		m_red[i]   = rainbow.hue[j].red   * vsat;
		m_green[i] = rainbow.hue[j].green * vsat;
		m_blue[i]  = rainbow.hue[j].blue  * vsat;
	    }

	} else if (strncmp (function, "Random16", 8) == 0) {
	    int red, green, blue;
	    
	    if (!seed)
		seed = time(0);
	    srand (seed++);

	    for (i=0;  i < nelem;  ) {
		red   = ((rand() >> 4) % vsat);
		green = ((rand() >> 4) % vsat);
		blue  = ((rand() >> 4) % vsat);

		for (j=0;  i < nelem && j < nelem/16;  j++, i++) {
		    m_red[i]   = red;
		    m_green[i] = green;
		    m_blue[i]  = blue;
		}
	    }

	} else if (strncmp (function, "Random8", 7) == 0) {
	    int red, green, blue;
	    
	    if (!seed)
		seed = time(0);
	    srand (seed++);

	    for (i=0;  i < nelem;  ) {
		red   = ((rand() >> 4) % vsat);
		green = ((rand() >> 4) % vsat);
		blue  = ((rand() >> 4) % vsat);

		for (j=0;  i < nelem && j < nelem/8;  j++, i++) {
		    m_red[i]   = red;
		    m_green[i] = green;
		    m_blue[i]  = blue;
		}
	    }

	} else if (strncmp (function, "Random", 6) == 0) {
	    if (!seed)
		seed = time(0);
	    srand (seed++);
	    for (i=0;  i < nelem;  i++) {
		m_red[i]   = ((rand() >> 4) % vsat);
		m_green[i] = ((rand() >> 4) % vsat);
		m_blue[i]  = ((rand() >> 4) % vsat);
	    }
	}

	/* Scale colormap values to 16 bits. */
	for (i=0;  i < nelem;  i++) {
	    m_red[i]   <<= 8;
	    m_green[i] <<= 8;
	    m_blue[i]  <<= 8;
	}

	return (OK);
}


hsv_to_rgb (h, s, v, r, g, b)
float h, s, v;
float *r, *g, *b;
{
	register int i;
	float f, p, q, t;

	while (h >= 360.0)
	    h -= 360.0;

	h /= 60.0;
	i = (int) h;
	f = h - i;
	p = v * (1 - s);
	q = v * (1 - s*f);
	t = v * (1 - s * (1.0 - f));

	switch (i) {
	case 0:
	    *r = v;  *g = t;  *b = p;
	    break;
	case 1:
	    *r = q;  *g = v;  *b = p;
	    break;
	case 2:
	    *r = p;  *g = v;  *b = t;
	    break;
	case 3:
	    *r = p;  *g = q;  *b = v;
	    break;
	case 4:
	    *r = t;  *g = p;  *b = v;
	    break;
	case 5:
	    *r = v;  *g = p;  *b = q;
	    break;
	}
}


/* GET_DIRFILE -- Get the next file name from an open directory file.
 */
static int
get_dirfile (dir, outstr, maxch)
DIR     *dir;
char    *outstr;
int     maxch;
{
        register int    n;
        register struct dirent *dp;
        register char   *ip, *op;
        int     status;

        for (dp = readdir(dir);  dp != NULL;  dp = readdir(dir))
            if (dp->d_ino != 0) {
		n = strlen (dp->d_name);
                status = n;
                for (ip=dp->d_name, op=outstr;  --n >= 0;  )
                    *op++ = *ip++;
                *op = EOS;
                return (status);
            }

        return (EOF);
}


/* LOAD_TESTPATTERN -- Load a test pattern into the given frame.
 */
static void
load_testpattern (xim, frame, type)
XimDataPtr xim;
int frame;
int type;			/* not used */
{
	register FrameBufPtr fb = &xim->frames[frame];
	register int i, j, ncolors;
	int rtype, width, height, depth;
	unsigned char *data;

	if (GtQueryRaster (xim->gt, fb->raster,
		&rtype, &width, &height, &depth) == 0)
	    return;

	data = (unsigned char *) XtMalloc (width);
	if (data == (unsigned char *)NULL)
	    return;

	ncolors = xim->ncolors;
	for (j=0;  j < height;  j++) {
	    for (i=0;  i < width;  i++)
		data[i] = (((i + j) * 10) % ncolors);
	    GtWritePixels (xim->gt, fb->raster, data, 8, 0, j, width, 1);
	}

	XtFree ((char *)data);
}


/* SET_COLORBAR -- Write the colorbar pixels.
 */
static void
set_colorbar (xim, w)
XimDataPtr xim;
Widget w;
{
	register int i;
	static int initialized = 0;
	int first, ngray, rgb_len, rtype, width, height, depth;
	unsigned short m_red[MAX_COLORS];
	unsigned short m_green[MAX_COLORS];
	unsigned short m_blue[MAX_COLORS];
	unsigned char *data;


	if (!w)
	    return;

	if (DBG_RASTER)
	    fprintf (stderr, "SETTING COLORBAR PIXELS...... init = %d\n",
		initialized);

	if (GtQueryRaster (w, 0, &rtype, &width, &height, &depth) == 0)
	    return;

	data = (unsigned char *) XtMalloc (width * height);
	for (i=0;  i < width;  i++)
	    data[i] = ((float)i / (float)(width - 1) * (xim->ncolors - 1));

	for (i=1;  i < height;  i++)
	    memmove (data + i * width, data, width);

	if (!initialized) {
	    xim_iiscolormap (w, m_red,m_green,m_blue, &first, &ngray, &rgb_len);
	    GtWriteColormap (w, 0, first, rgb_len, m_red, m_green, m_blue);

	    xim_setColormap ("Grayscale", NULL, m_red, m_green, m_blue, ngray);
	    GtWriteColormap (w, 0, first, ngray, m_red, m_green, m_blue);

	    xim->ncolors = ngray;
	    initialized++;
	}

	GtWritePixels (w, 0, data, 8, 0, 0, width, height);
	XtFree ((char *)data);

	if (DBG_RASTER)
	    fprintf (stderr, "SETTING COLORBAR PIXELS...... DONE\n");
}


/* SET_NFRAMES -- Called when the number of frame buffers changes.
 */
static void
set_nframes (xim, nframes)
XimDataPtr xim;
int nframes;
{
	xim->nframes = nframes;
	xim_msgi (xim, "nframes", nframes);
}
