#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <sys/stat.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Tcl/tcl.h>
#include <Obm.h>
#include <ObmW/Gterm.h>
#include "ximtool.h"

/*
 * XIMCLIENT.C -- The Ximtool "client" object.  This code implements an OBM
 * client and responds to messages sent to the client object by the GUI code
 * executing under the object manager.
 *
 *	     xim_clientOpen (xim)
 *	    xim_clientClose (xim)
 *	  xim_clientExecute (xim, objname, key, cmd)
 *
 * The clientExecute callback is called by the GUI code in the object manager
 * to execute ximtool client commands.
 *
 * Client commands:
 *
 *		   setFrame  frameno
 *       frameno = getFrame  [raster]
 *	 raster = getRaster  [frameno]
 *	  frame = getSource  [raster [sx sy snx sny]]
 *		  nextFrame
 *		  prevFrame
 *		matchFrames  [frames [reference_frame]]
 *	     registerFrames  [frames [reference_frame]]
 *	    offfsetRegister  [frames [reference_frame]]
 *		   fitFrame
 *		 clearFrame  [frame]
 *
 *		  setOption  option value [args]
 *		setColormap  colormap
 *		  setOffset  xoff yoff
 *	     windowColormap  offset scale
 *	          windowRGB  color offset scale save_flag
 *		       zoom  [mag | xmag ymag [ xcen ycen ]]
 *		    zoomAbs  [mag | xmag ymag [ xcen ycen ]]
 *        center = centroid  xcen ycen size [type]
 *          pix = getPixels  x0 y0 nx ny [format]
 *			pan  xcen ycen
 *		       flip  axis [axis ...]
 *
 *           setPrintOption  option value [args]
 *                    print  [x0 y0 nx ny]
 *            setSaveOption  option value [args]
 *                     save  [x0 y0 nx ny]
 *            setLoadOption  option value [args]
 *                     load
 *                     help
 *                     info  option
 *
 *       wcsstr = encodewcs  sx sy sz
 *	       retCursorVal  sx sy [frame [wcs [key [strval]]]]
 *
 *		  ism_start  task
 *		   ism_stop  task
 *		    ism_cmd  task [args]
 *
 *		 initialize
 *		      Reset
 *		       Quit
 */


/* Client callback struct. */
typedef struct {
        XimDataPtr xim;
        Tcl_Interp *tcl;
        Tcl_Interp *server;
} XimClient, *XimClientPtr;



static int initialize(), Reset(), Quit();
static int setColormap(), updateColormap(), windowColormap();
static int zoom(), pan(), getSource();
static int setFrame(), getFrame(), getRaster(), nextFrame(), prevFrame();
static int fitFrame(), matchFrames(), registerFrames(), retCursorVal();
static int encodewcs(), flip(), clearFrame(), setOption(), setOffset();
static int setPrintOption(), setSaveOption(), setLoadOption();
static int print(), save(), load(), help(), windowRGB();
static int centroid(), getPixels();
static int ism_start(), ism_stop(), ism_cmd();

extern int ism_evaluate(), info();
extern IsmModule ismNameToPtr();
extern double atof();


/* xim_clientOpen -- Initialize the ximtool client code.
 */
void
xim_clientOpen (xim)
XimDataPtr xim;
{
	register XimClientPtr xc;
	register Tcl_Interp *tcl;

	xc = (XimClientPtr) XtCalloc (1, sizeof(XimClient));
	xim->clientPrivate = (int *)xc;

	xc->xim = xim;
	xc->tcl = tcl = Tcl_CreateInterp();
	ObmAddCallback (xim->obm, OBMCB_clientOutput|OBMCB_preserve,
	    xim_clientExecute, (XtPointer)xc);

	Tcl_CreateCommand (tcl,
	    "Quit", Quit, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "Reset", Reset, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "initialize", initialize, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "setFrame", setFrame, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "getFrame", getFrame, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "getRaster", getRaster, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "getSource", getSource, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "nextFrame", nextFrame, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "prevFrame", prevFrame, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "fitFrame", fitFrame, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "matchFrames", matchFrames, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "registerFrames", registerFrames, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "offsetRegister", registerFrames, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "clearFrame", clearFrame, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "setOption", setOption, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "setColormap", setColormap, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "setOffset", setOffset, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "windowColormap", windowColormap, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "updateColormap", updateColormap, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "zoom", zoom, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "zoomAbs", zoom, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "centroid", centroid, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "getPixels", getPixels, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "pan", pan, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "flip", flip, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "retCursorVal", retCursorVal, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "encodewcs", encodewcs, (ClientData)xc, NULL);

	Tcl_CreateCommand (tcl,
	    "setPrintOption", setPrintOption, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "print", print, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "setSaveOption", setSaveOption, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "save", save, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "setLoadOption", setLoadOption, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "load", load, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "help", help, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "info", info, (ClientData)xc, NULL);
	Tcl_CreateCommand (tcl,
	    "windowRGB", windowRGB, (ClientData)xc, NULL);

	/* ISM module callbacks. */
        Tcl_CreateCommand (tcl,
            "ism_start", ism_start, (ClientData)xc, NULL);
        Tcl_CreateCommand (tcl,
            "ism_stop", ism_stop, (ClientData)xc, NULL);
        Tcl_CreateCommand (tcl,
            "ism_cmd", ism_cmd, (ClientData)xc, NULL);
}


/* xim_clientClose -- Shutdown the ximtool client code.
 */
void
xim_clientClose (xim)
XimDataPtr xim;
{
	register XimClientPtr xc = (XimClientPtr) xim->clientPrivate;
	Tcl_DeleteInterp (xc->tcl);
}


/* xim_clientExecute -- Called by the GUI code to send a message to the
 * "client", which from the object manager's point of view is ximtool itself.
 */
xim_clientExecute (xc, tcl, objname, key, command)
register XimClientPtr xc;
Tcl_Interp *tcl;		/* caller's Tcl */
char *objname;			/* object name */
int key;			/* notused */
char *command;
{
	register XimDataPtr xim = xc->xim;

	xc->server = tcl;
	if (strcmp (objname, "client") == 0)
            Tcl_Eval (xc->tcl, command);
	else
	    ism_evaluate (xim, objname, command);

	return (0);
}


/*
 * XIMTOOL CLIENT commands.
 * ----------------------------
 */

/* Quit -- Exit ximtool.
 *
 * Usage:	Quit
 */
static int 
Quit (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	xim_shutdown (xim);
}


/* Reset -- Reset ximtool.
 *
 * Usage:	Reset
 *
 * Reset does a full power-on reset of ximtool.
 */
static int 
Reset (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	xim_initialize (xim, xim->fb_configno, xim->nframes, 1);
}


/* initialize -- Reinitialize ximtool.
 *
 * Usage:	initialize
 *
 * Initialize does a partial reinitialization of ximtool, preserving the
 * current frame buffers and view.
 */
static int 
initialize (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	xim_initialize (xim, xim->fb_configno, xim->nframes, 0);
}


/* setFrame -- Set the frame to be displayed.
 *
 * Usage:	setFrame <frameno>
 */
static int 
setFrame (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	if (argc == 2)
	    xim_setFrame (xc->xim, atoi(argv[1]));
	return (TCL_OK);
}


/* getFrame -- Get the frame number.
 *
 * Usage:	frameno = getFrame [raster]
 *
 * This routine has two forms.  When called with no argument getFrame returns
 * the current display frame.  When called with a raster number getFrame
 * returns the frame number with which the raster is associated.
 */
static int 
getFrame (xc, tcl, argc, argv)
XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb;
	char frameno[SZ_NAME];
	register int i;
	int raster;

	if (argc == 1)
	    sprintf (frameno, "%d", xc->xim->display_frame);
	else {
	    raster = atoi (argv[1]);
	    strcpy (frameno, "0");

	    for (i=1;  i <= xim->nframes;  i++) {
		fb = &xim->frames[i-1];
		if (fb->raster == raster || fb->zoomras == raster) {
		    sprintf (frameno, "%d", fb->frameno);
		    break;
		}
	    }
	}

	Tcl_SetResult (xc->server, frameno, TCL_VOLATILE);
	return (TCL_OK);
}


/* getRaster -- Get the image raster number of the given frame.
 *
 * Usage:	raster = getRaster [frameno]
 *
 * This routine has two forms.  When called with no argument getRaster returns
 * the raster number of the current display frame.  When called with a frame
 * number getRaster returns the raster number of the given frame.
 */
static int 
getRaster (xc, tcl, argc, argv)
XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb;
	char buf[SZ_NAME];
	register int i;
	int frameno;

	if (argc == 1)
	    sprintf (buf, "%d", xim->df_p->raster);
	else {
	    frameno = atoi (argv[1]);
	    strcpy (buf, "0");

	    for (i=1;  i < xim->nframes;  i++) {
		fb = &xim->frames[i-1];
		if (fb->frameno == frameno) {
		    sprintf (buf, "%d", fb->raster);
		    break;
		}
	    }
	}

	Tcl_SetResult (xc->server, buf, TCL_VOLATILE);
	return (TCL_OK);
}


/* getSource -- Get the source of the currently displayed image.
 *
 * Usage:	frame = getSource [raster [sx sy snx sny]]
 *
 * getSource returns the frame number of the currently displayed frame as
 * the function value, and the raster number and source rect within that
 * raster are returned as function arguments.
 */
static int 
getSource (xc, tcl, argc, argv)
XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register int i;
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	int src, st, sx, sy, snx, sny;
	int dst, dt, dx, dy, dnx, dny;
	char buf[SZ_NAME];
	int frameno, rop;

	GtGetMapping (xim->gt, fb->zoommap, &rop,
	    &src,&st,&sx,&sy,&snx,&sny, &dst,&dt,&dx,&dy,&dnx,&dny);

	if (argc > 1) {
	    sprintf (buf, "%d", fb->raster);
		Tcl_SetVar (xc->server, argv[1], buf, 0);
	}
	if (argc > 2) {
	    sprintf (buf, "%d", sx);
		Tcl_SetVar (xc->server, argv[2], buf, 0);
	    sprintf (buf, "%d", sy);
		Tcl_SetVar (xc->server, argv[3], buf, 0);
	    sprintf (buf, "%d", snx);
		Tcl_SetVar (xc->server, argv[4], buf, 0);
	    sprintf (buf, "%d", sny);
		Tcl_SetVar (xc->server, argv[5], buf, 0);
	}

	sprintf (buf, "%d", fb->frameno);
	Tcl_SetResult (xc->server, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* nextFrame -- Display the next frame in sequence.
 *
 * Usage:	nextFrame
 */
static int 
nextFrame (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	int frame;

	if (xim->display_frame < xim->nframes)
	    frame = xim->display_frame + 1;
	else
	    frame = 1;

	xim_setFrame (xc->xim, frame);
	return (TCL_OK);
}


/* prevFrame -- Display the previous frame in sequence.
 *
 * Usage:	prevFrame
 */
static int 
prevFrame (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	int frame;

	if (xim->display_frame > 1)
	    frame = xim->display_frame - 1;
	else
	    frame = xim->nframes;

	xim_setFrame (xc->xim, frame);
	return (TCL_OK);
}


/* matchFrames -- Set the enhancement of the listed frames to match that of
 * the given reference frame.  If no reference frame is given the current
 * display frame is used.  If no frames are listed all frames are matched
 * to the current display frame.
 *
 * Usage:	matchFrames [frames [reference_frame]]
 */
static int 
matchFrames (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	int *frames, frame_list[32], reference_frame;
	int nitems, i;
	char **items;

	/* Get reference frame. */
	if (argc > 2)
	    reference_frame = atoi (argv[2]);
	else
	    reference_frame = xim->display_frame;

	/* Get frame list. */
	if (argc > 1) {
	    if (Tcl_SplitList (tcl, argv[1], &nitems, &items) != TCL_OK)
		goto nolist;
	    for (i=0, frames=frame_list;  i < nitems;  i++)
		frames[i] = atoi (items[i]);
	    frames[i] = (int) NULL;
	    XtFree ((char *)items);
	} else
nolist:	    frames = NULL;

	xim_matchFrames (xc->xim, frames, reference_frame);
	return (TCL_OK);
}


/* registerFrames -- Register the listed frames with the given reference
 * frame.  If no reference frame is given the current display frame is used.
 * If no frames are listed all frames are registered with the current
 * display frame.
 *
 * Usage:	registerFrames [frames [reference_frame]]
 * 		offsetRegister [frames [reference_frame]]
 */
static int 
registerFrames (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	int *frames, frame_list[32], reference_frame;
	int nitems, i, offsets;
	char **items;

	/* Get reference frame. */
	if (argc > 2)
	    reference_frame = atoi (argv[2]);
	else
	    reference_frame = xim->display_frame;

	/* Get frame list. */
	if (argc > 1) {
	    if (Tcl_SplitList (tcl, argv[1], &nitems, &items) != TCL_OK)
		goto nolist;
	    for (i=0, frames=frame_list;  i < nitems;  i++)
		frames[i] = atoi (items[i]);
	    frames[i] = (int) NULL;
	    XtFree ((char *)items);
	} else
nolist:	    frames = NULL;

	offsets = (strcmp (argv[0], "offsetRegister") == 0);
	xim_registerFrames (xc->xim, frames, reference_frame, offsets);
	return (TCL_OK);
}


/* setOffset -- Set the offset for the current display frame buffer.
 *
 * Usage:	setOffset xoff yoff
 */
static int 
setOffset (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
        register FrameBufPtr fb = xim->df_p;
	float	 xcen, ycen;
	float	 xmag, ymag;
	float	 xoff, yoff;
	Boolean  absolute = False;
	int	 frame = xim->display_frame - 1;


	if (argc < 3)
	    return (TCL_ERROR);

	xcen = fb->xcen;
	ycen = fb->ycen;
	xmag = fb->xmag;
	ymag = fb->ymag;

	/* Get offset values for the frame. */
	xoff = atof (argv[1]);
	yoff = atof (argv[2]);

	/* Set frame offset.  */
        xim_setZoom (xim, fb, fb->frameno, fb->zoommap,
            fb->raster, fb->zoomras, xcen, ycen, xmag, ymag, xoff, yoff,
	    absolute);

	/* Now set he frame values independent of the display frame. */
	fb = &xim->frames[frame];
	fb->xoff = xoff;
	fb->yoff = yoff;

	return (TCL_OK);
}



/* clearFrame -- Clear the given frame, or the current frame in no frame
 * number is given.
 *
 * Usage:	clearFrame [frame]
 */
static int 
clearFrame (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	int frame;

	if (argc > 1)
	    frame = atoi (argv[1]);
	else
	    frame = xim->display_frame;

	xim_eraseFrame (xc->xim, frame);
	return (TCL_OK);
}


/* fitFrame -- Attempt to make the display window the same size as the frame
 * buffer.
 *
 * Usage:	fitFrame
 */
static int 
fitFrame (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;

	xim_fitFrame (xc->xim);
	return (TCL_OK);
}


/* setOption -- Set an ximtool client option.
 *
 * Usage:	setOption option value [args]
 *
 * Options:
 *	autoscale	true|false
 *	antialias	true|false [type]
 *	tileFrames	true|false [frames]
 *	tileByRow	true|false
 *	tileTopDown	true|false
 *	tileGeom	type|geom  [frames]
 *	cmfocus		size
 */
char *h_orient[] = {
    "1x1","2x1","3x1","2x2", "3x2","3x2","4x2","4x2", "3x3","5x2","4x3","4x3"
};
char *v_orient[] = {
    "1x1","1x2","1x3","2x2", "2x3","2x3","2x4","2x4", "3x3","2x5","3x4","3x4"
};

static int 
setOption (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	char *option, *strval, **items;
	char buf[SZ_LINE];
	int ch, value, nx, ny, nitems, i, frame_list=0;


	if (argc < 3)
	    return (TCL_ERROR);
	else {
	    option = argv[1];
	    strval = argv[2];

	    if (strcmp (option, "tileGeom") != 0) {
	        ch = strval[0];
		if (isdigit (ch))
		    value = atoi (strval);
	        else if (ch == 'T' || ch == 't')
		    value = 1;
	        else if (ch == 'F' || ch == 'f')
		    value = 0;
	    }
	}

	if (strcmp (option, "autoscale") == 0) {
	    if (xim->autoscale != value) {
		xim->autoscale = value;
		xim_resize (xim, xim->gt);
		sprintf (buf, "%s", value ? "True" : "False");
		xim_message (xim, "autoscale", buf);
	    }
	} else if (strcmp (option, "antialias") == 0) {
	    if (xim->antialias != value) {
		xim->antialias = value;
		if (value) {
		    if (argc > 3)
			xim->rop = xim_getAntialias (xim, argv[3]);
		    else
			xim->rop = xim_getAntialias (xim, xim->antialiasType);
		} else
		    xim->rop = 0;
		xim_setRop (xim, fb, xim->rop);
		sprintf (buf, "%s", value ? "True" : "False");
		xim_message (xim, "antialias", buf);
	    }

	} else if (strcmp (option, "cmfocus") == 0) {
	    if (xim->cm_focus != value) {
		int box_size = value;
		xim->cm_focus = box_size;
		GtSetColormapFocus (box_size);
	    }

	} else if (strcmp (option, "tileFrames") == 0) {
	    if (xim->tileFrames != value) {
		/* Get list of frames to be tiled. */
		if (argc > 3) {
		    if (Tcl_SplitList (tcl, argv[3], &nitems, &items) != TCL_OK)
			goto nolist1;
		    for (i=0;  i < nitems;  i++)
			frame_list |= (1 << (atoi(items[i]) - 1));
		    XtFree ((char *)items);
		} else {
nolist1:	    for (i=0;  i < xim->nframes;  i++)
			frame_list |= (1 << i);
		    nitems = xim->nframes;
		}

		/* Set or clear tile frame mode. */
		xim_tileFrames (xim, value ? frame_list : 0);
	    }

	} else if (strcmp (option, "tileByRows") == 0) {
	    xim->tileByRows = value;
	    xim_tileFrames (xim, xim->tileFramesList);

	} else if (strcmp (option, "tileTopDown") == 0) {
	    xim->tileTopDown = value;
	    xim_tileFrames (xim, xim->tileFramesList);

	} else if (strcmp (option, "tileLabels") == 0) {
	    xim->tileLabels = value;
	    xim_tileFrames (xim, xim->tileFramesList);

	} else if (strcmp (option, "tileGeom") == 0) {
	    /* Get list of frames to be tiled. */
	    if (argc > 3) {
	        if (Tcl_SplitList (tcl, argv[3], &nitems, &items) != TCL_OK)
	    	    goto nolist2;
	        for (i=0;  i < nitems;  i++)
	    	    frame_list |= (1 << (atoi(items[i]) - 1));
	        XtFree ((char *)items);
	    } else {
nolist2:        for (i=0;  i < xim->nframes;  i++)
	    	    frame_list |= (1 << i);
		nitems = xim->nframes;
	    }
	    nitems = max (nitems, 1);


	    /* Get the option or tile geometry. */
	    if (strcmp (strval, "Best") == 0) {
		if (xim->width < xim->height)
		    goto horient;
		else
		    goto vorient;
	    } else if (strcmp (strval, "Square") == 0) {
		for (i=0; (i*i) < nitems; i++)
		    ;
		nx = ny = i;
	    } else if (strcmp (strval, "Horizontal") == 0) {
horient:	if (nitems >= 13)
		    nx = ny = 4;
		else
		    sscanf (h_orient[nitems-1], "%dx%d", &nx, &ny);
	    } else if (strcmp (strval, "Vertical") == 0) {
vorient:	if (nitems >= 13)
		    nx = ny = 4;
		else
		    sscanf (v_orient[nitems-1], "%dx%d", &nx, &ny);
	    } else if (strcmp (strval, "Row") == 0) {
		nx = nitems;
		ny = 1;
	    } else if (strcmp (strval, "Column") == 0) {
		nx = 1;
		ny = nitems;
	    } else {
		sscanf (strval, "%dx%d", &nx, &ny);
	    }

	    /* Set or clear tile frame mode. */
	    xim->tileRows = ny;
	    xim->tileCols = nx;
	    xim->tileFramesList = frame_list;
	    sprintf (buf, "%d %d", nx, ny);
	    xim_message (xim, "tileOptions", buf);

	    xim_tileFrames (xim, xim->tileFramesList);
	}

	return (TCL_OK);
}


/* setColormap -- Set the colormap for the current display frame.
 *
 * Usage:	setColormap <colormap>
 *
 * The colormap may be specified either by number or by name.
 */
static int 
setColormap (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	unsigned short m_red[MAX_COLORS];
	unsigned short m_green[MAX_COLORS];
	unsigned short m_blue[MAX_COLORS];
	char buf[SZ_LINE];
	ColorMapPtr cm;
	int i;


	if (argc == 2) {
	    if (isdigit (*argv[1]))
		i = atoi(argv[1]);
	    else {
		for (i=1;  i <= ncolormaps;  i++)
		    if (strcmp (colormaps[i-1].name, argv[1]) == 0)
			break;
	    }

	    if (i >= 1 && i <= ncolormaps) {
		cm = &colormaps[i-1];
		if (strncmp (cm->name, "Random", 6) == 0) {
		    xim_setColormap (cm->name, NULL,
			m_red, m_green, m_blue, xim->ncolors);
		    GtWriteColormap (xim->gt, cm->mapno,
			first_color, xim->ncolors, m_red, m_green, m_blue);
		}

		fb->colormap = i;
	    	GtSetColormapFocus (-1);	/* force full update	*/
		GtLoadColormap (xim->gt, cm->mapno, fb->offset, fb->scale);
	    	GtSetColormapFocus (xim->cm_focus);
		xim_enhancement (xim, fb);
	    }
	}

	return (TCL_OK);
}


/* windowColormap -- Set the colormap for the current display frame.
 *
 * Usage:	windowColormap <offset> <scale>
 */
static int 
windowColormap (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	register ColorMapPtr cm;
	char buf[SZ_LINE];

	if (argc > 1) {
	    cm = &colormaps[fb->colormap-1];
	    fb->offset = atof(argv[1]);
	    fb->scale = (argc > 2) ? (float)atof(argv[2]) : fb->scale;
	    GtLoadColormap (xim->gt, cm->mapno, fb->offset, fb->scale);
	    xim_enhancement (xim, fb);
	}

	return (TCL_OK);
}


/* updateColormap -- Update the colormap for the entire display frame.
 *
 * Usage:	updateColormap <offset> <scale>
 */
static int 
updateColormap (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	register ColorMapPtr cm;
	char buf[SZ_LINE];

	if (argc > 1) {
	    cm = &colormaps[fb->colormap-1];
	    fb->offset = atof(argv[1]);
	    fb->scale = (argc > 2) ? (float)atof(argv[2]) : fb->scale;
	    GtSetColormapFocus (-1);	/* force full update	*/
	    GtLoadColormap (xim->gt, cm->mapno, fb->offset, fb->scale);
	    GtSetColormapFocus (xim->cm_focus);
	    xim_enhancement (xim, fb);
	}

	return (TCL_OK);
}


/* zoom -- Set the zoom factors for the current frame to the given values.
 * A zoom factor > 1 enlarges the image, < 1 shrinks the image, 1.0 maps
 * one source pixel to one destination pixel.
 *
 * Usage:	zoom <xymag>				1 argument
 *		zoom <xmag> <ymag>			2 arguments
 *		zoom <xmag> <ymag> <xcen> <ycen>	4 arguments
 *
 * When called as "zoom" the magnification is relative to the fixed scaling,
 * if any, used to scale the frame to fit the display window at mag=1.0.
 * When called as zoomAbs" the magnification given is the actual scale factor
 * used to map raster pixels to display pixels.
 */
static int 
zoom (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	float xmag, ymag;
	float xcen, ycen;
	float xoff, yoff;
	Boolean absolute;

	xmag = fb->xmag;
	ymag = fb->ymag;
	xcen = fb->xcen;
	ycen = fb->ycen;
	xoff = fb->xoff;
	yoff = fb->yoff;

	switch (argc) {
	case 7:
	    xoff = atof (argv[5]);
	    yoff = atof (argv[6]);
	    /* fall through */
	case 5:
	    xcen = atof (argv[3]);
	    ycen = atof (argv[4]);
	    /* fall through */
	case 3:
	    xmag = atof (argv[1]);
	    ymag = atof (argv[2]);
	    break;
	case 2:
	    xmag = ymag = atof (argv[1]);
	    break;
	}

	absolute = (strcmp (argv[0], "zoomAbs") == 0);
	xim_setZoom (xim, fb, fb->frameno, fb->zoommap,
	    fb->raster, fb->zoomras, xcen, ycen, xmag, ymag, xoff, yoff,
	    absolute);

	return (TCL_OK);
}


/* pan -- Pan the current frame, i.e., change the view center.
 *
 * Usage:	pan <xcen> <ycen>
 */
static int 
pan (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	Boolean absolute = False;
	float xmag, ymag;
	float xcen, ycen;
	float xoff, yoff;
	double atof();

	xmag = fb->xmag;
	ymag = fb->ymag;
	xoff = fb->xoff;
	yoff = fb->yoff;

	if (argc == 3) {
	    xcen = atof (argv[1]);
	    ycen = atof (argv[2]);

	    xim_setZoom (xim, fb, fb->frameno, fb->zoommap,
		fb->raster, fb->zoomras, xcen, ycen, xmag, ymag, xoff, yoff,
		absolute);
	}

	return (TCL_OK);
}


/* centroid -- Center the cursor on the feature given an initial position
 * and box size.  Return a correction to the center.
 *
 * Usage:	centroid <xcen> <ycen> <size> [ <type> ]
 */
static int 
centroid (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr  xim = xc->xim;
	register FrameBufPtr fb  = xim->df_p;
        register CtranPtr    ct  = (CtranPtr) &fb->ctran;
        unsigned char junk[MAX_COLORS];
	unsigned char *pix = NULL;
	float 	 *data=NULL, *xm=NULL, *ym=NULL;
	float    xsum, ysum, xsumx, ysumx;
	float    cx, cy, lo, hi, px, xcen, ycen, size;
	float    xmin=999999.0, xmax=-999999.0;
	int	 w, h, ncolors, i, j;
	int	 dist=0, mind=99999, maxd=99999, imin=0, imax=0;
	int	 x0, y0, nx, npix, min_max=-1;
 	char     buf[SZ_LINE];
	double   atof();


	if (argc < 4)
	    return (TCL_ERROR);

	xcen = atof (argv[1]);
	ycen = atof (argv[2]);
	size = atof (argv[3]);
	if (argc == 5)
	    min_max = strcmp (argv[4],"max");

	x0 = xcen - size;
	y0 = ycen - size;
	nx = size * 2 + 1;
	npix = nx * nx;

	/* Read the display raster. */
	pix = xim_readDisplay (xim, x0,y0,nx,nx, &w,&h, junk,junk,junk,
		 &ncolors);

	/* Scale the data to the WCS pixel values for centroiding. */
	data = (float *) XtMalloc (npix * sizeof(float));
	for (i=0; i < npix; i++) {
            if (pix[i] == 0) {
                data[i] = 0.0;
            } else {
                if (ct->zt == W_LINEAR) {
                    data[i] = ((pix[i]-1) * (ct->z2 - ct->z1) / 199) + ct->z1;
                    data[i] = max (ct->z1, min (ct->z2, data[i]));
                } else
                    data[i] = (float) pix[i];
            }
	    cx = (nx / 2) - (i % nx);
	    cy = (nx / 2) - (i / nx);
	    dist = (int) (sqrt (cx*cx + cy*cy) + 0.5);

	    if (data[i] > xmax)
		xmax = data[i], maxd = dist, imax = i;
	    else if (data[i] == xmax && dist < maxd)
		maxd = dist, imax = i;

	    if (data[i] < xmin)
		xmin = data[i], mind = dist, imin = i;
	    else if (data[i] == xmin && dist < mind)
		mind = dist, imin = i;
	}
	XtFree ((char *)pix);

	if (min_max >= 0) {
	    if (min_max == 0) {
	        if (data[npix/2] == xmax)
	            sprintf (buf, "0 0");
		else
	            sprintf (buf, "%g %g", (imax%nx)-size, (imax/nx)-size);
	    } else {
	        if (data[npix/2] == xmin)
	            sprintf (buf, "0 0");
		else
	            sprintf (buf, "%g %g", (imin%nx)-size, (imin/nx)-size);
	    }

	    /* Return the correction to the position. */
	    Tcl_SetResult (xc->server, buf, TCL_VOLATILE);
	    return (TCL_OK);
	}


	/* Find the low threshold for the subraster (i.e. the mean). */
	lo = hi = xsum = data[0];
	for (i=1; i < npix ; i++) {
	    xsum += data[i];
	    lo = (data[i] < lo ? data[i] : lo);
	    hi = (data[i] > hi ? data[i] : hi);
	}

	/* Check for a raster with all the same pixels, in which case
	 * just return a zero offset.
	 */
	if (lo == hi) {
	    sprintf (buf, "0 0");
	    Tcl_SetResult (xc->server, buf, TCL_VOLATILE);
	    return (TCL_OK);

	} else
	    lo   = xsum / (float)npix; 

	/* Accumulate the x and y marginals. */
	xm = (float *) XtMalloc (nx * sizeof(float));
	ym = (float *) XtMalloc (nx * sizeof(float));
	for (i=0; i < nx; i++) {
	    xsum = xm[i] = 0.0, ysum = ym[i] = 0.0;
	    for (j=0; j < nx; j++) {
		px = data[(j*nx)+i];			/* column sum 	*/
		if (lo <= px)
		    xsum += px - lo;

		px = data[(i*nx)+j];			/* row sum 	*/
		if (lo <= px)
		    ysum += px - lo;
	    }
	    xm[i] = xsum;
	    ym[i] = ysum;
	}
	XtFree ((char *)data);

	/* Now calculate the centroids as the first moment.  If all the 
	 * marginals are zero (i.e. all pixels the same) then return a
	 * zero correction. 
	 */
	xsum = xsumx = 0.0, ysum = ysumx = 0.0;
	px = (float) nx;
	for (i=0; i < nx; i++) {
            xsum  += (xm[i] / px);
            xsumx += (xm[i] / px) * i;
	
            ysum  += (ym[i] / px);
            ysumx += (ym[i] / px) * i;
	}
	cx = (xsum == 0.0) ? size : xsumx / xsum;
	cy = (ysum == 0.0) ? size : ysumx / ysum;

	XtFree ((char *)xm); 		/* clean up */
	XtFree ((char *)ym);

	/* Return the correction to the position. */
	sprintf (buf, "%d %d", nint(cx-size), nint(cy-size));
	Tcl_SetResult (xc->server, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* getPixels -- Get an array of pixels around the given center position.
 *
 * Usage:	getPixels <x0> <y0> <nx> <ny> [format [scale]]
 */

#define PF_NONE		0		/* don't format the output pixels */
#define PF_PIXTAB	1		/* format for pixel table 	  */
#define PF_HCUT		2		/* format for horizontal cut-plot */
#define PF_VCUT		3		/* format for vertical cut-plot   */

static int 
getPixels (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr  xim = xc->xim;
	register FrameBufPtr fb  = xim->df_p;
        register CtranPtr    ct  = (CtranPtr) &fb->ctran;
	register int	 i, j, k, l;
	unsigned char *pix = NULL;
	char 	 *buf = NULL, ch, val[32];
	float 	 *data = NULL;
	float	 z1 = ct->z1, z2 = ct->z2;
	float    scale=1.0, yscale = 128.0 / (z2 - z1);
	int	 x0, y0, nx, ny, format=0, npix, sample=1, raster=0;
	double   atof();


	if (argc < 5)
	    return (TCL_ERROR);

	x0 = atof (argv[1]);
	y0 = atof (argv[2]);
	nx = atof (argv[3]);
	ny = atof (argv[4]);
	if (argc >= 6) {
	    if (isdigit ((ch = *argv[5])))
		format = atoi (argv[5]);
	    else if (ch == 'T' || ch == 't')
		format = 1;
	    else
		format = 0;
	}
	if (argc >= 7) 
	    sample = (isdigit(*argv[6]) ? atoi (argv[6]) : 1);
	if (argc == 8)
	    scale = atof (argv[7]);
	
	npix = nx * ny;


	/* Read the display raster. */
	pix = (unsigned char *) XtMalloc (npix);
        if (GtReadPixels (xim->gt, raster, pix, 8, x0, y0, nx, ny) < 0)
	    return (TCL_ERROR);

	/* Scale the data to the WCS pixel values for display.  We don't
	 * get here if an ISM is running that provides access to the real
	 * pixel values.
	 */
	data = (float *) XtCalloc (npix, sizeof(float));
	for (i=0; i < npix; i+=sample) {
	    if (pix[i] == 0) {
	        data[i] = 0.0;
	    } else {
		if (ct->zt == W_LINEAR) {
	    	    data[i] = ((pix[i]-1) * (z2 - z1) / 199) + z1;
	    	    data[i] = max (z1, min (z2, data[i]));
		} else
	            data[i] = (float) pix[i];
	    }
	    if (format > 1) { data[i] = (z2 - data[i]) * yscale; }
	}
	XtFree ((char *)pix);

        /* Get a text buffer large enough to hold the encoded data. */
        if (!(buf = (char *) XtMalloc ((npix + 4) * 30))) {
            XtFree ((char *)data);
            return (TCL_ERROR);
        }

        /* Encode the data as {ddd} {ddd} {ddd}...{ddd}.  The first four
	 * elements are the zscale values and the array min/max.
	 */
	strcpy (buf, "");
	sprintf (val, "{%10.1f} ", z1);   strcat (buf, val);
	sprintf (val, "{%10.1f} ", z2);   strcat (buf, val);

	if (format == PF_PIXTAB) {
	    for (i=0; i < npix; i++) {
	    	sprintf (val, "{%10.1f%c} ", data[i],
	        	(data[i] <= z1 ? '-' : (data[i] >= z2 ? '+' : ' ')) );
		strcat (buf, val);
	    }
	} else if (format == PF_HCUT) {
	    for (i=0; i < npix; i+=sample) {
	    	sprintf (val, "{%g %g} ", i * scale, data[i]);
		strcat (buf, val);
	    }
	} else if (format == PF_VCUT) {
	    for (i=0; i < npix; i+=sample) {
	    	sprintf (val, "{%g %g} ", data[i], i * scale);
		strcat (buf, val);
	    }
	} else {
	    for (i=0; i < npix; i++) {
	    	sprintf (val, "{%f} ", data[i]);
	    	strcat (buf, val);
	    }
	}
            
        Tcl_SetResult (xc->server, buf, TCL_VOLATILE);
        XtFree ((char *)data);
        XtFree ((char *)buf);

	return (TCL_OK);
}


/* flip -- Flip the current display frame in the indicated axis or axes.
 *
 * Usage:	flip [axis [axis ...]]
 */
static int 
flip (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	int flip_x = 0, flip_y = 0;
	int ch, i;

	for (i=1;  i < argc;  i++) {
	    ch = argv[i][0];
	    if (ch == 'x' || ch == 'X')
		flip_x = !flip_x;
	    else if (ch == 'y' || ch == 'Y')
		flip_y = !flip_y;
	}

	xim_setFlip (xim, fb, flip_x, flip_y);
	return (TCL_OK);
}


/* retCursorVal -- Return a cursor value to the ximtool client process.  This
 * should be executed by the GUI to terminate a cursor read.
 *
 * Usage:	retCursorVal sx sy [frame [wcs [key [strval]]]]
 */
static int 
retCursorVal (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	int frame, wcs, key;
	float sx, sy;
	char *s, *strval;

	if (argc < 3)
	    return (TCL_ERROR);

	sx = atof (argv[1]);
	sy = atof (argv[2]);
	frame = (argc > 3) ? atoi (argv[3]) : xim->display_frame;
	wcs = (argc > 4) ? atoi (argv[4]) : 1;

	if (argc > 5) {
	    s = argv[5];
	    if (s[0] == '^')
		key = s[1] - 'A' + 1;
	    else
		key = s[0];
	} else
	    key = 0;

	strval = (argc > 6) ? argv[6] : "";

	xim_retCursorVal (xim, sx, sy, frame, wcs, key, strval);

	return (TCL_OK);
}


/* encodewcs -- Convert raw screen coordinates x,y,z (z=pixel value) to
 * world coordinates using the WCS passed to ximtool by the client application
 * when the frame was loaded.  The encoded description of the current position
 * and pixel value is returned to the GUI as a string value.
 *
 * Usage:	string = encodewcs sx sy sz
 */
static int 
encodewcs (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	char buf[SZ_LINE];
	float sx, sy;
	int sz;

	if (argc < 3)
	    return (TCL_ERROR);

	sx = atof (argv[1]);
	sy = atof (argv[2]);
	sz = (argc > 3) ? atoi (argv[3]) : 0;

	xim_encodewcs (xc->xim, sx, sy, sz, buf);
	Tcl_SetResult (xc->server, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* setPrintOption -- Set an ximtool client hardcopy option.
 *
 * Usage:	setPrintOption option value [args]
 *
 * Options:
 *	autoscale	true|false
 *	autorotate	true|false
 *	maxaspect	true|false
 *	annotate	true|false
 *	compress	true|false
 *
 * 	orientation	portrait|landscape
 *	papersize	letter|legal|A4|B5
 *	imscale		value
 *
 *	colortype	gray|pseudo|rgb
 *	printername	strval
 *	devicetype	printer|file
 *
 *	printcmd	command
 *	printfile	filename
 *
 *	dotitle		true|false
 *	doborders	true|false
 *	docolorbars	true|false
 *	title		string
 *
 *	corners		llx lly urx ury
 */
static int 
setPrintOption (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	register PSImagePtr psim = xim->psim;
	register PrintCfgPtr pcp = xim->pcp;
	register PrinterPtr prp;
	register int i;
	int	 llx, lly, urx, ury;
	char *option, strval[SZ_LINE];
	char buf[SZ_LINE];
	int pnum, ch, value, psflags = psim->page.flags;

	if (argc < 3)
	    return (TCL_ERROR);
	else {
	    option = argv[1];
	    strcpy (strval, argv[2]);
	    for (i=3; i < argc; i++) {
		strcat (strval, " ");
		strcat (strval, argv[i]);
	    }

	    ch = strval[0];
	    if (isdigit (ch))
		value = atoi (strval);
	    else if (ch == 'T' || ch == 't')
		value = 1;
	    else if (ch == 'F' || ch == 'f')
		value = 0;
	}

	if (strcmp (option, "autoscale") == 0) { 		/* AUTOSCALE */
		psflags = value ? psflags | EPS_AUTOSCALE :
				  psflags & ~EPS_AUTOSCALE ;
		psim->page.flags = psflags;
		sprintf (buf, "%s %s", option, value ? "True" : "False");
		xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "autorotate") == 0) {	/* AUTOROTATE */
		psflags = value ? psflags | EPS_AUTOROTATE :
				  psflags & ~EPS_AUTOROTATE ;
		psim->page.flags = psflags;
		sprintf (buf, "%s %s", option, value ? "True" : "False");
		xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "maxaspect") == 0) {		/* MAXASPECT */
		psflags = value ? psflags | EPS_MAXASPECT :
				  psflags & ~EPS_MAXASPECT ;
		psim->page.flags = psflags;
		sprintf (buf, "%s %s", option, value ? "True" : "False");
		xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "dotitle") == 0) {		/* TITLE */
		psflags = value ? psflags | EPS_DOTITLE :
				  psflags & ~EPS_DOTITLE ;
		psim->page.flags = psflags;
		sprintf (buf, "%s %s", option, value ? "True" : "False");
		xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "doborders") == 0) {		/* BORDERS */
		psflags = value ? psflags | EPS_DOBORDERS :
				  psflags & ~EPS_DOBORDERS ;
		psim->page.flags = psflags;
		sprintf (buf, "%s %s", option, value ? "True" : "False");
		xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "docolorbar") == 0) {	/* COLORBAR */
		psflags = value ? psflags | EPS_DOCOLORBAR :
				  psflags & ~EPS_DOCOLORBAR ;
		psim->page.flags = psflags;
		sprintf (buf, "%s %s", option, value ? "True" : "False");
		xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "title") == 0) {      	/* TITLE STR */
	    if (strcmp ("imtitle", strval) != 0) {
	        strcpy (psim->label, strval);
                sprintf (buf, "title %s", strval);
                xim_message (xim, "printOptions", buf);
	    }

	} else if (strcmp (option, "annotate") == 0) {		/* ANNOTATE */
	    if (value) {
		if (!psim->label)
		    psim->label = (char *) calloc (SZ_LINE, sizeof (char));
        	sprintf (psim->label, "[Frame %d] %s", 
		    fb->frameno, fb->ctran.imtitle);
	 	psim->annotate = 1;
	    } else {
		if (psim->label) {
		    XtFree ((char *)psim->label);
		    psim->label = NULL;
		}
	 	psim->annotate = 0;
	    }
	    sprintf (buf, "%s %s", option, value ? "True" : "False");
	    xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "compress") == 0) {	      /* COMPRESS    */
	    if (value)
		psim->compression = RLECompression;
	    else
		psim->compression = NoCompression;
	    sprintf (buf, "%s %s", option, value ? "True" : "False");
	    xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "orientation") == 0) {     /* ORIENTATION */
	    if (ch == 'P' || ch == 'p')
		psim->page.orientation = EPS_PORTRAIT;
	    else if (ch == 'L' || ch == 'l')
		psim->page.orientation = EPS_LANDSCAPE;
	    sprintf (buf, "%s %s", option, strval);
	    xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "papersize") == 0) {	       /* PAPER SIZE */
	    if (strval[2] == 'T' || strval[2] == 't')
		psim->page.page_type = EPS_LETTER;
	    else if (strval[2] == 'G' || strval[2] == 'g')
		psim->page.page_type = EPS_LEGAL;
	    else if (strval[0] == 'A' || strval[0] == 'a')
		psim->page.page_type = EPS_A4;
	    else if (strval[0] == 'B' || strval[0] == 'b')
		psim->page.page_type = EPS_B5;
	    sprintf (buf, "%s %s", option, strval);
	    xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "imscale") == 0) {	       /* IMAGE SCALE */
	    if (value >= 10) {
	        if ((int)(psim->page.scale*100.0) != value)
	            psim->page.scale = (float) value / 100.0;
	        sprintf (buf, "%s %d", option, value);
	        xim_message (xim, "printOptions", buf);
	    }

	} else if (strcmp (option, "colortype") == 0) {		/* COLORTYPE */
	    if (ch == 'G' || ch == 'g')
		psim->colorClass = EPS_GRAYSCALE;
	    else if (ch == 'P' || ch == 'p')
		psim->colorClass = EPS_PSEUDOCOLOR;
	    else if (ch == 'R' || ch == 'r')
		psim->colorClass = EPS_TRUECOLOR;
	    sprintf (buf, "%s %s", option, strval);
	    xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "devicetype") == 0) {       /* DEVICETYPE */

	    if (strval[0] == 'p' || strval[0] == 'P') {
		pcp->diskfile = 0;
	        sprintf (buf, "deviceType Printer");
	        xim_message (xim, "printOptions", buf);
        	sprintf (buf, "printCmd %s", pcp->printCmd);
        	xim_message (xim, "printOptions", buf);
        	sprintf (buf, "printerName %d", pcp->printno);
        	xim_message (xim, "printOptions", buf);

	    } else if (strval[0] == 'f' || strval[0] == 'F') {
		pcp->diskfile = 1;
	        sprintf (buf, "deviceType File");
	        xim_message (xim, "printOptions", buf);
        	sprintf (buf, "printFile %s", pcp->printFile);
        	xim_message (xim, "printOptions", buf);
	    }

	} else if (strcmp (option, "printername") == 0) {    /* PRINTER NAME */
	    /* Set to printer mode if called in file mode. */
	    if (pcp->diskfile) {
		pcp->diskfile = 0;
	        sprintf (buf, "deviceType Printer");
	        xim_message (xim, "printOptions", buf);
	    }
	    pnum = xim_getPrinterInfo (xim, strval);
	    sprintf (buf, "printerName %s", strval);
	    xim_message (xim, "printOptions", buf);
	    strcpy (pcp->printCmd, printer_list[pnum].printCmd);
	    sprintf (buf, "printCmd %s", pcp->printCmd);
	    xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "printcmd") == 0) {      /* PRINT COMMAND */
	    strcpy (pcp->printCmd, strval);
            sprintf (buf, "printCmd %s", strval);
            xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "printfile") == 0) {    /* PRINT FILENAME */
	    strcpy (pcp->printFile, strval);
            sprintf (buf, "printFile %s", strval);
            xim_message (xim, "printOptions", buf);

	} else if (strcmp (option, "corners") == 0) {      /* IMAGE CORNERS */
	    /* Set the corners of the image being printed. */
	    sscanf (strval, "%d %d %d %d", &llx, &lly, &urx, &ury);
	    eps_setCorners (psim, llx, lly, urx, ury);
	}

        /* Reload the page parameters in case anything's changed. */
        eps_setPage (psim, psim->page.orientation, psim->page.page_type,
            (int)(psim->page.scale*100), psim->page.flags);

	return (TCL_OK);
}


/* setSaveOption -- Set an ximtool client disk file option.
 *
 * Usage:       setSaveOption option value [args]
 *
 * Options:
 *
 *	format 	fmt
 *	color	gray|pseudo|rgb
 *	fname	strval
 */
static int
setSaveOption (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
        register XimDataPtr xim = xc->xim;
        register FrameBufPtr fb = xim->df_p;
	register fileSavePtr fsp = xim->fsp;
	register int i;
        char *option, strval[SZ_LINE];
        char buf[SZ_LINE];
        int ch, value;

        if (argc < 3)
            return (TCL_ERROR);
        else {
            option = argv[1];
	    strcpy (strval, argv[2]);
	    for (i=3; i < argc; i++) {
		strcat (strval, " ");
		strcat (strval, argv[i]);
	    }


            ch = strval[0];
            if (isdigit (ch))
                value = atoi (strval);
            else if (ch == 'T' || ch == 't')
                value = 1;
            else if (ch == 'F' || ch == 'f')
                value = 0;
        }

	if (strcmp (option, "format") == 0) {	       		/* FORMAT */
	    switch (strval[0]) {
	    case 'r':
		if (strval[2] == 's') {
		    fsp->format = XIM_RAS;
		    strcpy (fsp->fname, "frame%d.ras");
		} else if (strval[2] == 'w') {
		    fsp->format = XIM_RAW;
		    strcpy (fsp->fname, "frame%d.raw");
		}
		break;
	    case 'g': 	
		fsp->format = XIM_GIF; 	
		strcpy (fsp->fname, "frame%d.gif");
		break;
	    case 'j': 	
		fsp->format = XIM_JPEG; 
		strcpy (fsp->fname, "frame%d.jpg");
		break;
	    case 't': 	
		fsp->format = XIM_TIFF; 
		strcpy (fsp->fname, "frame%d.tiff");
		break;
	    case 'f': 	
		fsp->format = XIM_FITS; 
		strcpy (fsp->fname, "frame%d.fits");
		break;
	    case 'e': 	
		fsp->format = XIM_EPS; 	
		strcpy (fsp->fname, "frame%d.eps");
		break;
	    case 'x': 	
		fsp->format = XIM_X11; 	
		strcpy (fsp->fname, "frame%d.xwd");
		break;
	    }
	    sprintf (buf, "%s %s", option, strval);
	    xim_message (xim, "saveOptions", buf);
            sprintf (buf, "fname %s", fsp->fname);
            xim_message (xim, "saveOptions", buf);

	} else if (strcmp (option, "color") == 0) {	      	/* COLOR */
            if (ch == 'G' || ch == 'g')
                fsp->colorType = XIM_GRAYSCALE;
            else if (ch == 'P' || ch == 'p')
                fsp->colorType = XIM_PSEUDOCOLOR;
            else if (ch == 'R' || ch == 'r')
                fsp->colorType = XIM_RGB;
	    sprintf (buf, "%s %s", option, strval);
	    xim_message (xim, "saveOptions", buf);

	} else if (strcmp (option, "fname") == 0) {	       	/* FILENAME */
	    strcpy (fsp->fname, strval);
	    sprintf (buf, "%s %s", option, strval);
	    xim_message (xim, "saveOptions", buf);
	}

	return (TCL_OK);
}


/* setLoadOption -- Set an ximtool client disk file option.
 *
 * Usage:       setLoadOption option value [args]
 *
 * Options:
 * 	up
 *	root
 *	home
 * 	rescan
 * 	headers
 *	pattern	patstr
 *	gray    0|1
 *	zscale  0|1
 *	zrange  0|1
 *	z1 value
 *	z2 value
 *	nsample value
 *
 */
static int
setLoadOption (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register int i;
        register XimDataPtr xim = xc->xim;
	register fileLoadPtr flp = xim->flp;
        char *ip, *op, *option, *strval;
        char buf[SZ_LINE];

        if (argc < 2)
            return (TCL_ERROR);
        else {
            option = argv[1];
            strval = (argc == 3 ? argv[2] : "" );
        }

	if (strcmp (option, "up") == 0) {	       		/* UP      */
	    if (strcmp("/", flp->curdir) != 0) {
		for (i=strlen(flp->curdir); i > 1; i--) {
		    if (flp->curdir[i] == '/')
			break;
		}
		flp->curdir[i] = '\0';
	        sprintf (buf, "curdir %s", flp->curdir);
	        xim_message (xim, "loadOptions", buf);
	        xim_dirRescan (xim);
	    }

	} else if (strcmp (option, "root") == 0) {	      	/* ROOT    */
	    strcpy (flp->curdir, "/");
	    sprintf (buf, "curdir %s", flp->curdir);
	    xim_message (xim, "loadOptions", buf);
	    xim_dirRescan (xim);

	} else if (strcmp (option, "home") == 0) {	      	/* HOME    */
	    strcpy (flp->curdir, flp->homedir);
	    sprintf (buf, "curdir %s", flp->curdir);
	    xim_message (xim, "loadOptions", buf);
	    xim_dirRescan (xim);

	} else if (strcmp (option, "pattern") == 0) {	       	/* PATTERN */
	    if (strcmp(strval, flp->pattern) != 0) {
	        strcpy (flp->pattern, strval);
	        sprintf (buf, "pattern %s", flp->pattern);
	        xim_message (xim, "loadOptions", buf);
	        xim_dirRescan (xim);
	    }

	} else if (strcmp (option, "rescan") == 0) {	       	/* RESCAN  */
	    xim_dirRescan (xim);

	} else if (strcmp (option, "headers") == 0) {	       	/* HEADERS */
	    xim_scanHeaders (xim);

	} else if (strcmp (option, "gray") == 0) {	      	/* GRAY    */
	    flp->gray = (strval[0] == '0' ? 0 : 1);
	    sprintf (buf, "gray %s", strval[0] == '0' ? "off" : "on");
	    xim_message (xim, "loadOptions", buf);

	} else if (strcmp (option, "zscale") == 0) {	      	/* ZSCALE  */
	    flp->zscale = (strval[0] == '0' ? 0 : 1);
	    sprintf (buf, "zscale %s", strval[0] == '0' ? "off" : "on");
	    xim_message (xim, "loadOptions", buf);

	} else if (strcmp (option, "zrange") == 0) {	      	/* ZRANGE  */
	    flp->zrange = (strval[0] == '0' ? 0 : 1);
	    sprintf (buf, "zrange %s", strval[0] == '0' ? "off" : "on");
	    xim_message (xim, "loadOptions", buf);

	} else if (strcmp (option, "z1") == 0) {	      	/* Z1      */
	    sscanf (argv[2], "%g", &(flp->z1));
	    sprintf (buf, "z1 %s", argv[2]);
	    xim_message (xim, "loadOptions", buf);

	} else if (strcmp (option, "z2") == 0) {	      	/* Z2      */
	    sscanf (argv[2], "%g", &(flp->z2));
	    sprintf (buf, "z2 %s", argv[2]);
	    xim_message (xim, "loadOptions", buf);

	} else if (strcmp (option, "nsample") == 0) {	      	/* NSAMPLE */
	    sscanf (argv[2], "%d", &(flp->nsample));
	    sprintf (buf, "nsample %s", argv[2]);
	    xim_message (xim, "loadOptions", buf);
	}

	return (TCL_OK);
}


/* Print -- Print the current display frame to a printer or to a file (EPS).
 *
 * Usage:       print [x0 y0 nx ny]
 *
 *		print rename old new
 *		print cancel fname
 *
 * If a subregion is given the indicated region is printed, otherwise the
 * full display frame is printed.
 *
 * The forms "print rename" and "print cancel" are actions for print alerts.
 */
static int
print (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
        register XimDataPtr xim = xc->xim;
        int x0, y0, nx, ny;

        /* Handle the special cases first. */
        if (argc == 4 && strcmp (argv[1], "rename") == 0) {
	    ximp_rename (xim, argv[2], argv[3]);
	    return (TCL_OK);
        } else if (argc == 3 && strcmp (argv[1], "cancel") == 0) {
	    ximp_cancel (xim, argv[2]);
	    return (TCL_OK);
        }

	/* Normal case of a print. */
	if (argc == 5) {
	    x0 = atoi (argv[1]);
	    y0 = atoi (argv[2]);
	    nx = atoi (argv[3]);
	    ny = atoi (argv[4]);
	} else
	    x0 = y0 = nx = ny = 0;

	if (xim_print (xim, x0,y0, nx,ny) < 0)
	    return (TCL_ERROR);
	else
	    return (TCL_OK);
}


/* Save -- Save the current display frame to a disk file.
 *
 * Usage:       save [x0 y0 nx ny]
 *
 *		save rename old new
 *		save cancel fname
 *
 * If a subregion is given the indicated region is saved, otherwise the
 * full display frame is saved.
 *
 * The forms "save rename" and "save cancel" are actions for save alerts.
 */
static int
save (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
        register XimDataPtr xim = xc->xim;
	register fileSavePtr fsp = xim->fsp;
        int x0, y0, nx, ny;

        /* Handle the special cases first. */
        if (argc == 4 && strcmp (argv[1], "rename") == 0) {
	    xims_rename (xim, argv[2], argv[3]);
	    return (TCL_OK);
        } else if (argc == 3 && strcmp (argv[1], "cancel") == 0) {
	    xims_cancel (xim, argv[2]);
	    return (TCL_OK);
        }

	if (argc == 5) {
	    x0 = atoi (argv[1]);
	    y0 = atoi (argv[2]);
	    nx = atoi (argv[3]);
	    ny = atoi (argv[4]);
	} else
	    x0 = y0 = nx = ny = 0;

	/* Pass off to the file save routines. */
	if (xim_saveFile (xim, fsp->fname, fsp->format, x0,y0, nx,ny) < 0)
            return (TCL_ERROR);
	else
            return (TCL_OK);
}


/* Load -- Load a frame from a disk file.
 *
 * Usage:       load filename [frame]
 *
 * Options:	frame			display frame to be loaded
 */
static int
load (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register int i;
        register XimDataPtr xim = xc->xim;
	register fileLoadPtr flp = xim->flp;
        char *ip, *op, *fname;
        char *flist, buf[SZ_LINE];
	struct stat file_info;
	int frame;

        if (argc < 2)
            return (TCL_ERROR);

	fname = argv[1];
	frame = (argc >= 3) ? atoi(argv[2]) : xim->display_frame;

        /* If given a directory change the directory browser current reference
	 * directory and get a listing.
	 */
        if (fname[strlen(fname)-1] == '/') {
            fname[strlen(fname)-1] = '\0';
	    if (fname[0] == '/') {
                sprintf (flp->curdir, "%s", fname);
	    } else {
                strcat (flp->curdir, "/");
                strcat (flp->curdir, fname);
	    }
            sprintf (buf, "curdir %s", flp->curdir);
            xim_message (xim, "loadOptions", buf);

            xim_dirRescan (xim);

        } else {
            /* Otherwise it may be some kind of image to be loaded, but first
	     * check to see if it's not some other directory first.
	     */
	    (void) stat (fname, &file_info);
	    if (S_ISDIR(file_info.st_mode)) {
                sprintf (flp->curdir, "%s", fname);
                sprintf (buf, "curdir %s", flp->curdir);
                xim_message (xim, "loadOptions", buf);
                xim_dirRescan (xim);
		return (TCL_OK);
	    }

	    /* It's not a directory, so try loading the file. */
	    if (xim_loadFile (xim, fname, frame) != 0)
                return (TCL_ERROR);
        }

	return (TCL_OK);
}


/* Help -- Send the default help text (HTML) to the GUI.
 *
 * Usage:       help
 */

/* The builtin default help text. */
static char *help_text[] = {
    "setValue {",
#   include "ximtool.html.h"
    "}",
    NULL
};

static int
help (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
        register XimDataPtr xim = xc->xim;
        register char *ip, *op, *helptxt;
        register int i;

        helptxt = (char *) XtMalloc (1024000);
        for (i=0, op=helptxt;  ip = help_text[i];  i++) {
            while (*ip)
                *op++ = *ip++;
            *op++ = '\n';
        }
        *op++ = '\0';

        ObmDeliverMsg (xim->obm, "help", helptxt);
        XtFree ((char *)helptxt);

	return (TCL_OK);
}


/* Info -- Send various kinds of information to the GUI.  The 'args' option
 * allows us to pass in information from the GUI that cannot be easily
 * obtained otherwise, e.g. private information inthe Gterm widget such as
 * the basePixel resource.
 *
 * Usage:       info  option [ args ... ] 
 */

info (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
        register XimDataPtr xim = xc->xim;
        char line[SZ_LINE], path[80], *option, *message;

        if (argc < 2)
            return (TCL_ERROR);
        else
            option = argv[1];

        message = (char *) XtCalloc (8182, sizeof(char));

	if (strcmp (option, "server") == 0) {
	    info_server (xim, argc, argv, message);

	} else if (strcmp (option, "wcs") == 0) {
	    info_wcs (xim, message);

	} else if (strcmp (option, "clients") == 0) {
	    info_clients (xim, message);

	} else if (strcmp (option, "imtoolrc") == 0) {
	    info_imtoolrc (xim, message);

        } else {
            XtFree ((char *)message);
            return (TCL_ERROR);
	}

	strcat (message, "\n\0");

	if (*message)
            xim_message (xim, "info", message);
        XtFree ((char *)message);

	return (TCL_OK);
}


/* INFO_SERVER -- Helper routine to report server state information.
 */
info_server (xim, argc, argv, text)
register XimDataPtr xim;
int	argc;
char	**argv;
char	*text;
{
	extern char *ximtool_version[];
	extern int ncolormaps, first_color;
	char cmapname[80], line[SZ_LINE];
        ColorMapPtr cm;

	sprintf (text, "\t%s\n\n", ximtool_version[0]);

	sprintf (line, "%20s:  %s\n", "Base Pixel",
		(argc >= 3 ? argv[2] :""));
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "Max Colors",
		(argc >= 4 ? argv[3] :""));
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "Memory Model", xim->memModel);
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "Antialias Type", xim->antialiasType);
	strcat (text, line);
	strcat (text, "\n");

        cm = &colormaps[DEF_COLORMAP-1];
        strcpy (cmapname, cm->name);
	sprintf (line, "%20s:  %s\n", "Current Colormap", cmapname);
	strcat (text, line);
	sprintf (line, "%20s:  %d\n", "Colormaps Available", ncolormaps);
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "User Cmap 1", xim->userCMap1);
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "User Cmap 2", xim->userCMap1);
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "Cmap Dir 1", xim->userCMapDir1);
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "Cmap Dir 2", xim->userCMapDir2);
	strcat (text, line);
	strcat (text, "\n");
	sprintf (line, "%20s:  %s\n", "Printer Config", xim->printConfig);
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "Printer File", xim->pcp->printFile);
	strcat (text, line);
	sprintf (line, "%20s:  %s\n", "Printer Cmd", xim->pcp->printCmd);
	strcat (text, line);
}


/* INFO_CLIENTS -- Helper routine to report client (display or ISM) state.
 */
info_clients (xim, text)
register XimDataPtr xim;
char	*text;
{
        register IsmModule ism;
	register int i;
        char line[SZ_LINE], path[80];
	extern ismModule ism_modules[];
	extern int ism_nmodules;

	strcpy (text, "\t\tClient Communications Channels\n\n");

	strcat (text, "  Display Client Connections\t    ");
	strcat (text, "ISM Client Connections\n");
	strcat (text, "  --------------------------\t    ");
	strcat (text, "----------------------\n");

	if (xim->port)
	    sprintf (line, "  inet:  %d\t\t\t    ", xim->port);
	else
	    sprintf (line, "  inet:  Disabled\t\t");
	strcat (text, line);

	sprintf (path, xim->ism_addr, getuid());
	sprintf (line, "unix:  %s\n", path);
	strcat (text, line);

	if (strcmp(xim->unixaddr, "none") != 0) {
	    sprintf (path, xim->unixaddr, getuid());
	    sprintf (line, "  unix:  %s\n", path);
	} else
	    sprintf (line, "  unix:  Disabled\n");
	strcat (text, line);

	if (strcmp(xim->input_fifo,"") != 0 && 
	    strcmp(xim->input_fifo,"none") != 0)
	        sprintf (line, "  fifo:  %s\n\t %s\n\n",
		    xim->input_fifo, xim->output_fifo);
	else
	    sprintf (line, "  fifo:  Disabled\n\n");
	strcat (text, line);

	strcat (text, "\n");
	strcat (text, "\t\t   Available ISM Components\n\n");
	strcat (text, "  Name      Channel         Command\n");
	strcat (text, "  ----      -------         -------\n");
        for (i=0; i < ism_nmodules; i++) {
            ism = &ism_modules[i];
	    sprintf (line, "  %-9.9s %-15.15s '%s'\n", 
		    ism->name,
		    (ism->connected ? ism->chan->path : "Disabled"),
		    ism->command);
	    strcat (text, line);
        }
}


/* INFO_WCS -- Helper routine to report WCS and mapping information for 
 * each frame in the display.
 */
info_wcs (xim, text)
register XimDataPtr xim;
char	*text;
{
        register FrameBufPtr fr = xim->df_p;
        register CtranPtr ct = &fr->ctran;
	register int i;
        MappingPtr mp = (MappingPtr) NULL;
	char	 line[SZ_LINE];

	/* Write the current frame WCS. */
	sprintf (line,
	    "\t\tFrame %d  WCS & Mappings\n\t\t------------------------\n\n",
		fr->frameno);
	strcat (text, line);

	strcat (text, "Frame WCS:\n");
	sprintf (line, "    a = %9.3f\t b = %9.3f %s\n",
	    ct->a, ct->b, "# Scale factors");
	strcat (text, line);

	sprintf (line, "    c = %9.3f\t d = %9.3f %s\n",
	    ct->c, ct->d, "# Cross factors");
	strcat (text, line);

	sprintf (line, "   tx = %9.3f\tty = %9.3f %s\n",
	    ct->tx, ct->ty, "# Translation");
	strcat (text, line);
	sprintf (line, "   z1 = %9.3f\tz2 = %9.3f %s\n",
	    ct->z1, ct->z2, "# z-scale range");
	strcat (text, line);

	sprintf (line, "   zt = %9s\t%30s\n",
	    (ct->zt == W_UNITARY ? "unitary" : 
	    (ct->zt == W_LINEAR  ? "linear" : 
	    (ct->zt == W_LOG     ? "log" : "unknown"))),
	    "# z-scale type\n");
	strcat (text, line);
		

	fr = (FrameBufPtr) NULL; 
	for (i=0; i < xim->nframes; i++) {
	    fr = &xim->frames[i];
	    if (xim->display_frame == fr->frameno)
		break;
	}

	if (!fr) {
	    strcat (text, " \n \n");
	    return;
	}

	for (i=0; i < fr->nmaps; i++) {
	    mp = &(fr->mapping[i]);
            ct = &(mp->ctran);

	    sprintf (line, "\nMapping %d: \n", mp->id);
	    strcat (text, line);

	    sprintf (line, "    a = %7.3f    b = %7.3f\n", ct->a, ct->b);
	    strcat (text, line);
	    sprintf (line, "    c = %7.3f    d = %7.3f\n", ct->c, ct->d);
	    strcat (text, line);
	    sprintf (line, "   tx = %7.3f   ty = %7.3f\n", ct->tx, ct->ty);
	    strcat (text, line);
	    sprintf (line, "   z1 = %7.3f   z2 = %7.3f\tzt: %s\n",
		ct->z1, ct->z2,
	        (ct->zt == W_UNITARY ? "unitary" : 
	        (ct->zt == W_LINEAR  ? "linear" : 
	        (ct->zt == W_LOG     ? "log" : "unknown"))) );
	    strcat (text, line);

	    sprintf (line, "   region %d: %s\n", mp->regid, mp->region);
	    strcat (text, line);
	    sprintf (line, "      src: x=%9f  y=%9f  nx=%d ny=%d\n",
		mp->sx, mp->sy, mp->snx, mp->sny);
	    strcat (text, line);
	    sprintf (line, "     dest: x=%9d  y=%9d  nx=%d ny=%d\n",
		mp->dx, mp->dy, mp->dnx, mp->dny);

	    strcat (text, line);
	    sprintf (line, "      ref: %s\n", mp->ref);
	    strcat (text, line);
	}
	strcat (text, " \n \n");
}


/* INFO_IMTOOLRC -- Helper routine to report the frame buffer configuration
 * table.
 */
info_imtoolrc (xim, text)
register XimDataPtr xim;
char	*text;
{
	register int last_fb_used = MAX_FBCONFIG;
	register int i, w, h, nf, fb_config = xim->fb_configno;
	char	 line[SZ_LINE];

	strcpy (text, "    Frame Buffer Configuration Table\n");
	strcat (text, "    --------------------------------\n\n");

	sprintf (line, "  Imtoolrc File:  %s\n", xim->imtoolrc);
	strcat (text, line);
	strcat (text, "\n  Config      NFrames\tWidth\tHeight\n");
	strcat (text, "  ------      -------\t-----\t------\n");

	/* Find the index of the last FB defined. */
	for (i=MAX_FBCONFIG; i > 1; i--)
	    if (xim->fb_config[i-1].width != DEF_FRAME_WIDTH || 
	        xim->fb_config[i-1].height != DEF_FRAME_HEIGHT) {
	            last_fb_used = i;
	            break;
	    }

	/* Print out the frame buffer configurations. */
	for (i=1; i <= last_fb_used; i++) {
	    w = xim->fb_config[i-1].width;
	    h = xim->fb_config[i-1].height;
	    nf = xim->fb_config[i-1].nframes;
	    if (i > 1 && (w == DEF_FRAME_WIDTH && h == DEF_FRAME_HEIGHT)) {
	        sprintf (line, "  %4d\t\t 0\t  n/a\t  n/a\n", i);
	    } else {
	        sprintf (line, "  %4d\t\t%2d\t%5d\t%5d\t  %s\n",
	            i, nf, w, h, ((i==fb_config) ? "<--- current" : " "));
	    }
	    strcat (text, line);
	}
	strcat (text, " \n \n");
}


/* windowRGB -- Window an individual component of an RGB colormap.  We start
 * with the currently defined cmap and scale it's component by the given
 * offset and slope.  A 'save' flag is set when the button is released meaning
 * the user is done with that component and the loaded colormap is updated,
 * otherwise continue changing that color from the previous call allowing us
 * to window the color progressively.  The GUI's 'initialize' option should
 * restore the original colormap.  [This is still test code.]
 *
 * Usage:	windowRGB <color> <offset> <scale> <save>
 *
 * Options:	color		color to manipulate (1=Red,2=Green,3=Blue)
 *		offset		offset of transformation
 *		scale		slope of transformation
 *		save		save to loaded colormap when complete?
 */

static int 
windowRGB (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register FrameBufPtr fb = xim->df_p;
	register ColorMapPtr cm;
	int color, first, nelem, maxelem, save = 0;
	unsigned short r[MAX_COLORS];
	unsigned short g[MAX_COLORS];
	unsigned short b[MAX_COLORS];
	char buf[SZ_LINE];


	if (argc > 1) {
	    cm = &colormaps[fb->colormap-1];
	    color = atoi(argv[1]);
	    fb->offset = atof(argv[2]);
	    fb->scale = (argc > 2) ? (float)atof(argv[3]) : fb->scale;
	    save = atoi(argv[4]);

	    /* Query and read the current colormap. */
	    GtQueryColormap (xim->gt, cm->mapno, &first, &nelem, &maxelem);
	    GtReadColormap (xim->gt, cm->mapno, first, nelem, r,g,b);

	    /* compute the scaled colormap, scaling only the color we're
	     * interested in.
	     */
	    switch (color) {
	    case 1:
		cmapScale (r, nelem, first, fb->offset, fb->scale);
		break;
	    case 2:
		cmapScale (g, nelem, first, fb->offset, fb->scale);
		break;
	    case 3:
		cmapScale (b, nelem, first, fb->offset, fb->scale);
		break;
	    }

	    /* Lastly, write it back to the widget. */
	    GtWriteColormap (xim->gt, 0, first, nelem, r, g, b);
	    if (save)
	        GtWriteColormap (xim->gt, cm->mapno, first, nelem, r, g, b);
	}

	return (TCL_OK);
}


/* cmapScale -- Given a single-color cmap scale it with the given offset and
 * slope, the scaling is done in place.
 */

cmapScale (map, ncells, first, offset, slope)
unsigned short map[MAX_COLORS];
int ncells, first;
float offset, slope;
{
	register int i, c1, c2;
	register float x, y, z, frac;
	unsigned short val, out[MAX_COLORS];

        for (i=0;  i < ncells;  i++) {
            x = (float)i / (float)(ncells - 1);
            y = (x - offset) * slope + 0.5;

            if (y <= 0.0) {
                val = map[first];
            } else if (y >= 1.0) {
                val = map[ncells-1];
            } else {
                z = y * (ncells - 1);
                c1 = (int)z;
                c2 = min (ncells-1, c1 + 1);
                frac = z - c1;
                val = map[c1] * (1.0 - frac) + map[c2] * frac;
            }

            out[i] = val;
        }  

	for (i=0; i < MAX_COLORS; i++)
	    map[i] = out[i];
}



/* ISM_START -- Start the ISM task.  The named task must be listed in
 * the array of ISM modules.
 *
 * Usage:       ism_start  task
 */
static int
ism_start (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register IsmModule ism;

        if (argc < 2)
            return (TCL_ERROR);

	/* Lookup the command for the task and start it.
	 */
	if ((ism = ismNameToPtr (argv[1]))) {
            system (ism->command);
            return (TCL_OK);
	}

	/* Task not found, return an error. */
        return (TCL_ERROR);
}


/* ISM_STOP -- Stop the ISM task.  The named task is told to shut itself
 * down by executing the registered shutdown callback.  We return OK if
 * the shutdown can be executed, an ERR is returned if the named task is
 * not currently running.
 *
 * Usage:       ism_stop  task
 */
static int
ism_stop (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register IsmModule ism;

        if (argc < 2)
            return (TCL_ERROR);

	/* Lookup the command for the task and stop it.  */
	if ((ism = ismNameToPtr (argv[1]))) {
            (*ism->shutdownCB) (xim, ism);
            ism->connected = 0;
            return (TCL_OK);
 	}

	/* Task not found, return an error. */
        return (TCL_ERROR);
}


/* ISM_CMD -- Send a command to the named ISM command callback.  Return
 * values are sent as messages to the GUI 'ism_msg' parameter object, we
 * simply pass the argv to the appropriate function.
 *
 * Usage:       ism_cmd  task <args>
 */
static int
ism_cmd (xc, tcl, argc, argv)
register XimClientPtr xc;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register XimDataPtr xim = xc->xim;
	register IsmModule ism;
	char 	 **cmd_argv;
	int  	 cmd_argc;

        if (argc < 2)
            return (TCL_ERROR);

	/* Lookup the command callback for the task and run it.  */
	if ((ism = ismNameToPtr (argv[1]))) {

	    /* Get local copy of argc and argv containing only the commands
	     * for the ISM callback.
	     */
            if ((cmd_argc = (argc - 2) > 0)) {
                cmd_argv = (char **) XtMalloc (cmd_argc * sizeof(char *));
                memmove (cmd_argv, &argv[2], cmd_argc * sizeof(char *)); 
            }

            /* Process the command. */
	    (*ism->commandCB) (xim, ism, cmd_argc, cmd_argv);
            return (TCL_OK);
	}

	/* Task not found, return an error. */
        return (TCL_ERROR);
}
