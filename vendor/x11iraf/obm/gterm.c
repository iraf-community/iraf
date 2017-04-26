/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */

#include <ObmP.h>
#include "widget.h"

/*
 * Gterm-Image widget class (a subclass of Widget).
 * -------------------------------------------------
 * The gterm-image widget is a general 2D graphics-imaging widget providing
 * a wide range of facilities for drawing graphics and text, for image
 * display, and for graphics interaction.  Normally the client communicates
 * directly with the Gterm widget to draw graphics, download image data,
 * and so on, using some communications protocol outside the domain of the
 * object manager.  Nonetheless so far as possible the facilities of the Gterm
 * widget have also been made available to GUI code via the commands listed
 * here.
 *
 * The gterm class uses a special OBM callback known as OBMCB_setGterm.
 * A client application can use this to post a procedure to be called when
 * a gterm widget receives the setGterm command.  The calling sequence for
 * the setGterm callback is as follows:
 *
 *                    setgterm (client_data, gterm_widget)
 *
 * The purpose of this callback is to tell the client which gterm widget is
 * the "active" gterm widget.  This is used by clients which only support
 * one active Gterm widget, i.e., which can only direct graphics output to
 * one Gterm widget at a time.
 *
 * The messages or commands that can be sent to the Gterm widget by GUI
 * code follow.
 *
 * General commands:
 *
 *		   setGterm [arg [arg ...]]	# make widget the active Gterm
 *
 *	           activate
 *	         deactivate
 *	        addCallback procedure-name callback-type
 *	     deleteCallback procedure-name
 *	              reset
 *	              flush
 *
 *	       setCursorPos x y [raster]
 * 	       getCursorPos x y
 *            setCursorType cursortype
 *	               bell
 *
 * Graphics drawing commands:
 *
 *	          setRaster raster
 *       raster = getRaster [raster]
 *
 *	          setLogRes width height
 *	          getLogRes width height
 *	         setPhysRes width height
 *	         getPhysRes width height
 *	         setTextRes rows cols
 *	       setDataLevel level
 *	       setLineWidth width
 *	       setLineStyle style
 *	      setColorIndex index
 *	        setFillType filltype
 *
 *	        clearScreen
 *	       drawPolyline vector
 *	     drawPolymarker vector
 *	        drawPolygon vector
 *	         drawMarker type x y xsize ysize [rotangle]
 *
 *	      drawAlphaText x y text
 * width = getAlphaTextSize [string [width [height [base]]]]
 *              startDialog
 *	          endDialog
 *	        eraseDialog
 *	     drawDialogText x y text
 *width = getDialogTextSize [string [width [height [base]]]]
 * 
 * The coordinates used in the graphics drawing commands are logical
 * coordinates as defined by setLogRes, in the coordinate system of the
 * reference drawing raster as defined by setRaster.  The default reference
 * raster is raster zero, the widget's window.  Vectors are specified as
 * a list of points, e.g., { {x y} {x y} ... }.
 *
 * Imaging commands:
 *
 *               rasterInit
 *             assignRaster raster drawable
 *             createRaster raster width height [type [depth]]
 *            destroyRaster raster
 *     exists = queryRaster raster [width [height [type [depth]]]]
 *      raster = nextRaster
 *        n = activeRasters
 *
 *		   setPixel raster x y value
 *	   value = getPixel raster x y
 *              writePixels raster pixels encoding x1 y1 nx ny [bias]
 *      pixels = readPixels raster encoding x1 y1 nx ny [bias]
 *            refreshPixels raster ct x1 y1 nx ny
 *		  setPixels raster color [ct x1 y1 nx ny [rop]]
 *            extractPixmap pixmap src [x y [width height]]
 *             insertPixmap pixmap dst [x y [width height]]
 *
 *  colormap = nextColormap
 *             freeColormap colormap
 *            writeColormap colormap colors [offset]
 *   ncolors = readColormap colormap colors [offset [ncolors]]
 *             loadColormap colormap offset scale
 *      pixel = clientPixel gterm_pixel
 *           bias = getBias [nelem [maxelem]]
 *
 *             initMappings
 *    mapping = nextMapping
 *              freeMapping mapping
 *	       raiseMapping mapping [reference]
 *	       lowerMapping mapping [reference]
 *            enableMapping mapping [refresh]
 *           disableMapping mapping [erase]
 *   active = activeMapping mapping
 *           refreshMapping mapping
 *
 *               copyRaster rop src  st sx sy snx sny  dst dt dx dy dnx dny
 *               setMapping mapping rop
 *                          src st sx sy snx sny  dst dt dx dy dnx dny
 *               getMapping mapping rop 
 *                          src st sx sy snx sny  dst dt dx dy dnx dny
 *
 *    raster = selectRaster dras dt dx dy rt rx ry [map]
 *	         unmapPixel sx sy raster rx ry [rz]
 *
 *		       flip mapping axis [axis...]
 *
 * Pixel arrays are long strings consisting either of a sequence of numeric
 * pixel values separated by whitespace (space or newline), or a hex encoded
 * sequence of bytes (1 or 2 hex digits per 8 bit pixel).  Hex encoded pixel
 * arrays may optionally be compressed using a simple run length encoding
 # scheme.  Colors are specified as a list of RGB triplets, e.g., { {R G B}
 # {R G B} ... }.
 *
 * Refer to the documentation for the Gterm widget for a detailed description
 * of rasters, mappings, and colormaps.
 *
 * Markers:
 *
 * 	       createMarker name [attribute-list]
 *               markerInit
 *
 * New markers may be created with createMarker.  Once created, a marker
 * functions under the Object Manager as a named object of class "marker".
 * Refer to the marker class for a description of the commands defined for
 * a marker.
 */

#define	MAX_COLORS	256
#define	MAX_POLYPTS	4096
#define	FIRST_COLOR	10
#define	CB_Input	1
#define	CB_Resize	2
#define	CB_Reset	3

/* Gterm class instance descriptor. */
struct gtermPrivate {
	ObmCallback callback_list;
	int colormap;
	float offset, scale;
};

typedef struct gtermPrivate *GtermPrivate;

struct gtermObject {
	struct obmObjectCore core;
	struct widgetPrivate widget;
	struct gtermPrivate gterm;
};

typedef struct gtermObject *GtermObject;

/* Gterm class class record private data. */
typedef struct {
	/* standard MsgContext fields. */
	Tcl_Interp *tcl;		/* class interpreter */
	ObmObject object[MAX_LEVELS];	/* object which received last message */
	int level;

	/* Gterm specific fields. */
	/* (none) */
} gtermClassData, *GtermClassData;


void GtermDestroy();
void GtermClassDestroy();
ObmObject GtermCreate();

static	int gtermActivate(), gtermActiveMapping(), gtermActiveRasters();
static	int gtermAddCallback(), gtermDeleteCallback();
static	int gtermAssignRaster(), gtermBell(), gtermGetBias();
static	int gtermClearScreen(), gtermClientPixel(), gtermCopyRaster();
static	int gtermCreateMarker(), gtermCreateRaster(), gtermDeactivate();
static	int gtermDestroyRaster(), gtermDisableMapping(), gtermDrawAlphaText();
static	int gtermDrawDialogText(), gtermDrawMarker(), gtermDrawPolygon();
static	int gtermDrawPolyline(), gtermDrawPolymarker(), gtermEnableMapping();
static	int gtermEndDialog(), gtermEraseDialog(), gtermExtractPixmap();
static	int gtermFlip(), gtermFlush(), gtermFreeColormap();
static	int gtermFreeMapping(), gtermRaiseMapping(), gtermLowerMapping();
static	int gtermGetAlphaTextSize(), gtermGetCursorPos();
static	int gtermGetDialogTextSize(), gtermGetLogRes(), gtermGetMapping();
static	int gtermGetPhysRes(), gtermGetPixel(), gtermGetRaster();
static	int gtermInitMappings(), gtermInsertPixmap(), gtermLoadColormap();
static	int gtermMarkerInit(), gtermNextColormap(), gtermNextMapping();
static	int gtermNextRaster(), gtermQueryRaster(), gtermRasterInit();
static	int gtermReadColormap(), gtermReadPixels(), gtermRefreshMapping();
static	int gtermRefreshPixels(), gtermReset(), gtermSelectRaster();
static	int gtermSetColorIndex(), gtermSetCursorPos(), gtermSetCursorType();
static	int gtermSetDataLevel(), gtermSetFillType(), gtermSetGterm();
static	int gtermSetLineStyle(), gtermSetLineWidth(), gtermSetLogRes();
static	int gtermSetMapping(), gtermSetPhysRes(), gtermSetPixel();
static	int gtermSetPixels(), gtermSetRaster(), gtermSetTextRes();
static	int gtermStartDialog(), gtermUnmapPixel(), gtermWriteColormap();
static	int gtermWritePixels();

static	void gtermInputCallback();
static	void gtermResizeCallback(), gtermResetCallback();
static	void get_mapping(), put_mapping();
static	XPoint *get_points();
extern	double strtod(), atof();


/* GtermClassInit -- Initialize the class record for the gterm widget class.
 */
void
GtermClassInit (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	register GtermClassData gcd;
	register Tcl_Interp *tcl;
	register ClientData c_gcd;

	/* Install the class methods. */
	classrec->ClassDestroy = GtermClassDestroy;
	classrec->Create = (ObmFunc) GtermCreate;
	classrec->Destroy = GtermDestroy;
	classrec->Evaluate = WidgetEvaluate;

	/* The gterm widget subclass has its own command set hence has its
	 * own interpreter.  The widget will respond both to all the commands
	 * defined here, and to all the commands implemented by the base
	 * Widget class.
	 */
	if (!classrec->class_data) {
	    gcd = (GtermClassData) XtCalloc (1, sizeof (gtermClassData));
	    gcd->tcl = tcl = Tcl_CreateInterp();
	    classrec->class_data = (XtPointer) gcd;
	    c_gcd = (ClientData)gcd;
	    gcd->level = 0;

	    Tcl_CreateCommand (tcl,
		"activate", gtermActivate, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"activeMapping", gtermActiveMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"activeRasters", gtermActiveRasters, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"addCallback", gtermAddCallback, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"deleteCallback", gtermDeleteCallback, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"assignRaster", gtermAssignRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"bell", gtermBell, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"clearScreen", gtermClearScreen, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"clientPixel", gtermClientPixel, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"copyRaster", gtermCopyRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"createMarker", gtermCreateMarker, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"createRaster", gtermCreateRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"deactivate", gtermDeactivate, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"destroyRaster", gtermDestroyRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"disableMapping", gtermDisableMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"drawAlphaText", gtermDrawAlphaText, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"drawDialogText", gtermDrawDialogText, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"drawMarker", gtermDrawMarker, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"drawPolygon", gtermDrawPolygon, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"drawPolyline", gtermDrawPolyline, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"drawPolymarker", gtermDrawPolymarker, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"enableMapping", gtermEnableMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"endDialog", gtermEndDialog, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"eraseDialog", gtermEraseDialog, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"extractPixmap", gtermExtractPixmap, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"flip", gtermFlip, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"flush", gtermFlush, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"freeColormap", gtermFreeColormap, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"freeMapping", gtermFreeMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"raiseMapping", gtermRaiseMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"lowerMapping", gtermLowerMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getAlphaTextSize", gtermGetAlphaTextSize, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getBias", gtermGetBias, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getCursorPos", gtermGetCursorPos, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getDialogTextSize", gtermGetDialogTextSize, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getLogRes", gtermGetLogRes, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getMapping", gtermGetMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getPhysRes", gtermGetPhysRes, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getPixel", gtermGetPixel, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"getRaster", gtermGetRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"initMappings", gtermInitMappings, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"insertPixmap", gtermInsertPixmap, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"loadColormap", gtermLoadColormap, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"markerInit", gtermMarkerInit, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"nextColormap", gtermNextColormap, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"nextMapping", gtermNextMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"nextRaster", gtermNextRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"queryRaster", gtermQueryRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"rasterInit", gtermRasterInit, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"readColormap", gtermReadColormap, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"readPixels", gtermReadPixels, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"refreshMapping", gtermRefreshMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"refreshPixels", gtermRefreshPixels, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"reset", gtermReset, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"selectRaster", gtermSelectRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setColorIndex", gtermSetColorIndex, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setCursorPos", gtermSetCursorPos, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setCursorType", gtermSetCursorType, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setDataLevel", gtermSetDataLevel, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setFillType", gtermSetFillType, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setGterm", gtermSetGterm, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setLineStyle", gtermSetLineStyle, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setLineWidth", gtermSetLineWidth, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setLogRes", gtermSetLogRes, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setMapping", gtermSetMapping, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setPhysRes", gtermSetPhysRes, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setPixel", gtermSetPixel, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setPixels", gtermSetPixels, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setRaster", gtermSetRaster, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"setTextRes", gtermSetTextRes, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"startDialog", gtermStartDialog, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"unmapPixel", gtermUnmapPixel, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"writeColormap", gtermWriteColormap, c_gcd, NULL);
	    Tcl_CreateCommand (tcl,
		"writePixels", gtermWritePixels, c_gcd, NULL);
	}
}


/* GtermClassDestroy -- Custom destroy procedure for the widget class.
 */
void
GtermClassDestroy (obm, classrec)
ObmContext obm;
register ObjClassRec classrec;
{
	register GtermClassData gcd = (GtermClassData) classrec->class_data;

	if (gcd) {
	    if (gcd->tcl)
		Tcl_DeleteInterp (gcd->tcl);
	    XtFree ((char *)gcd);
	    classrec->class_data = NULL;
	}
}


/* GtermCreate -- Create an instance of a gterm object.
 */
ObmObject
GtermCreate (obm, name, classrec, parent, args, nargs)
ObmContext obm;
char *name;
ObjClassRec classrec;
char *parent;
ArgList args;
int nargs;
{
	register GtermObject obj;
	Widget w, pw;

	obj = (GtermObject) WidgetCreate (obm, name,
	    classrec, parent, args, nargs);
	if (obj == NULL)
	    return (NULL);

	obj = (GtermObject) XtRealloc ((char *)obj, sizeof(struct gtermObject));
	if (obj == NULL)
	    return (NULL);

	/* Initialize GtermPrivate instance structure. */
	obj->gterm.callback_list = NULL;
	obj->gterm.colormap = 0;
	obj->gterm.offset = 0.5;
	obj->gterm.scale = 1.0;

	return ((ObmObject) obj);
}


/* GtermDestroy -- Destroy an instance of a gterm object.
 */
void
GtermDestroy (object)
ObmObject object;
{
	GtermObject obj = (GtermObject) object;
	ObjClassRec classrec = obj->core.classrec;
	register GtermClassData gcd = (GtermClassData) classrec->class_data;
	register ObmCallback cb, cb_next;
	ObmContext obm = obj->widget.obm;
	Widget w = obj->widget.w;

	/* Destroy the object in the second final call to Destroy. */
	if (!obj->core.being_destroyed++)
	    return;

	/* Invoke any posted setGterm callbacks.  This is not completely
	 * correct; in principle we should call the setGterm callback only
	 * if the active gterm widget is destroyed.
	 */
	for (cb = obm->callback_list;  cb;  cb = cb->next)
	    if ((cb->callback_type & OBMCB_setGterm) && cb->u.fcn)
		(*cb->u.fcn) (cb->client_data, NULL);

	/* Free any gterm callback descriptors. */
	for (cb = obj->gterm.callback_list;  cb;  cb = cb_next) {
	    cb_next = cb->next;

	    /* Delete the widget level callback. */
	    switch (cb->callback_type) {
	    case CB_Input:
		GtDeleteInputProc (w, gtermInputCallback, (XtPointer)cb);
		break;
	    case CB_Resize:
		GtDeleteResizeProc (w, gtermResizeCallback, (XtPointer)cb);
		break;
	    case CB_Reset:
		GtDeleteResetProc (w, gtermResetCallback, (XtPointer)cb);
		break;
	    }

	    XtFree ((char *)cb);
	}

	WidgetDestroy (object);
}


/*
 * GTERM class commands.
 * -----------------------
 */


/* SetGterm -- Set the active Gterm widget.  Call any OBMCB_setGterm callbacks
 * registered by the client code, passing the client the Xt Widget handle of
 * the active gterm widget.
 *
 *  Usage:	setGterm [arg [arg ...]]
 *
 * This feature may be used during GUI execution to identify the currently
 * active gterm widget to the client, or during startup to pass the Widget id
 * to the client code so that it can talk directly to the gterm widget.
 */
static int 
gtermSetGterm (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	register GtermClassData gcd = (GtermClassData) msg;
	GtermObject obj = (GtermObject) gcd->object[gcd->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmCallback cb;
	ObmContext obm = wp->obm;
	char *message;

	message = (argc > 1) ? Tcl_Concat (argc-1, &argv[1]) : NULL;

	for (cb = obm->callback_list;  cb;  cb = cb->next)
	    if ((cb->callback_type & OBMCB_setGterm) && cb->u.fcn)
		(*cb->u.fcn) (cb->client_data, wp->w, message);

	if (message)
	    free ((char *)message);

	return (TCL_OK);
}


/* Activate -- Activate the gterm widget.   This causes the next GIN mode
 * setCursorType to warp the pointer into the gterm window.
 *
 * Usage:	activate
 */
static int 
gtermActivate (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtActivate (w);
	return (TCL_OK);
}


/* Deactivate -- Deactivate the gterm widget.   If the cursor has been warped
 * into the window by a previous activate/setCursorType GIN mode, this causes
 * the cursor to be warped back to where it was previously.
 *
 * Usage:	deactivate
 */
static int 
gtermDeactivate (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtDeactivate (w);
	return (TCL_OK);
}


/* Reset -- Reset the gterm widget.  This causes a number of state variables
 * affecting graphics drawing options to be set to their default values.
 *
 * Usage:	reset
 */
static int 
gtermReset (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtReset (w);
	return (TCL_OK);
}


/* Flush -- Flush any graphics output and synchronize the state of the widget
 * with what is shown on the display.
 *
 * Usage:	flush
 *
 * The gterm widget uses XLIB, which buffers graphics drawing commands and
 * automatically sends them to the X server when 1) the buffer fills,
 * 2) input is requested from the server.  Such buffering of data is necessary
 * for efficient operation and it should rarely be necessary to explicitly
 * flush graphics output since XLIB does this automatically in most cases.
 * An example of when explicitly flushing the ouptut might be necessary is in
 * cases where smooth animation is desired and drawing the graphics in batches
 * could cause the display to appear "jerky".
 */
static int 
gtermFlush (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtFlush (w);
	return (TCL_OK);
}


/* AddCallback -- Post a callback for a Gterm widget event.
 *
 * Usage:	addCallback procedure-name [callback-type]
 *
 * The recognized Gterm callbacks are
 *
 *	input		Called when the graphics-input action is invoked in
 *			a translation table.  The default Gterm translation
 *			table invokes this action when a KeyPress event occurs
 *			in the Gterm window.
 *
 *			    Callback:	widget-name input-type event-data
 *
 *	resize		Called when the gterm window is resized.
 *
 *			    Callback:	widget-name width height
 *
 *	reset		Called when the "reset" action is invoked.
 *
 *			    Callback:	widget-name
 *
 * If no callback is specified the default is "input".
 *
 * Note that in GUI code one can also use the translation table to directly
 * invoke GUI procedures without need to use the Gterm input mechanism.  This
 * is more flexible but we support the Gterm input callback here for
 * applications that use the default translations.
 */
static int 
gtermAddCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register GtermPrivate gp = &obj->gterm;
	register Widget w = wp->w;
	char *userproc, *callback_type;
	ObmCallback cb, new;
	int type;

	if (argc < 2)
	    return (TCL_ERROR);

	userproc = argv[1];
	callback_type = (argc > 2) ? argv[2] : "input";

	if (strcmp (callback_type, "input") == 0)
	    type = CB_Input;
	else if (strcmp (callback_type, "resize") == 0)
	    type = CB_Resize;
	else if (strcmp (callback_type, "reset") == 0)
	    type = CB_Reset;
	else
	    return (TCL_ERROR);

	/* Initialize callback descriptor. */
	new = (ObmCallback) XtCalloc (1, sizeof (obmCallback));
	new->u.obj = (ObmObject) obj;
	new->callback_type = type;
	strncpy (new->name, userproc, SZ_NAME);

	/* Append descriptor to callback list for widget. */
	for (cb = gp->callback_list;  cb && cb->next;  cb = cb->next)
	    ;
	if (cb)
	    cb->next = new;
	else
	    gp->callback_list = new;

	/* Register the callback with the widget. */
	switch (type) {
	case CB_Input:
	    GtPostInputProc (w, gtermInputCallback, (XtPointer)new);
	    break;
	case CB_Resize:
	    GtPostResizeProc (w, gtermResizeCallback, (XtPointer)new);
	    break;
	case CB_Reset:
	    GtPostResetProc (w, gtermResetCallback, (XtPointer)new);
	    break;
	}

	return (TCL_OK);
}


/* DeleteCallback -- Delete a gterm callback.
 *
 *  Usage:	deleteCallback procedure
 */
static int 
gtermDeleteCallback (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register GtermPrivate gp = &obj->gterm;
	register ObmCallback cb, prev;
	Widget w = obj->widget.w;
	char *procedure;

	if (argc < 2)
	    return (TCL_ERROR);
	procedure = argv[1];

	/* Locate and delete procedure entry in callback list. */
	for (prev=NULL, cb=gp->callback_list;  cb;  prev=cb, cb=cb->next)
	    if (strcmp (cb->name, procedure) == 0) {
		/* Delete the widget level callback. */
		switch (cb->callback_type) {
		case CB_Input:
		    GtDeleteInputProc (w, gtermInputCallback, (XtPointer)cb);
		    break;
		case CB_Resize:
		    GtDeleteResizeProc (w, gtermResizeCallback, (XtPointer)cb);
		    break;
		case CB_Reset:
		    GtDeleteResetProc (w, gtermResetCallback, (XtPointer)cb);
		    break;
		}
		if (prev)
		    prev->next = cb->next;
		else
		    gp->callback_list = cb->next;
		XtFree ((char *)cb);
		break;
	    }

	return (TCL_OK);
}


/* gtermInputCallback -- Low level callback procedure, called by the Gterm
 * widget when an input event occurs.
 *
 * Callback:	userproc widget-name input-type x y data
 * Example:	userproc widget-name keyPress x y {a shift}
 *
 * where input-type is keyPress, keyRelease, buttonPress, or buttonRelease,
 * and data depends upon the type of input event.  data is a list of strings
 * delimited by braces.  The first string is the key or button pressed and
 * the following strings give the state of any modifier keys ("shift", 
 * "control" and so on).
 */
static void
gtermInputCallback (cb, w, event)
ObmCallback cb;
Widget w;
XEvent *event;
{
	GtermObject obj = (GtermObject) cb->u.obj;
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	char s_x[SZ_NUMBER], s_y[SZ_NUMBER];
	char message_data[SZ_LINE];
	char *event_type;
	int status;

	switch (event->type) {
	case KeyPress:
	case KeyRelease:
	    {   XKeyPressedEvent *ev = (XKeyPressedEvent *) event;
		register char *ip, *op = message_data;
		char buf[SZ_MESSAGE];
		int n;

		if (event->type == KeyPress)
		    event_type = "keyPress";
		else
		    event_type = "keyRelease";

		sprintf (s_x, "%d", ev->x);
		sprintf (s_y, "%d", ev->y);

		if ((n = XLookupString(ev,buf,sizeof(buf),NULL,NULL)) > 0) {
		    for (ip=buf;  --n >= 0;  )
			if (*ip <= ' ') {
			    *op++ = '^';
			    *op++ = *ip++ + 'A' - 1;
			} else if (isprint (*ip)) {
			    *op++ = *ip++;
			} else
			    ip++;
		} else {
		    /* This case occurs when only a modifier is typed. */
		    for (ip = "??";  *op++ = *ip++; )
			;
		}
		*op++ = ' ';
		op = widgetEventState (op, ev->state);
		while (op > message_data && isspace (*(op-1)))
		    --op;
		*op = '\0';
	    }
	    break;

	case ButtonPress:
	case ButtonRelease:
	    {   XButtonPressedEvent *ev = (XButtonPressedEvent *) event;
		register char *op = message_data;

		if (event->type == KeyPress)
		    event_type = "buttonPress";
		else
		    event_type = "buttonRelease";

		sprintf (s_x, "%d", ev->x);
		sprintf (s_y, "%d", ev->y);

		sprintf (op, "%d ", ev->button);
		while (*op)
		    op++;
		*op++ = ' ';
		op = widgetEventState (op, ev->state);
		while (op > message_data && isspace (*(op-1)))
		    --op;
		*op = '\0';
	    }
	    break;

	default:
	    strcpy (message_data, "unknown none");
	}

	status = Tcl_VarEval (obm->tcl,
	    cb->name, " ",
	    obj->core.name, " ",
	    event_type, " ",
	    s_x, " ",
	    s_y, " ",
	    "{", message_data, "}",
	    NULL);

	if (status != TCL_OK) {
	    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
	    fprintf (stderr, "Error on line %d in %s: %s\n",
		obm->tcl->errorLine, cb->name,
		errstr ? errstr : obm->tcl->result);
	}
}


/* gtermResizeCallback -- Low level callback procedure, called by the Gterm
 * widget when a resize event occurs.
 *
 * Callback: userproc widget-name width height
 */
static void
gtermResizeCallback (cb, w)
ObmCallback cb;
Widget w;
{
	GtermObject obj = (GtermObject) cb->u.obj;
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	int rtype, width, height, depth, status;
	char s_width[SZ_NUMBER], s_height[SZ_NUMBER];

	GtQueryRaster (wp->w, 0, &rtype, &width, &height, &depth);
	sprintf (s_width, "%d", width);
	sprintf (s_height, "%d", height);

	status = Tcl_VarEval (obm->tcl,
	    cb->name, " ",
	    obj->core.name, " ",
	    s_width, " ",
	    s_height, " ",
	    NULL); 

	if (status != TCL_OK) {
	    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
	    fprintf (stderr, "Error on line %d in %s: %s\n",
		obm->tcl->errorLine, cb->name,
		errstr ? errstr : obm->tcl->result);
	}
}


/* gtermResetCallback -- Low level callback procedure, called by the Gterm
 * widget when a reset event occurs.
 *
 * Callback: userproc
 */
static void
gtermResetCallback (cb, w)
ObmCallback cb;
Widget w;
{
	GtermObject obj = (GtermObject) cb->u.obj;
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	int status;

	status = Tcl_VarEval (obm->tcl,
	    cb->name, " ",
	    obj->core.name, " ",
	    NULL); 

	if (status != TCL_OK) {
	    char *errstr = Tcl_GetVar (obm->tcl, "errorInfo", 0);
	    fprintf (stderr, "Error on line %d in %s: %s\n",
		obm->tcl->errorLine, cb->name,
		errstr ? errstr : obm->tcl->result);
	}
}


/* SetCursorPos -- Warp the cursor (pointer) to the given coordinates.  This
 * is a graphics drawing command and if no raster number is specified the
 * current reference drawing raster, as set with setRaster, defines the
 * coordinate system.
 * 
 * Usage:	setCursorPos x y [raster]
 *
 * A raster number may optionally given to define the raster coordinate system
 * to be used.  raster=0 yields screen coordinates.
 */
static int 
gtermSetCursorPos (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	int raster, sv_raster, x, y;

	if (argc < 3)
	    return (TCL_ERROR);

	x = (int) atof (argv[1]);
	y = (int) atof (argv[2]);
	raster = (argc > 3) ? atoi (argv[3]) : -1;

	if (raster >= 0) {
	    sv_raster = GtGetRaster (wp->w);
	    if (raster != sv_raster)
		GtSetRaster (wp->w, raster);
	}

	GtSetCursorPos (wp->w, x, y);

	if (raster >= 0)
	    if (raster != sv_raster)
		GtSetRaster (wp->w, sv_raster);

	return (TCL_OK);
}


/* GetCursorPos -- Get the cursor position (raster 0 or screen coordinates).
 * 
 * Usage:	getCursorPos x y
 */
static int 
gtermGetCursorPos (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	char *xout, *yout, buf[SZ_NUMBER];
	int x, y;

	if (argc < 3)
	    return (TCL_ERROR);

	xout = argv[1];
	yout = argv[2];

	GtGetCursorPos (wp->w, &x, &y);
	sprintf (buf, "%d", x);
	Tcl_SetVar (obm->tcl, xout, buf, 0);
	sprintf (buf, "%d", y);
	Tcl_SetVar (obm->tcl, yout, buf, 0);

	return (TCL_OK);
}


/* setCursorType -- Set the cursor type.
 *
 * Usage:	setCursorType cursor-type
 *
 *	idle		default cursor
 *
 *	busy		busy cursor, e.g, when program is busy
 *
 *	ginMode		graphics input mode cursor, set when program is
 *			waiting for graphics input
 */
static int 
gtermSetCursorType (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char *cursor_type;
	int type;

	if (argc < 2)
	    return (TCL_ERROR);

	cursor_type = argv[1];
	if (strcmp (cursor_type, "idle") == 0)
	    type = GtIdleCursor;
	else if (strcmp (cursor_type, "busy") == 0)
	    type = GtBusyCursor;
	else if (strcmp (cursor_type, "ginMode") == 0)
	    type = GtGinmodeCursor;
	else
	    return (TCL_ERROR);

	GtSetCursorType (w, type);

	return (TCL_OK);
}


/* Bell -- Gterm widget sound output.
 *
 * Usage:	bell
 */
static int 
gtermBell (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtBell (w);
	return (TCL_OK);
}


/* setRaster -- Set the number of the raster to be used to define the drawing
 * context (e.g. coordinate system) for graphics and text drawing functions.
 *
 * Usage:	setRaster raster-number
 */
static int 
gtermSetRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int raster;

	if (argc < 2)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	GtSetRaster (w, raster);

	return (TCL_OK);
}


/* getRaster -- Get the number of the raster which defines the drawing
 * context, as set in the last setRaster call.
 *
 * Usage:	raster = getRaster [raster]
 *
 * If the name of a variable is given the raster number will be stored 
 * directly in that variable.
 */
static int 
gtermGetRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char buf[SZ_NUMBER], *raster_var;
	int raster;

	raster = GtGetRaster (w);
	sprintf (buf, "%d", raster);

	if (argc == 2)
	    Tcl_SetVar (obm->tcl, argv[1], buf, 0);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* setLogRes -- Set the logical resolution of the graphics drawing surface
 * in pixels.  This defines the range of coordinates in drawing commands
 * for drawing graphics such as lines, areas, or text.
 *
 * Usage:	setLogRes width height
 *
 * Note that this has nothing to do with imaging and the resolution of an
 * image raster.  The logical resolution of the graphics system is independent
 * of the physical resolution of the drawing surface.
 */
static int 
gtermSetLogRes (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int width, height;

	if (argc < 3)
	    return (TCL_ERROR);

	width = atoi (argv[1]);
	height = atoi (argv[2]);
	GtSetLogRes (w, width, height);

	return (TCL_OK);
}


/* getLogRes -- Get the logical resolution of the graphics drawing surface
 * in pixels.
 *
 * Usage:	getLogRes width height
 */
static int 
gtermGetLogRes (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char *s_width, *s_height;
	char buf[SZ_NUMBER];
	int width, height;

	if (argc < 3)
	    return (TCL_ERROR);

	s_width = argv[1];
	s_height = argv[2];
	GtGetLogRes (w, &width, &height);

	sprintf (buf, "%d", width);
	Tcl_SetVar (obm->tcl, s_width, buf, 0);
	sprintf (buf, "%d", height);
	Tcl_SetVar (obm->tcl, s_height, buf, 0);

	return (TCL_OK);
}


/* setPhysRes -- Set the physical resolution of the graphics drawing surface
 * in pixels.  This represents an attempt to resize the graphics window to
 * provide the requested resolution, i.e., to the given size width*height.
 *
 * Usage:	setPhysRes width height [raster]
 *
 * This function is equivalent to a createRaster request for the indicated
 * raster.  The default raster is the current drawing raster.
 */
static int 
gtermSetPhysRes (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int raster, width, height;

	if (argc < 3)
	    return (TCL_ERROR);

	width = atoi (argv[1]);
	height = atoi (argv[2]);
	raster = (argc > 3) ? atoi(argv[3]) : GtGetRaster(w);
	GtSetPhysRes (w, raster, width, height);

	return (TCL_OK);
}


/* getPhysRes -- Get the physical resolution of the graphics drawing surface
 * in pixels.
 *
 * Usage:	raster = getPhysRes width height [raster]
 *
 * If no raster number is specified the dimensions of the current drawing
 * raster are returned.
 */
static int 
gtermGetPhysRes (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char *s_width, *s_height;
	int raster, width, height;
	char buf[SZ_NUMBER];

	if (argc < 3)
	    return (TCL_ERROR);

	s_width = argv[1];
	s_height = argv[2];
	raster = (argc > 3) ? atoi(argv[3]) : GtGetRaster(w);
	GtGetPhysRes (w, raster, &width, &height);

	sprintf (buf, "%d", width);
	Tcl_SetVar (obm->tcl, s_width, buf, 0);
	sprintf (buf, "%d", height);
	Tcl_SetVar (obm->tcl, s_height, buf, 0);

	sprintf (buf, "%d", raster);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* setTextRes -- Set the resolution of the graphics drawing surface in
 * characters (e.g., 80x35).
 *
 * Usage:	setTextRes rows cols
 *
 * When drawing text the widget will space characters to achieve the desired
 * resolution.  When the drawing window is resized the widget will use this
 * number to select the best font.
 */
static int 
gtermSetTextRes (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int rows, cols;

	if (argc < 3)
	    return (TCL_ERROR);

	rows = atoi (argv[1]);
	cols = atoi (argv[2]);
	GtSetTextRes (w, rows, cols);

	return (TCL_OK);
}


/* setDataLevel -- Set the logical drawing function used when drawing graphics
 * or text.
 *
 * Usage:	setDataLevel level
 *
 * The recognized data levels for drawing are "set", "clear", and "invert".
 * Once set the data level remains in effect until the next clearScreen or
 * reset.
 */
static int 
gtermSetDataLevel (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int level;

	if (argc < 2)
	    return (TCL_ERROR);
	if ((level = dataLevelType (argv[1])) < 0)
	    return (TCL_ERROR);
	GtSetDataLevel (w, level);

	return (TCL_OK);
}


/* setLineWidth -- Set the line width for drawing operations.
 *
 * Usage:	setLineWidth width
 *
 * The line width is specified in integer pixels.  The value width=0 is
 * equivalent to width=1 but may permit faster drawing in some cases.
 * Once set the line width remains in effect until the next clearScreen or
 * reset.
 */
static int 
gtermSetLineWidth (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int width;

	if (argc < 2)
	    return (TCL_ERROR);
	if ((width = atoi (argv[1])) < 0)
	    return (TCL_ERROR);
	GtSetLineWidth (w, width);

	return (TCL_OK);
}


/* setLineStyle -- Set the line style for drawing operations.
 *
 * Usage:	setLineStyle style
 *
 * The line style determines whether a solid or dashed line is drawn.  The
 * recognized line styles are "solid", "dashed", "dotted", dashDot", and
 * "dash3dot".  Once set the line style remains in effect until the next
 * clearScreen or reset.
 */
static int 
gtermSetLineStyle (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int style;

	if (argc < 2)
	    return (TCL_ERROR);
	if ((style =  lineStyle (argv[1])) < 0)
	    return (TCL_ERROR);
	GtSetLineStyle (w, style);

	return (TCL_OK);
}


/* setColorIndex -- Set the gterm widget color index for drawing graphics and
 * text.
 *
 * Usage:	setColorIndex index
 *
 * The color index is an integer in the range 0 to N, where N is the maximum
 * number of color table entries permitted by the widget.  The gterm widget
 * implements a simple color allocation scheme: color index 0 is the background
 * color, 1 is the foreground, 2-9 are fixed, statically allocated colors (red,
 * green, blue, etc.) and color indices 10 and greater are dynamically
 * allocated private colors allocated at runtime by the application.
 *
 * Colors may be specified by number or by one of the names "background"
 * "foreground", "black", "white", "red", "green", "blue", "cyan", "yellow",
 * "magenta", "user1", or "user2".  These names are aliases for color indices
 * 0-9 and the actual color may differ from the logical color associated with
 * the given statically defined color index.
 *
 * Once set the drawing color remains in effect until the next clearScreen or
 * reset.
 */
static int 
gtermSetColorIndex (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int color;

	if (argc < 2)
	    return (TCL_ERROR);
	if ((color =  colorToIndex (argv[1])) < 0)
	    return (TCL_ERROR);
	GtSetColorIndex (w, color);

	return (TCL_OK);
}


/* setFillType -- Set the type of fill for area-fill drawing operations.
 *
 * Usage:	setFillType filltype
 *
 * The fill type may be "solid" or "outline".  Once set the fill type remains
 * in effect until the next clearScreen or reset.
 */
static int 
gtermSetFillType (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int filltype;

	if (argc < 2)
	    return (TCL_ERROR);
	if ((filltype =  fillType (argv[1])) < 0)
	    return (TCL_ERROR);
	GtSetFillType (w, filltype);

	return (TCL_OK);
}


/* clearScreen -- Clear the "screen", i.e., window.  This action clears the
 * drawing window and sets a number of drawing state variables to their default
 * values.
 *
 * Usage:	clearScreen
 */
static int 
gtermClearScreen (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtClearScreen (w);
	return (TCL_OK);
}


/* drawPolyline -- Draw a polyline.
 *
 * Usage:	drawPolyline points
 *
 * The points vector is a list of points, wherein each point is itself a list
 * consisting of two elements, the X and Y coordinates of the point.  The
 * coordinate system is the logical coordinate system defined by setLogRes.
 * All drawing attributes such as the line width, style, color, context raster
 * if any, and so on will affect the drawing operation.
 */
static int
gtermDrawPolyline (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;
	int npoints;

	if (argc < 2)
	    return (TCL_ERROR);

	if ((pv = get_points (argv[1], &npoints)) == NULL)
	    return (TCL_ERROR);
	GtDrawPolyline (w, pv, npoints);

	XtFree ((char *)pv);
	return (TCL_OK);
}


/* drawPolymarker -- Draw a polymarker, or sequence of points.
 *
 * Usage:	drawPolymarker points
 *
 * The points vector is a list of points, wherein each point is itself a list
 * consisting of two elements, the X and Y coordinates of the point.  The
 * coordinate system is the logical coordinate system defined by setLogRes.
 * All drawing attributes such as the line width, style, color, context raster
 * if any, and so on will affect the drawing operation.
 */
static int
gtermDrawPolymarker (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;
	int npoints;

	if (argc < 2)
	    return (TCL_ERROR);

	if ((pv = get_points (argv[1], &npoints)) == NULL)
	    return (TCL_ERROR);
	GtDrawPolymarker (w, pv, npoints);

	XtFree ((char *)pv);
	return (TCL_OK);
}


/* drawPolygon -- Draw a polygon, or filled area.
 *
 * Usage:	drawPolygon points
 *
 * The points vector is a list of points, wherein each point is itself a list
 * consisting of two elements, the X and Y coordinates of the point.  The
 * coordinate system is the logical coordinate system defined by setLogRes.
 * All drawing attributes such as the line width, style, color, context raster
 * if any, and so on will affect the drawing operation.
 */
static int
gtermDrawPolygon (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;
	int npoints;

	if (argc < 2)
	    return (TCL_ERROR);

	if ((pv = get_points (argv[1], &npoints)) == NULL)
	    return (TCL_ERROR);
	GtDrawPolygon (w, pv, npoints);

	XtFree ((char *)pv);
	return (TCL_OK);
}


/* drawMarker -- Draw a marker.
 *
 * Usage:	drawMarker type x y xsize ysize [rotangle]
 *
 * A marker of the indicated size and type is drawn at the indicated position.
 * The marker type is one of "box", "circle", "ellipse", and so on.
 */
static int
gtermDrawMarker (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	/* not yet implemented. */
	return (TCL_ERROR);
}


/* drawAlphaText -- Draw a graphics text string.
 *
 * Usage:	drawAlphaText x y text
 *
 * A text string is drawn at the indicated position using the current alpha
 * text font.  The font is selected based on the window size from a resource
 * defined list of alpha fonts of different sizes.  Alpha text is drawn like
 * line graphics, i.e., the background is visible through the text.  Drawing
 * attributes such as color, data level, context raster, etc. apply to text
 * as well as to line graphics.  Rotation of text strings is not supported.
 * The coordinaes X,Y refer to the lower left corner of the text string where
 * "lower" refers to the baseline of the font.  That is, if the text string
 * is "E", the coordinates x,y refer to the lower left corner of the E.
 */
static int
gtermDrawAlphaText (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char *text;
	int x, y;

	if (argc < 4)
	    return (TCL_ERROR);

	x = atoi (argv[1]);
	y = atoi (argv[2]);
	text = argv[3];
	GtDrawAlphaText (w, x, y, text);

	return (TCL_OK);
}


/* getAlphaTextSize -- Get the size in destination drawable pixels of an
 * alpha text string in terms of the current graphics context.
 *
 * Usage:	width = getAlphaTextSize [string [width [height [base]]]]
 *
 * The size in pixels of the given string is returned in the WIDTH and HEIGHT
 * output variables.  If no string or the null string is given the maximum
 * width and height of a single character in the font are returned.  If a BASE
 * output variable is given this variable will be set to the Y offset from the
 * top of the string to the baseline of the characters forming the string.
 */
static int
gtermGetAlphaTextSize (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;

	char *s_width, *s_height, *s_base;
	int width, height, base;
	char buf[SZ_NUMBER];
	char *text;

	if (argc > 1 && (int)strlen(argv[1]) > 0)
	    text = argv[1];
	else
	    text = NULL;

	s_width  = (argc > 2) ? argv[2] : NULL;
	s_height = (argc > 3) ? argv[3] : NULL;
	s_base   = (argc > 4) ? argv[4] : NULL;

	GtGetAlphaTextSize (w, text, &width, &height, &base);
	if (s_width) {
	    sprintf (buf, "%d", width);
	    Tcl_SetVar (obm->tcl, s_width, buf, 0);
	}
	if (s_height) {
	    sprintf (buf, "%d", height);
	    Tcl_SetVar (obm->tcl, s_height, buf, 0);
	}
	if (s_base) {
	    sprintf (buf, "%d", base);
	    Tcl_SetVar (obm->tcl, s_base, buf, 0);
	}

	sprintf (buf, "%d", width);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* startDialog -- Activate the dialog area for dialog text drawing.
 *
 * Usage:	startDialog
 *
 * Dialog text is text which is drawn into the dialog area at the bottom of
 * the gterm window.  Dialog text is transient and is not a permanent part of
 * the graphics being drawn.  Dialog text is normally used to interact with
 * the user or to display messages during program operation, without affecting
 * the graphics being drawn.
 *
 * startDialog is called to prepare the dialog area and initialize dialog
 * text mode, prior to drawing dialog text with drawDialogText.
 */
static int
gtermStartDialog (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;

	GtStartDialog (w);
	return (TCL_OK);
}


/* endDialog -- Deactivate the dialog area used for dialog text drawing.
 *
 * Usage:	endDialog
 *
 * endDialog is called when one is finished drawing dialog text, erasing
 * the dialog text area and terminating dialog text mode.
 */
static int
gtermEndDialog (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;

	GtEndDialog (w);
	return (TCL_OK);
}


/* eraseDialog -- Erase the dialog text area.
 *
 * Usage:	eraseDialog
 *
 * eraseDialog may be called at any time to erase the dialog text area without
 * exiting dialog text mode.
 */
static int
gtermEraseDialog (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;

	GtEraseDialog (w);
	return (TCL_OK);
}


/* drawDialogText -- Draw a dialog box text string.
 *
 * Usage:	drawDialogText x y text
 *
 * A text string is drawn at the indicated position using the current dialog
 * text font.  The font is selected based on the window size from a resource
 * defined list of dialog fonts of different sizes.  The attributes of dialog
 * text (color etc.) are determined by the widget resources and the window
 * size and are independent of the graphics context used for alpha text and
 * other graphics.  The coordinaes X,Y refer to the lower left corner of the
 * text string where "lower" refers to the baseline of the font.  That is, if
 * the text string is "E", the coordinates x,y refer to the lower left corner
 * of the E.
 */
static int
gtermDrawDialogText (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;
	char *text;
	int x, y;

	if (argc < 4)
	    return (TCL_ERROR);

	x = atoi (argv[1]);
	y = atoi (argv[2]);
	text = argv[3];
	GtDrawDialogText (w, x, y, text);

	return (TCL_OK);
}


/* getDialogTextSize -- Get the size in destination drawable pixels of an
 * dialog text string in terms of the current graphics context.
 *
 * Usage:	width = getDialogTextSize [string [width [height [base]]]]
 *
 * The size in pixels of the given string is returned in the WIDTH and HEIGHT
 * output variables.  If no string or the null string is given the maximum
 * width and height of a single character in the font are returned.  If a BASE
 * output variable is given this variable will be set to the Y offset from the
 * top of the string to the baseline of the characters forming the string.
 */
static int
gtermGetDialogTextSize (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	register XPoint *pv;

	char *s_width, *s_height, *s_base;
	int width, height, base;
	char buf[SZ_NUMBER];
	char *text;

	if (argc > 1 && (int)strlen(argv[1]) > 0)
	    text = argv[1];
	else
	    text = NULL;

	s_width  = (argc > 2) ? argv[2] : NULL;
	s_height = (argc > 3) ? argv[3] : NULL;
	s_base   = (argc > 4) ? argv[4] : NULL;

	GtGetDialogTextSize (w, text, &width, &height, &base);
	if (s_width) {
	    sprintf (buf, "%d", width);
	    Tcl_SetVar (obm->tcl, s_width, buf, 0);
	}
	if (s_height) {
	    sprintf (buf, "%d", height);
	    Tcl_SetVar (obm->tcl, s_height, buf, 0);
	}
	if (s_base) {
	    sprintf (buf, "%d", base);
	    Tcl_SetVar (obm->tcl, s_base, buf, 0);
	}

	sprintf (buf, "%d", width);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* rasterInit -- Initialize the raster subsystem, deleting all rasters and
 * mappings and freeing the dynamic part of the colortable.
 *
 * Usage:	rasterInit
 */
static int 
gtermRasterInit (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtRasterInit (w);
	return (TCL_OK);
}


/* assignRaster -- Assign a raster to a preexisting, externally defined
 * drawable (e.g. widget window or server pixmap).
 *
 * Usage:	assignRaster raster drawable
 *
 * The drawable may be the name of a widget object elsewhere in the GUI,
 * or the numeric server code for an arbitrary server pixmap or window.
 * A special case occurs when the named widget object is another gterm
 * widget.  In this case graphics pipelines can be constructed piping data
 * from the rasters and mappings of the first widget to those of the second,
 * using a mapping to connect the two.  When the destination raster is in
 * another gterm widget the widget code will automatically execute any
 * mappings defined on the affected raster when it is modified by the
 * mapping from the first widget.  This allows a raster to be mapped to
 * and displayed in multiple destination windows.
 */
static int 
gtermAssignRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject gt_obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &gt_obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	XtPointer drawable;
	ObmObject obj;
	int raster, type;
	char *object;

	if (argc < 3)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	object = argv[2];

	/* Get the drawable handle and type.
	 */
	if (isdigit (*object)) {
	    /* A server pixmap or window passed by server ID. */
	    drawable = (XtPointer) atoi(object);
	    type = GtWindow;

	} else {
	    /* A named object. */
	    if ((obj = obmFindObject (obm, object)) == NULL)
		return (TCL_ERROR);
	    if (obj->core.classrec->object_type != OtNonShell)
		return (TCL_ERROR);	/* no window */

	    if (obmClass (obj->core.classrec, WtGterm)) {
		/* Gterm widget.
		drawable = (XtPointer) widgetGetPointer (obj);
		 */
		drawable = (XtPointer) XtWindow (widgetGetPointer(obj));
		type = GtWidget;

	    } else {
		/* Some other type of widget. */
		drawable = (XtPointer) XtWindow (widgetGetPointer(obj));
		type = GtWindow;
	    }
	}

	if (GtAssignRaster (w, raster, drawable, type) == OK)
	    return (TCL_OK);
	else
	    return (TCL_ERROR);
}


/* createRaster -- Create a raster of the given type and size.
 *
 * Usage:	createRaster raster width height [type [depth]]
 *
 * A raster number RASTER is created with the given size and type.  The
 * possible raster types are "client", and "server", the default being
 * to create a client raster.  Rasters created in client memory are the
 * most general and are best for most applications.  Server side rasters
 * are used only in special applications; server rasters can be copied to
 * the display window very quickly but server memory is a limited resource
 * and a program using large amounts of server memory may not run on some
 * servers.  Mappings other than one-to-one are *less* efficient on server
 * rasters than on client rasters.
 *
 * Currently only rasters of depth 8 bits are supported.  A createRaster on
 * an existing raster will destroy the old raster, along with any mappings
 * defined on it, and create a new one.
 *
 * Raster number zero is the gterm widget's display window.  If one attempts
 * a createRaster on this window the widget will try to resize the window.
 * The resize attempt may or may not succeed, depending upon the resize
 * restrictions of the geometry or window managers in use, shell resources,
 * and so on.
 *
 * There is a limit on the maximum number of rasters which can be created,
 * set by the gterm widget resource maxRasters at widget creation.
 */
static int
gtermCreateRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int raster, width, height, type, depth;
	char *s_type;

	if (argc < 4)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	width  = atoi (argv[2]);
	height = atoi (argv[3]);
	s_type = (argc > 4) ? argv[4] : "client";
	depth  = (argc > 5) ? atoi(argv[5]) : 0;

	if (strcmp (s_type, "server") == 0)
	    type = GtServer;
	else
	    type = GtClient;

	if (GtCreateRaster (w, raster, type, width, height, depth) == ERR)
	    return (TCL_ERROR);

	return (TCL_OK);
}


/* destroyRaster -- Destroy a raster.
 *
 * Usage:	destroyRaster raster
 *
 * Raster number RASTER is destroyed along with all of its mappings.  This is
 * a no-op if the raster is not currently defined.  Attempts to destroy raster
 * number zero (the widget's window) are ignored.
 */
static int
gtermDestroyRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int raster;

	if (argc < 2)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	GtDestroyRaster (w, raster);

	return (TCL_OK);
}


/* queryRaster -- Query a raster's attributes.
 *
 * Usage:	exists = queryRaster raster [width height [type [depth]]]
 *
 * The width and height, and optionally the type and depth, of raster number
 * RASTER are returned in the named output variables.  The boolean function
 * value indicates whether or not the raster is currently defined.  If the
 * raster does not exist the output variables may be undefined after the call.
 */
static int
gtermQueryRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	char *s_width, *s_height, *s_type, *s_depth;
	int exists, raster, width, height, type, depth;
	char buf[SZ_NUMBER], *v_type;

	if (argc < 2)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	s_width  = (argc > 2) ? argv[2] : NULL;
	s_height = (argc > 3) ? argv[3] : NULL;
	s_type   = (argc > 4) ? argv[4] : NULL;
	s_depth  = (argc > 5) ? argv[5] : NULL;

	if (GtQueryRaster (w, raster, &type, &width, &height, &depth)) {
	    if (s_width) {
		sprintf (buf, "%d", width);
		Tcl_SetVar (obm->tcl, s_width, buf, 0);
	    }
	    if (s_height) {
		sprintf (buf, "%d", height);
		Tcl_SetVar (obm->tcl, s_height, buf, 0);
	    }
	    if (s_type) {
		v_type = (type == GtServer) ? "server" : "client";
		Tcl_SetVar (obm->tcl, s_type, v_type, 0);
	    }
	    if (s_depth) {
		sprintf (buf, "%d", depth);
		Tcl_SetVar (obm->tcl, s_depth, buf, 0);
	    }
	    Tcl_SetResult (obm->tcl, TRUESTR, TCL_STATIC);

	} else {
	    Tcl_SetResult (obm->tcl, FALSESTR, TCL_STATIC);
	}

	return (TCL_OK);
}


/* nextRaster -- Return the number of the next available, unused raster.
 *
 * Usage:	raster = nextRaster
 *
 */
static int
gtermNextRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char buf[SZ_NUMBER];
	int raster;

	if ((raster = GtNextRaster (w)) < 0)
	    return (TCL_ERROR);
	else {
	    sprintf (buf, "%d", raster);
	    Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);
	}

	return (TCL_OK);
}


/* activeRasters -- Return the number of currently defined rasters.
 *
 * Usage:	count = activeRasters
 *
 */
static int
gtermActiveRasters (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char buf[SZ_NUMBER];

	sprintf (buf, "%d", GtNRasters(w));
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* setPixel -- Set the value of a single pixel.
 *
 * Usage:	setPixel raster x y value
 *
 * 	raster		The raster number.
 *
 *	x, y		The pixel to be set.
 *
 *	value		The pixel value.
 *
 * This routine is more efficient than writePixels for setting the value of
 * a single pixel, but is a lot less efficient if a block of pixels are to
 * be set.
 */
static int 
gtermSetPixel (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	Widget w = wp->w;
	int raster, x, y;
	uchar data[1];

	if (argc < 5)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	x = atoi (argv[2]);
	y = atoi (argv[3]);
	data[0] = atoi (argv[4]);
	GtWritePixels (w, raster, data, 8, x, y, 1, 1);

	return (TCL_OK);
}


/* getPixel -- Get the value of a single pixel.
 *
 * Usage:	getPixel raster x y
 *
 * 	raster		The raster number.
 *
 *	x, y		The pixel to be set.
 *
 * This routine is more efficient than readPixels for getting the value of
 * a single pixel, but is a lot less efficient if a block of pixels are to
 * be read.
 */
static int 
gtermGetPixel (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	Widget w = wp->w;
	char buf[SZ_NUMBER];
	int raster, x, y;
	uchar data[1];

	if (argc < 4)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	x = atoi (argv[2]);
	y = atoi (argv[3]);
	GtReadPixels (w, raster, data, 8, x, y, 1, 1);

	sprintf (buf, "%d", data[0]);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* writePixels -- Set the values of some subset of the pixels in a raster.
 * If any mappings are defined on the affected region and are enabled, any
 * destination rasters will be automatically updated as defined by the mapping.
 *
 * Usage:	writePixels raster pixels encoding nbits x1 y1 nx ny [bias]
 *
 * 	raster		The raster number.
 *
 *	pixels		The pixel array, encoded as a string.
 *
 *	encoding	The pixel encoding.  "numeric" means each pixel is
 *			encoded as a decimal integer delimited by whitespace.
 *
 *			"hex1" means the pixel array is hex encoded, 1 bytes
 *			per 8 bit pixel, as a printable text string.  Hex1
 *			encoding can only be used for pixel values in the
 *			range 0-63.  The 64 possible pixel values are encoded
 *			as follows:  '0'-'9', 'A'-'Z', 'a'-'z', '$', '_'.
 *			(Since there are 26 letters this is 10+26+26+1+1=64).
 *
 *			"hex2" means the pixel array is hex encoded, 2 bytes
 *			per 8 bit pixel, as a printable text string.  The
 *			two bytes are defined as follows (v = pixel value):
 *
 *			     byte1 = ((v >> 4) & 017) in hex [0-9A-F]
 *			     byte2 = ((v     ) & 017) in hex [0-9A-F]
 *			
 *			Either "hex1" or "hex2" followed by "-rle", e.g.,
 *			"hex2-rle" means that the hex-encoded data is in
 *			turn run length encoded.  In a run length encoded
 *			string all characters are data characters except
 *			for "@" and "%", which are repeat operators.  "@" is
 *			followed by a single hex1-encoded character giving
 *			the repeat count minus one: the hex1-encoded number
 *			is decoded as N and the most recent pixel value is
 *			repeated N+1 times.  "%" is similar, but is followed
 *			by a 2 character hex2-encoded repeat count.
 *
 *			Whitespace in a hex encoded string is ignored.
 *			Hex2 encoding reduces the data volume by about a factor
 *			of two (compared to numeric) and is only a factor of
 *			two less space efficient than binary.  Hex1 encoding
 *			is as space efficient as binary but pixel values larger
 *			than 63 (64 possible values) cannot be represented.
 *
 *	nbits		Number of bits per pixel - currently only 8 bit pixels
 *			are supported.
 *
 *	x1,y1,nx,ny	Region of the raster to be written to.
 *
 *	bias		If a bias value is given this value is added to the
 *			value of each input pixel.
 *
 * Most real-world image processing applications get the Gterm widget handle
 * with setGterm and pass binary data to the widget by calling GtWritePixels
 * directly.  This is the most efficient approach for serious image processing
 * where large amounts of data are involved.  However, being able to read and
 * write raster pixels directly in a GUI can be useful in specialized
 * applications, e.g., where the image is computed or modified by the GUI.
 */
static int 
gtermWritePixels (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	Widget w = wp->w;

	register char *ip;
	register uchar *op;
	register int v, i, j;
	static uchar hex1[256], hex2[256];
	static int have_tables = 0;
	int raster, nbits, bias;
	char *pixels, *encoding;
	int x1, y1, nx, ny;
	uchar *data, *otop;

	if (argc < 9)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	pixels = argv[2];
	encoding = argv[3];
	nbits = atoi (argv[4]);
	x1 = atoi (argv[5]);
	y1 = atoi (argv[6]);
	nx = atoi (argv[7]);
	ny = atoi (argv[8]);
	bias = (argc > 9) ? atoi(argv[9]) : 0;

	if (nbits != 8)
	    return (TCL_ERROR);

	/* Generate hex to binary lookup tables in first call. */
	if (!have_tables) {
	    /* Generate char-to-binary table for the hex1 encoding. */
	    for (i=0;  i < 256;  i++)
		hex1[i] = 0177;
	    for (i='0';  i <= '9';  i++)
		hex1[i] = i - '0';
	    for (i='A';  i <= 'Z';  i++)
		hex1[i] = i - 'A' + 10;
	    for (i='a';  i <= 'z';  i++)
		hex1[i] = i - 'a' + 36;
	    hex1['$'] = 62;
	    hex1['_'] = 63;

	    /* Generate char-to-binary table for the hex2 encoding. */
	    for (i=0;  i < 256;  i++)
		hex2[i] = 0177;
	    for (i='0';  i <= '9';  i++)
		hex2[i] = i - '0';
	    for (i='a';  i <= 'f';  i++)
		hex2[i] = i - 'a' + 10;
	    for (i='A';  i <= 'F';  i++)
		hex2[i] = i - 'A' + 10;

	    have_tables++;
	}

	/* Decode the pixel data. */
	if (!(data = (uchar *) XtMalloc (nx * ny)))
	    return (TCL_ERROR);
	otop = data + nx * ny;

	/* Uncompress the input if RLE compression is indicated. */
	if (strcmp (&encoding[strlen(encoding)-4], "-rle") == 0) {
	    int buflen = nx * ny * 2;
	    char *ibuf, *op;
	    int ch;

	    /* Get buffer to hold the uncompressed pixel data array. */
	    if (!(ibuf = (char *) XtMalloc (buflen + 1)))
		goto err;

	    /* Uncompress the pixel array.  */
	    for (ip=pixels, op=ibuf;  *ip;  ) {
		while (isspace (*ip))
		    ip++;

		if ((ch = *ip++) == '@') {
		    if ((i = hex1[*ip++]) >= 0x7f)
			while (*ip && ((i = hex1[*ip++]) >= 0x7f))
			    ;
		    if (op-ibuf + i + 1 > buflen)
			goto err;
		    for (v = *(op-1), i++;  --i >= 0;  )
			*op++ = v;

		} else if (ch == '%') {
		    if ((i = hex2[*ip++]) >= 0x7f)
			while (*ip && ((i = hex2[*ip++]) >= 0x7f))
			    ;
		    if ((j = hex2[*ip++]) >= 0x7f)
			while (*ip && ((j = hex2[*ip++]) >= 0x7f))
			    ;
		    i = ((i << 4) | j) + 1;
		    if (op-ibuf + i > buflen)
			goto err;
		    for (v = *(op-1);  --i >= 0;  )
			*op++ = v;

		} else
		    *op++ = ch;
	    }

	    *op = '\0';
	    pixels = ibuf;
	}

	/* Convert the ascii pixels array to a binary data array.
	 */
	if (strcmp (encoding, "numeric") == 0) {
	    for (ip=pixels;  isspace(*ip) || *ip == '{';  ip++)
		;
	    for (op=data;  *ip && op < otop;  ) {
		for (v=0;  isdigit(*ip);  )
		    v = v * 10 + *ip++ - '0';
		*op++ = v + bias;
		while (isspace(*ip) || *ip == '}')
		    ip++;
	    }
	} else if (strncmp (encoding, "hex1", 4) == 0) {
	    for (ip=pixels, op=data;  *ip && op < otop;  ) {
		if ((v = hex1[*ip++]) > 0xf)
		    while (*ip && ((v = hex1[*ip++]) > 0xf))
			;
		*op++ = v + bias;
	    }
	} else if (strncmp (encoding, "hex2", 4) == 0) {
	    for (ip=pixels, op=data;  *ip && op < otop;  ) {
		if ((v = hex2[*ip++]) > 0xf)
		    while (*ip && ((v = hex2[*ip++]) > 0xf))
			;
		if ((i = hex2[*ip++]) > 0xf)
		    while (*ip && ((i = hex2[*ip++]) > 0xf))
			;
		*op++ = ((v << 4) | i) + bias;
	    }
	} else {
err:	    XtFree ((char *)data);
	    if (pixels != argv[2])
		XtFree (pixels);
	    return (TCL_ERROR);
	}

	/* Write the pixels. */
	GtWritePixels (w, raster, data, nbits, x1, y1, nx, ny);
	XtFree ((char *)data);
	if (pixels != argv[2])
	    XtFree (pixels);

	return (TCL_OK);
}


/* readPixels -- Get the values of some subset of the pixels in a raster.
 *
 * Usage:	pixels = readPixels raster encoding nbits x1 y1 nx ny [bias]
 *
 * 	raster		The raster number.
 *
 *	encoding	The pixel encoding.  "numeric" means each pixel is
 *			encoded as a decimal integer delimited by whitespace.
 *			"hex1", hex2", and "hex1-rle or "hex2-rle" are
 *			possible encodings.  See writePixels for details.
 *
 *	nbits		Number of bits per pixel - currently only 8 bit pixels
 *			are supported.
 *
 *	x1,y1,nx,ny	Region of the raster to be read.
 *
 *	bias		The bias value is subtracted from the pixel value
 *			returned by readPixels.
 *
 * The pixel array, encoded as a string, is returned as the function value.
 * Use readPixels to read a block of pixels, and getPixel to get the value
 * of a single pixel.
 */
static int 
gtermReadPixels (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	Widget w = wp->w;

	register int v, i;
	register uchar *ip, *op;
	int bias, nchars, npix, n, j;
	char *pixels, *encoding;
	int x1, y1, nx, ny;
	int raster, nbits;
	uchar *data;
	char *buf;

	if (argc < 8)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	encoding = argv[2];
	nbits = atoi (argv[3]);
	x1 = atoi (argv[4]);
	y1 = atoi (argv[5]);
	nx = atoi (argv[6]);
	ny = atoi (argv[7]);
	bias = (argc > 8) ? atoi(argv[8]) : 0;
	npix = nx * ny;

	if (nbits != 8)
	    return (TCL_ERROR);

	/* Get the pixel data. */
	if (!(data = (uchar *) XtMalloc (npix * sizeof(uchar))))
	    return (TCL_ERROR);
	if (GtReadPixels (w, raster, data, nbits, x1, y1, nx, ny) == ERR) {
	    XtFree ((char *)data);
	    return (TCL_ERROR);
	}

	/* Get a text buffer large enough to hold the encoded data. */
	nchars = npix * 4 + (npix/16) * 3 + 5;
	if (!(buf = (char *) XtMalloc (nchars))) {
	    XtFree ((char *)data);
	    return (TCL_ERROR);
	}

	/* Encode the pixel data as a printable text string using the
	 * encoding specified by the caller.
	 */
	if (strcmp (encoding, "numeric") == 0) {
	    /* Encode the data as {ddd ddd ddd ... ddd}. */
	    op = (uchar *)buf;
	    *op++ = '{';
	    *op++ = ' ';

	    for (ip=data, n=npix, j=0;  --n >= 0;  ) {
		v = *ip++ - bias;

		i = (v / 100);
		if (i) {
		    *op++ = i + '0';
		    v -= i * 100;
		} else
		    *op++ = ' ';

		i = (v / 10);
		if (i) {
		    *op++ = i + '0';
		    v -= i * 10;
		} else
		    *op++ = ' ';

		*op++ = v + '0';
		*op++ = ' ';

		if (++j >= 16) {
		    *op++ = '\n';
		    *op++ = ' ';
		    *op++ = ' ';
		    j = 0;
		}
	    }

	    if (j)
		*op++ = '\n';
	    *op++ = '}';
	    *op++ = '\0';
	    
	} else if (strncmp (encoding, "hex", 3) == 0) {
	    static uchar hex1[256], hex2[256*2];
	    static int have_tables = 0;
	    uchar *obuf, *cbuf;

	    if (!have_tables) {
		/* Generate binary to hex1 (64 element) lookup table. */
		for (n=0, op=hex1;  n < 256;  n++) {
		    i = (n % 64);
		    if (i < 10)
			*op++ = i + '0';
		    else if (i < 36)
			*op++ = (i - 10) + 'A';
		    else if (i < 62)
			*op++ = (i - 36) + 'a';
		    else if (i == 62)
			*op++ = '$';
		    else
			*op++ = '_';
		}

		/* Generate binary to hex2 (256 element) lookup table. */
		for (n=0, op=hex2;  n < 256;  n++) {
		    i = ((n >> 4) & 017);
		    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
		    i = (n & 017);
		    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
		}

		have_tables++;
	    }

	    if ((obuf = (uchar *) XtMalloc (npix*2)) == NULL)
		return (TCL_ERROR);

	    if (strncmp (encoding, "hex1", 4) == 0) {
		/* Hex1 encoding uses only one character per pixel but the
		 * pixel range is restricted to 0 to 63.
		 */
		for (j=0, ip=data, op=obuf;  j < ny;  j++) {
		    for (i=0;  i < nx;  i++)
			*op++ = hex1[*ip++ - bias];
		}
		*op = '\0';

	    } else if (strncmp (encoding, "hex2", 4) == 0) {
		/* Hex2 encoding uses 2 characters per pixel and supports
		 * pixel values in the range 0 to 255.
		 */
		for (j=0, ip=data, op=obuf;  j < ny;  j++) {
		    for (i=0;  i < nx;  i++) {
			v = (*ip++ - bias) * 2;
			*op++ = hex2[v];
			*op++ = hex2[v+1];
		    }
		    if (nx % 2)
			ip++;
		}
		*op = '\0';
	    }

	    /* Run length compress the data.  The compressed data stream
	     * contains a mixture of literal data codes and repeat codes.
	     * A "@" followed by a hex1-encoded number N causes the most
	     * recent pixel value to be repeated N+1 times, where N < 64.
	     * A "%" followed by a hex2-encoded number N causes the most
	     * recent pixel value to be repeated N+1 times, where N < 256.
	     */
	    if (strcmp (&encoding[strlen(encoding)-4], "-rle") == 0) {
		if ((cbuf = (unsigned char *) XtMalloc (npix*3)) == NULL)
		    return (TCL_ERROR);

		ip = obuf;
		op = cbuf;
		*op++ = v = *ip++;
		while (*ip) {
		    for (n=0;  n < 256 && *ip == v;  ip++, n++)
			;
		    if (n == 0) {
			*op++ = v = *ip++;
		    } else if (n < 3) {
			while (--n >= 0)
			    *op++ = v;
		    } else if (n <= 64) {
			*op++ = '@';
			*op++ = hex1[n-1];
		    } else if (n <= 256) {
			*op++ = '%';
			*op++ = hex2[(n-1)*2];
			*op++ = hex2[(n-1)*2+1];
		    }
		}
		*op = '\0';

		XtFree ((char *)obuf);
		obuf = cbuf;
	    }

	    /* Output the encoded pixel data.
	     */
	    op = (uchar *)buf;
	    *op++ = '{';
	    *op++ = ' ';

	    for (ip=obuf, n=1;  *ip;  ip++, n++) {
		*op++ = *ip;
		if (n && n > 72) {
		    *op++ = '\n';
		    *op++ = ' ';
		    *op++ = ' ';
		    n = 0;
		}
	    }

	    if (n)
		*op++ = '\n';
	    *op++ = '}';
	    *op++ = '\0';
	    XtFree ((char *)obuf);

	} else {
	    XtFree ((char *)data);
	    return (TCL_ERROR);
	}

	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);
	XtFree ((char *)data);
	XtFree ((char *)buf);

	return (TCL_OK);
}


/* refreshPixels -- Refresh any mappings the source rect of which intersects
 * the given region of the specified raster.
 *
 * Usage:	refreshPixels raster ctype x1 y1 nx ny
 *
 * Any mappings defined on the region [x1,y1,nx,ny] of the given raster are
 * updated, redisplaying the indicated region.  refreshPixels is like
 * writePixels except that the affected pixels are redisplayed without
 * actually having been modified.  Raster coordinates may be given in either
 * raster pixel coordinates (ctype=Pixel) or NDC coordinates (ctype=NDC)
 * in the range 0-1 floating.  The origin in the upper left for raster
 * coordinates and in the lower left for NDC coordinates.  Raster coordinates
 * are zero indexed.
 */
static int
gtermRefreshPixels (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	float x1, y1, nx, ny;
	int raster, ctype;

	if (argc < 7)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	ctype = coordType (argv[2]);
	x1 = atof (argv[3]);
	y1 = atof (argv[4]);
	nx = atof (argv[5]);
	ny = atof (argv[6]);

	GtRefreshPixels (w, raster, ctype, x1, y1, nx, ny);

	return (TCL_OK);
}


/* setPixels -- Set a region of a raster to a single color.
 *
 * Usage:	setPixels raster [color [ctype x1 y1 nx ny [rop]]]
 *
 * The region [x1,y1,nx,ny] of raster RASTER, specified in the coordinate
 * system CTYPE (Pixel or NDC) is set to the color number COLOR.  If no
 * region is specified the entire raster is assumed.  The color number is
 * specified in the color system defined by the client, which may or may not
 * be the same as the internal gterm widget color system (the client and
 * widget color systems are the same only if no iomap has been specified,
 * or, equivalently, if the iomap is one-to-one).  If no color is given the
 * background color is assumed.  Any mappings mapped to the affected pixels
 * will be updated to propagate and possibly display the changes.
 *
 * Although this routine permits an optional rasterop argument (ROP) there
 * is currently no support for symbolically defining the bitfields used to
 * form this word.  Most applications do not need to specify a rasterop.
 */
static int
gtermSetPixels (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int color, rop, raster, ctype;
	float x1, y1, nx, ny;

	if (argc < 2)
	    return (TCL_ERROR);

	raster = atoi (argv[1]);
	color = (argc > 2) ? atoi(argv[2]) : GtGetClientPixel(w,0);

	if (argc >= 8) {
	    ctype = coordType (argv[3]);
	    x1 = atof (argv[4]);
	    y1 = atof (argv[5]);
	    nx = atof (argv[6]);
	    ny = atof (argv[7]);
	} else {
	    ctype = GtNDC;
	    x1 = 0;
	    y1 = 0;
	    nx = 1.0;
	    ny = 1.0;
	}

	rop = (argc > 8) ? atoi(argv[8]) : 0;

	if (ctype == GtNDC) {
	    x1 *= MAXNDC;  y1 *= MAXNDC;
	    nx *= MAXNDC;  ny *= MAXNDC;
	}

	if (GtSetPixels (w, raster, ctype, (int)x1, (int)y1, (int)nx, (int)ny,
		color, rop) == ERR)
	    return (TCL_ERROR);

	return (TCL_OK);
}


/* extractPixmap -- Extract a region of a raster into a pixmap.
 *
 * Usage:	extractPixmap pixmap raster [ctype x1 y1 nx ny]
 *
 * The given region of raster RASTER is extracted and placed into a pixmap
 * object with name PIXMAP.  The pixmap object is created if it does not
 * already exist.  The size of the pixmap object will be the size of the
 * extracted region.  If no region is given the entire raster is assumed.
 */
static int
gtermExtractPixmap (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	float x1, y1, nx, ny;
	int raster, ctype;
	Pixmap pixmap;
	char *s_pixmap;

	if (argc < 3)
	    return (TCL_ERROR);

	s_pixmap = argv[1];
	raster = atoi (argv[2]);

	if (argc >= 8) {
	    ctype = coordType (argv[3]);
	    x1 = atof (argv[4]);
	    y1 = atof (argv[5]);
	    nx = atof (argv[6]);
	    ny = atof (argv[7]);
	} else {
	    ctype = GtNDC;
	    x1 = 0;
	    y1 = 0;
	    nx = 1.0;
	    ny = 1.0;
	}

	if (pixmap = GtExtractPixmap (w, raster, ctype, x1, y1, nx, ny))
	    createPixmap (obm, s_pixmap, nx, ny, 8, pixmap, NULL, 0, 0);
	else
	    return (TCL_ERROR);

	return (TCL_OK);
}


/* insertPixmap -- Insert a pixmap into a region of a raster.
 *
 * Usage:	insertPixmap pixmap raster [ctype x1 y1 nx ny]
 *
 * The given pixmap PIXMAP is inserted to raster RASTER at the given
 * location.  If no region is given the entire raster is assumed.
 */
static int
gtermInsertPixmap (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	float x1, y1, nx, ny;
	int raster, ctype;
	Pixmap pixmap;
	char *s_pixmap;

	if (argc < 3)
	    return (TCL_ERROR);

	s_pixmap = argv[1];
	raster = atoi (argv[2]);

	if (argc >= 8) {
	    ctype = coordType (argv[3]);
	    x1 = atof (argv[4]);
	    y1 = atof (argv[5]);
	    nx = atof (argv[6]);
	    ny = atof (argv[7]);
	} else {
	    ctype = GtNDC;
	    x1 = 0;
	    y1 = 0;
	    nx = 1.0;
	    ny = 1.0;
	}

	if (pixmap = findPixmap (obm, s_pixmap)) {
	    if (GtInsertPixmap (w, pixmap, raster, ctype,x1,y1,nx,ny) == ERR)
		return (TCL_ERROR);
	} else
	    return (TCL_ERROR);

	return (TCL_OK);
}


/* nextColormap -- Get the index of the next unused colormap.
 *
 * Usage:	colormap = nextColormap
 *
 * Colormaps are dynamically allocated so there is no builtin limit on the
 * number of colormaps.
 */
static int
gtermNextColormap (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char buf[SZ_NUMBER];

	sprintf (buf, "%d", GtNextColormap (w));
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* freeColormap -- Free a colormap.
 *
 * Usage:	freeColormap colormap
 *
 * The given colormap and all of its resources are freed.  This is a no-op if
 * the given colormap is not defined.
 */
static int
gtermFreeColormap (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int colormap;

	if (argc < 2)
	    return (TCL_ERROR);

	colormap = atoi (argv[1]);
	GtFreeColormap (w, colormap);

	return (TCL_OK);
}


/* writeColormap -- Write to a colormap.
 *
 * Usage:	writeColormap colormap colors [offset]
 *
 * The given list of colors are loaded into the given colormap.  If no
 * offset is specified the offset will default to the offset of the first
 * dynamically allocatable color cell (e.g. 10).  Colormap zero is the
 * window colormap and writing to this colormap will immediately affect
 * the display.  The nonzero colormaps are merely stored within the gterm
 * widget and will not take effect until loaded with loadColormap.
 *
 * Colors are specified as a list of RGB color triplets in the range 0-255.
 * For example, { {R G B} {R G B} ...}.
 *
 * The gterm widget supports both private colormaps and the default colormap.
 * Which is used is controlled by the cmapName resource; writeColormap works
 * the same way for both types of colormaps.
 */
static int 
gtermWriteColormap (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	ObmContext obm = obj->widget.obm;
	GtermPrivate gp = &obj->gterm;
	Widget w = obj->widget.w;

	register int ncolors;
	register char *ip;
	char *colors, *ip_save;
	ushort r[MAX_COLORS];
	ushort g[MAX_COLORS];
	ushort b[MAX_COLORS];
	int offset, colormap;
	char *ipp;

	if (argc < 3)
	    return (TCL_ERROR);

	colormap = atoi (argv[1]);
	colors = argv[2];
	offset = (argc > 3) ? atoi(argv[3]) : FIRST_COLOR;

	for (ncolors=0, ip=colors;  *ip && ncolors < MAX_COLORS;  ) {
	    while (isspace(*ip) || *ip == '{')
		ip++;

	    ip_save = ip;
	    r[ncolors] = (strtol (ip, &ipp, 10)) << 8;  ip = ipp;
	    g[ncolors] = (strtol (ip, &ipp, 10)) << 8;  ip = ipp;
	    b[ncolors] = (strtol (ip, &ipp, 10)) << 8;  ip = ipp;
	    if (ip == ip_save)
		return (TCL_ERROR);

	    while (isspace(*ip) || *ip == '}')
		ip++;

	    ncolors++;
	}

	if (GtWriteColormap (w, colormap, offset, ncolors, r, g, b) == ERR)
	    return (TCL_ERROR);

	return (TCL_OK);
}


/* readColormap -- Read from a colormap.
 *
 * Usage:	ncolors = readColormap colormap colors [offset [ncolors]]
 *
 * The given region of the colormap is read and return in the output variable
 * COLORS.  The actual number of color values read is returned as the function
 * value.  If no offset is specified the offset will default to the offset of
 * the first dynamically allocatable color cell (e.g. 10).  If the number of
 * colors to be read (NCOLORS) is not specified readColormap will return a
 * list of all the allocated colors starting at the specified colortable
 * offset.
 *
 * Colors are returned as a list of RGB color triplets in the range 0-255.
 * For example, { {R G B} {R G B} ...}.
 */
static int 
gtermReadColormap (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	ObmContext obm = obj->widget.obm;
	GtermPrivate gp = &obj->gterm;
	Widget w = obj->widget.w;

	register int i;
	register char *op;
	char colors[MAX_COLORS * 3 * 20];
	ushort r[MAX_COLORS];
	ushort g[MAX_COLORS];
	ushort b[MAX_COLORS];
	int offset, colormap;
	int ncolors, request;
	char buf[SZ_NUMBER];
	char *s_colors;

	if (argc < 3)
	    return (TCL_ERROR);

	colormap = atoi (argv[1]);
	s_colors = argv[2];
	offset = (argc > 3) ? atoi(argv[3]) : FIRST_COLOR;
	request = (argc > 4) ? atoi(argv[4]) : MAX_COLORS;

	ncolors = GtReadColormap (w, colormap, offset, request, r, g, b);

	op = colors;
	*op++ = '{';
	*op++ = ' ';
	for (i=0;  i < ncolors;  i++) {
	    sprintf (op, "{%d %d %d} ", (r[i] >> 8), (g[i] >> 8), (b[i] >> 8));
	    while (*op)
		op++;
	}
	*op++ = '}';
	*op++ = '\0';
	Tcl_SetVar (obm->tcl, s_colors, colors, 0);

	sprintf (buf, "%d", ncolors);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* loadColormap -- Load a colormap.
 *
 * Usage:	loadColormap colormap [offset [scale]]
 *
 * The offset and scale parameters may be used to adjust the brightness and
 * contrast of the image when the colormap is loaded.  The normalized colormap
 * has offset=0.5, scale=1.0.  Colormap zero is the hardware colormap.
 */
static int 
gtermLoadColormap (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	ObmContext obm = obj->widget.obm;
	GtermPrivate gp = &obj->gterm;
	Widget w = obj->widget.w;
	float offset, scale;
	int colormap;

	if (argc < 2)
	    return (TCL_ERROR);

	colormap = atoi (argv[1]);
	offset = (argc > 2) ? atof(argv[2]) : gp->offset;
	scale  = (argc > 3) ? atof(argv[3]) : gp->scale;

	GtLoadColormap (w, colormap, offset, scale);

	gp->colormap = colormap;
	gp->offset = offset;
	gp->scale = scale;

	return (TCL_OK);
}


/* clientPixel -- Convert a gterm pixel to a client pixel.
 *
 * Usage:	pixel = clientPixel gterm_pixel
 *
 * If the client has an iomap installed, gterm i/o operations which deal
 * with pixel values will map to and from client (external) pixels and the
 * internal gterm widget color model.  The clientPixel routine can be used to
 * convert a pixel (i.e. color) in the gterm color model to the corresponding
 * client pixel in the client's color model.  For example "clientPixel 0"
 * will return the client pixel corresponding to the gterm widget background
 * color.
 */
static int
gtermClientPixel (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int pixel, client_pixel;
	char buf[SZ_NUMBER];

	if (argc < 2)
	    return (TCL_ERROR);

	pixel = atoi (argv[1]);
	client_pixel = GtGetClientPixel (w, pixel);

	sprintf (buf, "%d", client_pixel);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* getBias -- Get the colormap bias value.
 *
 * Usage:	bias = getBias [nelem [maxelem]]
 *
 * The colormap bias value is the pixel value corresponding to the first
 * dynamically allocatable colormap cell.  That is, the gterm widget
 * defines N preallocated static colors 0 to N-1, followed by an arbitrary
 * number of dynamically allocatable colors.  The bias value is the pixel
 * value of the first dynamically allocatable color.
 *
 * If the optional arguments nelem and maxelem are given then the number
 * of currently allocated colors and the maximum number of allocatable colors
 * are returned.  The latter values do not include the N=bias static colors,
 * i.e. nelem=0 if no dynamic colors have been allocated.
 */
static int
gtermGetBias (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int first, nelem, maxelem;
	char buf[SZ_NUMBER];

	GtQueryColormap (w, 0, &first, &nelem, &maxelem);

	if (argc > 1) {
	    sprintf (buf, "%d", nelem);
	    Tcl_SetVar (obm->tcl, argv[1], buf, 0);
	}
	if (argc > 2) {
	    sprintf (buf, "%d", maxelem);
	    Tcl_SetVar (obm->tcl, argv[2], buf, 0);
	}

	sprintf (buf, "%d", first);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* initMappings -- Initialize the mapping subsystem, deleting any existing
 * mappings.
 *
 * Usage:	initMappings
 */
static int 
gtermInitMappings (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;

	GtInitMappings (w);
	return (TCL_OK);
}


/* nextMapping -- Return the index of the next unused mapping.
 *
 * Usage:	mapping = nextMapping
 *
 * Returns the mapping number as the function value.
 */
static int 
gtermNextMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	Widget w = wp->w;
	char buf[SZ_NUMBER];
	int mapping;

	mapping = GtNextMapping (w);
	sprintf (buf, "%d", mapping);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);

	return (TCL_OK);
}


/* freeMapping -- Free or delete a mapping.
 *
 * Usage:	freeMapping mapping
 *
 * The given mapping descriptor is freed and any resources used by the mapping
 * are freed.
 */
static int
gtermFreeMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, erase;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	GtDisableMapping (w, mapping, erase=1);
	GtFreeMapping (w, mapping);

	return (TCL_OK);
}


/* lowerMapping -- Lower a mapping, i.e., change its stacking order so that
 * it is drawn below other mappings.
 *
 * Usage:	lowerMapping mapping [reference]
 *
 * If a reference mapping is named the stacking order of the target mapping
 * will be modified to make it appear just beneath the reference mapping.
 * If no reference mapping is given the mapping will be moved to the bottom
 * of the mapping stacking order, making it be drawn below all other mappings.
 */
static int
gtermLowerMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, reference;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	reference = (argc > 2) ? atoi(argv[2]) : 0;
	GtLowerMapping (w, mapping, reference);

	return (TCL_OK);
}


/* raiseMapping -- Raise a mapping, i.e., change its stacking order so that
 * it is drawn above other mappings.
 *
 * Usage:	raiseMapping mapping [reference]
 *
 * If a reference mapping is named the stacking order of the target mapping
 * will be modified to make it appear just above the reference mapping.
 * If no reference mapping is given the mapping will be moved to the top
 * of the mapping stacking order, making it be drawn above all other mappings.
 */
static int
gtermRaiseMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, reference;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	reference = (argc > 2) ? atoi(argv[2]) : 0;
	GtRaiseMapping (w, mapping, reference);

	return (TCL_OK);
}


/* enableMapping -- Reenable a mapping.
 *
 * Usage:	enableMapping mapping [refresh]
 *
 * The given mapping is enabled and optionally refreshed.  This is a no-op
 * if the mapping is already enabled.
 */
static int
gtermEnableMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, refresh;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	refresh = (argc > 2) ? (strcmp(argv[2],"refresh") == 0) : False;

	if (GtEnableMapping (w, mapping, refresh) == ERR)
	    return (TCL_ERROR);

	return (TCL_OK);
}


/* disableMapping -- Disable a mapping.
 *
 * Usage:	disableMapping mapping [erase]
 *
 * The given mapping is disabled and optionally erased.  This is a no-op
 * if the mapping is not enabled.
 */
static int
gtermDisableMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, erase;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	erase = (argc > 2) ? (strcmp(argv[2],"erase") == 0) : False;

	if (GtDisableMapping (w, mapping, erase) == ERR)
	    return (TCL_ERROR);

	return (TCL_OK);
}


/* activeMapping -- Test whether a given mapping is active (enabled).
 *
 * Usage:	active = activeMapping mapping
 *
 * Returns True if the mapping is defined and enabled, False otherwise.
 */
static int 
gtermActiveMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	char buf[SZ_NUMBER];
	int mapping, active;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	active = GtActiveMapping (w, mapping);

	Tcl_SetResult (obm->tcl, active ? TRUESTR : FALSESTR, TCL_STATIC);
	return (TCL_OK);
}


/* refreshMapping -- Refresh a mapping.
 *
 * Usage:	refreshMapping mapping
 *
 * The given mapping is unconditionally refreshed, i.e., the destination
 * rect is repainted.
 */
static int
gtermRefreshMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	GtRefreshMapping (w, mapping);

	return (TCL_OK);
}


/* copyRaster -- Copy a region from one raster to another.
 *
 * Usage:	copyRaster rop
 *		    src st sx sy snx sny
 *		    dst dt dx dy dnx dny
 *
 * The specified region of the source raster is scaled as necessary and
 * written to the specified region of the destination raster.  This is
 * equivalent to defining a mapping between the source and destination,
 * refreshing the mapping, and then freeing the mapping.  Refer to setMapping
 * for a description of the arguments.
 *
 * Copyraster may be used to manually display rasters (without setting up a
 * mapping) by copying rasters to raster zero, the display window.  If the
 * source raster is a server raster and the mapping is one-to-one this can
 * be done very quickly.  For the fastest possible results the transient
 * flag should be set in the rasterop (ROP) to prevent saving of the displayed
 * data in an off-screen pixmap (this may prevent the window from being
 * refreshed properly in response to window system expose events).
 */
static int 
gtermCopyRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int rop, src, st, dst, dt, status;
	int sx, sy, snx, sny, dx, dy, dnx, dny;

	if (argc != 14)
	    return (TCL_ERROR);

	rop = atoi (argv[1]);
	get_mapping (&argv[2],
	    &src, &st, &sx, &sy, &snx, &sny,
	    &dst, &dt, &dx, &dy, &dnx, &dny);

	status = GtCopyRaster (w, rop,
	    src, st, sx, sy, snx, sny,
	    dst, dt, dx, dy, dnx, dny);

	return (status == ERR ? TCL_ERROR : TCL_OK);
}


/* setMapping -- Set or modify a mapping.
 *
 * Usage:	setMapping mapping rop
 *		    src st sx sy snx sny
 *		    dst dt dx dy dnx dny
 *
 * setMapping defines a new mapping function, or modifies an old one.  If a new
 * mapping is defined it is merely enabled, and no refreshing of the screen
 * takes place until either some mapped source data is modified or the mapping
 * is explicitly refreshed.  If an existing mapping is modified the old and new
 * mappings are examined and only those portions of the destination rect for
 * which the mapping changed are updated.  This permits minor changes to a
 * mapping (e.g.  moving an edge) without having to redraw the entire region.
 * Regions of the destination drawable which were previously covered by the
 * mapping but which were exposed by modifying the mapping are redrawn, in
 * effect erasing the mapping.
 *
 * SetMapping optimizes the mapping operation where possible.  In particular,
 * if the mapping is one-to-one and both the source and destination rasters
 * are server rasters, a very fast memory copy in the server is used.  Other
 * cases, for example a dezoom involving antialising, can be expensive.
 * 
 * The mapping number is arbitrary.  Mapping numbers may be preallocated by
 * some client defined logic, or dynamically allocated with nextMapping.
 * Most applications will want to set the rasterop (ROP) argument to zero,
 * which causes the mapping to copy the source to the destination.  For further
 * information on the significance of the bits in the rasterop refer to the
 * gterm widget documentation.
 * 
 * The source and destination rects are specifed with six fields each: the
 * raster number, coordinate type, X,Y coordinates of the rect, and the
 * X,Y size of the rect in pixels.  The coordinate type may be either "Pixel"
 * (raster pixel coordinates) or NDC.  NDC coordinates are normalized device
 * coordinates in the range 0.0 to 1.0 in either axis.  The origin is in the
 * upper left corner for Pixel coordinates, and in the lower left corner for
 * NDC coordinates.
 * 
 * The source and destination rects need not be the same size.  The source
 * will be scaled as necessary to fill the destination.  Scaling options,
 * e.g. the antialiasing technique used for a dezoom, are controlled by the
 * rasterop.  If the DNX or DNY field is negative the mapping will flip the
 * image about the corresponding axis.
 *
 * The source and destination rects can be any two rasters, or even the
 * same raster.  A special case is raster zero, the display window.  Mapping
 * a source raster to dst=0 is equivalent to displaying the raster.  By
 * default data mapped to raster zero will also be saved in an off-screen
 * pixmap in the server and used to autmatically refresh the window in
 * response to window system expose events.  This feature may be disabled
 * by setting the transient flag in the rasterop.
 *
 * A raster can have multiple source or destination mappings defined on it.
 * A region of a raster may be the source for more than one mapping in which
 * case all mappings are updated when the source region is modified.  Multiple
 * sources may be mapped to (different regions) of a destination raster to
 * implement special effects such as split screen or picture insets.  Mappings
 * may be chained to set up graphics pipelines, where the destination of one
 * mapping is the source of the next.  Care must be taken to avoid feedback or
 * infinite loops can result.
 */
static int 
gtermSetMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, rop, src, st, dst, dt, status;
	int sx, sy, snx, sny, dx, dy, dnx, dny;

	if (argc != 15)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	rop = atoi (argv[2]);
	get_mapping (&argv[3],
	    &src, &st, &sx, &sy, &snx, &sny,
	    &dst, &dt, &dx, &dy, &dnx, &dny);

	status = GtSetMapping (w, mapping, rop,
	    src, st, sx, sy, snx, sny,
	    dst, dt, dx, dy, dnx, dny);

	return (status == ERR ? TCL_ERROR : TCL_OK);
}


/* getMapping -- Get a mapping.
 *
 * Usage:	getMapping mapping rop
 *		    src st sx sy snx sny
 *		    dst dt dx dy dnx dny
 *
 * The given mapping is returned in the output variables.  All arguments
 * except MAPPING are output variables.  It is an error if the mapping is
 * not defined, but the mapping need not be enabled.
 */
static int 
gtermGetMapping (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, rop, src, st, dst, dt, status;
	int sx, sy, snx, sny, dx, dy, dnx, dny;
	char buf[SZ_NUMBER];

	if (argc != 15)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	status = GtGetMapping (w, mapping, &rop,
	    &src, &st, &sx, &sy, &snx, &sny,
	    &dst, &dt, &dx, &dy, &dnx, &dny);
	if (status == ERR)
	    return (TCL_ERROR);

	sprintf (buf, "%d", rop);
	Tcl_SetVar (obm->tcl, argv[2], buf, 0);
	put_mapping (obm->tcl, &argv[3],
	    src, st, sx, sy, snx, sny,
	    dst, dt, dx, dy, dnx, dny);

	return (TCL_OK);
}


/* SelectRaster -- Given the raw screen coordinates SX,SY (or coords in
 * any destination raster), determine the mapping and source raster which are
 * mapped to that pixel and return the raster and mapping numbers and the
 * coordinates of the same pixel in the source raster.
 *
 * Usage:	raster = selectRaster dras dt dx dy rt rx ry [map]
 *
 * where	dras		display raster
 *		dt,rt		coordinate type - "pixel" or "ndc"
 *		dx,dy		display raster coordinates (input)
 *		rx,ry		source raster coordinates (output)
 *		map		mapping selected (output)
 *
 * Note that the coordinates returned by selectRaster are measured (taking
 * a line as an example) from zero at the left edge of the first pixel, to 
 * "width" at the right edge of the last pixel.  This means that the floating
 * point coordinates of the center of raster pixel N will be N + 0.5.  For
 * example, if we input screen coordinates (dras=0), x=117, and no mapping
 * is in effect, the floating point raster coordinates returned will be 117.5.
 * The difference occurs because the input coordinate is a pixel number 
 * (integer) while the output coordinate is a floating point coordinate
 * measuring the continuously variable location a pixel.  int(x) will convert
 * this coordinate to a raster pixel number.
 */
static int 
gtermSelectRaster (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	int raster, dras, dt, dx, dy, rt, rx, ry, mp;
	char *xout, *yout, *mpout;
	float fx, fy;
	char buf[64];

	if (argc < 8)
	    return (TCL_ERROR);

	/* Get arguments. */
	dras = atoi (argv[1]);
	if ((dt = coordType (argv[2])) < 0)
	    dt = GtPixel;
	dx = (int) atof (argv[3]);
	dy = (int) atof (argv[4]);
	if (dt == GtNDC) {
	    dx *= MAXNDC;
	    dy *= MAXNDC;
	}
	if ((rt = coordType (argv[5])) < 0)
	    rt = GtPixel;
	xout = argv[6];
	yout = argv[7];
	mpout = (argc > 8) ? argv[8] : NULL;

	raster = GtSelectRaster (wp->w, dras, dt, dx, dy, GtNDC, &rx, &ry, &mp);

	if (rt == GtNDC) {
	    /* Return coords scaled 0.0 - 1.0.  */
	    fx = (float)rx / MAXNDC;
	    fy = (float)ry / MAXNDC;
	} else {
	    /* Return raster pixel coordinates. */
	    ndcToPixel (wp->w, raster, rx, ry, &fx, &fy);
	}

	sprintf (buf, "%g", fx);
	Tcl_SetVar (obm->tcl, xout, buf, 0);
	sprintf (buf, "%g", fy);
	Tcl_SetVar (obm->tcl, yout, buf, 0);
	if (mpout) {
	    sprintf (buf, "%d", mp);
	    Tcl_SetVar (obm->tcl, mpout, buf, 0);
	}

	sprintf (buf, "%d", raster);
	Tcl_SetResult (obm->tcl, buf, TCL_VOLATILE);
	return (TCL_OK);
}


/* UnmapPixel -- unmapPixel is a simplified, less general version of
 * selectRaster which will automatically follow graphics pipelines back to
 * the original mapped raster.  If desired the raster pixel value can be
 * returned as well as the raster number and raster pixel coordinates
 * corresponding to a screen (raster 0) pixel.
 *
 * Usage:	unmapPixel sx sy raster rx ry [rz]
 *
 * where	sx,sy		"screen" (raster 0) coordinates
 *		raster		original mapped raster (output)
 *		rx,ry		source raster coordinates (output)
 *		rz		source raster pixel value (output)
 *
 * By following graphics pipelines back to the original source raster we mean
 * the following.  If raster A is mapped to raster B which is mapped to C (the
 * screen), given a screen coordinate in the mapped region unmapPixel will
 * return the raster number and coordinates for raster A.
 */
static int 
gtermUnmapPixel (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	int dst, dx, dy, src, sx, sy, mapping;
	char *raster_out, *x_out, *y_out, *z_out;
	char buf[SZ_NUMBER];
	float fx, fy;

	if (argc < 6)
	    return (TCL_ERROR);

	/* Get arguments. */
	dx = (int) atof (argv[1]);
	dy = (int) atof (argv[2]);
	raster_out = argv[3];
	x_out = argv[4];
	y_out = argv[5];
	z_out = (argc > 6) ? argv[6] : NULL;

	/* Follow the pipeline back to the original mapped raster. */
	fx = dx;  fy = dy;
	src = 0;

	do {
	    src = GtSelectRaster (wp->w, dst=src,
		GtPixel, dx, dy, GtNDC, &sx, &sy, &mapping);
	    if (src != dst) {
		ndcToPixel (wp->w, src, sx, sy, &fx, &fy);
		dx = (int) fx;
		dy = (int) fy;
	    }
	} while (dst != src);

	sprintf (buf, "%d", src);
	Tcl_SetVar (obm->tcl, raster_out, buf, 0);
	sprintf (buf, "%g", fx);
	Tcl_SetVar (obm->tcl, x_out, buf, 0);
	sprintf (buf, "%g", fy);
	Tcl_SetVar (obm->tcl, y_out, buf, 0);

	if (z_out) {
	    uchar data[1];
	    GtReadPixels (wp->w, src, data, 8, dx, dy, 1, 1);
	    sprintf (buf, "%d", data[0]);
	    Tcl_SetVar (obm->tcl, z_out, buf, 0);
	}

	return (TCL_OK);
}


/* flip -- Edit a mapping to flip the mapped subimage in X and/or Y.
 *
 * Usage:	flip mapping axis [axis]
 *
 * where axis is "x" or "y".  This is a convenience routine for changing only
 * the flip portion of a mapping.
 */
static int 
gtermFlip (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	WidgetPrivate wp = &obj->widget;
	ObmContext obm = wp->obm;
	register Widget w = wp->w;
	int mapping, rop, flipX, flipY, i;
	int src, st, sx, sy, snx, sny;
	int dst, dt, dx, dy, dnx, dny;

	if (argc < 2)
	    return (TCL_ERROR);

	mapping = atoi (argv[1]);
	flipX = flipY = 0;
	for (i=2;  i < argc;  i++) {
	    if (argv[i][0] == 'x')
		flipX = !flipX;
	    else if (argv[i][0] == 'y')
		flipY = !flipY;
	}

	if (flipX || flipY) {
	    GtGetMapping (w, mapping,
		&rop, &src,&st,&sx,&sy,&snx,&sny, &dst,&dt,&dx,&dy,&dnx,&dny);

	    if (flipX)
		dnx = -dnx;
	    if (flipY)
		dny = -dny;

	    GtSetMapping (w, mapping,
		rop, src,st,sx,sy,snx,sny, dst,dt,dx,dy,dnx,dny);
	}

	return (TCL_OK);
}


/* gtermMarkerInit -- Initialize the Marker subsystem for a Gterm widget.
 * This destroys all markers and initializes the marker subsystem.
 *
 * Usage:	markerInit
 */
static int 
gtermMarkerInit (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;

	GtMarkerInit (wp->w);
	return (TCL_OK);
}


/* gtermCreateMarker -- Create a new marker.
 *
 * Usage:	createMarker name attribute-list
 *   e.g.	createMarker name {attribute value [attribute value ...]}
 *     or	createMarker name attribute value [attribute value ...]
 *
 * Any marker attribute may be assigned a value when the marker is created.
 * Refer to <ObmW/Gterm.h> for a list of marker attribute names.  Often the
 * the attributes "type" and "createMode" need to be specified at marker
 * create time.
 *
 *	type		The marker type: text, rectangle, circle, etc.
 *
 *	createMode	A marker should be created with createMode=interactive
 *			if the user is expected to interactively drag out
 *			the marker using the pointer and either the default
 *			or an application specified translation table.  A
 *			marker can also be created interactively using only
 *			the m_create (marker create) action, however m_create
 *			does not allow the marker attributes to be set.
 *
 * There are any number of ways to use a GUI to create a marker under the
 * Object Manager, but an example might be using a translation to call a GUI
 * procedure which issues the createMarker call.  For example a pointer down
 * event could translate as "call(newMarker,$name,$x,$y) m_create()" where
 * newMarker is a GUI marker creation procedure which sends a createMarker
 * message to the Gterm widget.  The GUI procedure could set the marker
 * attributes as desired, possibly using additional GUI components to define
 * the marker attributes.  The m_create action will notice that a
 * createMarker has been executed and will merely activate the marker and
 * give it the pointer focus (i.e. install the marker translations).  The
 * user will then use the pointer or keyboard to drag out the marker.
 *
 * If the marker is created noninteractive the application must set the marker
 * position and size using marker attributes.  If the marker is sensitive
 * the user can then use the marker's translations to interactively modify
 * the marker (resize it, move it, etc.).  All markers which are visible and
 * sensitive and which have the necessary translations can be interactively
 * modified by the user; the reason for creating a marker in interactive mode
 * is to allow the initial marker position and size to be specified
 * interactively *when* the marker is created, instead of afterwards.
 *
 * Any number of attributes may be given when the marker is created.  Most
 * marker attributes can also be modified after a marker has been created
 * by sending setAttribute messages to the marker.
 */
static int 
gtermCreateMarker (msg, tcl, argc, argv)
MsgContext msg;
Tcl_Interp *tcl;
int argc;
char **argv;
{
	GtermObject obj = (GtermObject) msg->object[msg->level];
	register WidgetPrivate wp = &obj->widget;
	register ObmContext obm = wp->obm;
	Arg args[MAX_ARGS];
	int nargs, i;
	char **items;
	int nitems;
	char *name;

	if (argc < 2)
	    return (TCL_ERROR);

	name = argv[1];

	if (argc == 3) {
	    /* Attribute list passed as a list argument. */
	    if (Tcl_SplitList (tcl, argv[2], &nitems, &items) != TCL_OK)
		return (TCL_ERROR);
	} else if (argc > 3) {
	    /* Attribute list passed as separate arguments. */
	    nitems = argc - 2;
	    items = (char **) XtMalloc (nitems * sizeof(char *));
	    if (items == NULL)
		return (TCL_ERROR);
	    for (i=0;  i < nitems;  i++)
		items[i] = argv[i+2];
	} else
	    return (TCL_ERROR);

	if (argc > 2)
	    for (i=nargs=0;  i < nitems && nargs < MAX_ARGS;  i += 2) {
		XtSetArg (args[nargs], items[i], items[i+1]);
		nargs++;
	    }

	obmNewObject (obm, name, "Marker", obj->core.name, args, nargs);

	if (argc > 2)
	    free ((char *) items);

	return (TCL_OK);
}


/*
 * Gterm widget utility procedures.
 * ------------------------------------------
 */


/* coordType -- Convert a coordinate type string "pixel" or "ndc" to an
 * integer code.
 */
coordType (name)
char *name;
{
	if (strcmp (name, "pixel") == 0 ||
	    strcmp (name, "Pixel") == 0 ||
	    strcmp (name, "PIXEL") == 0) {

	    return (GtPixel);

	} else if (
	    strcmp (name, "ndc") == 0 ||
	    strcmp (name, "NDC") == 0) {

	    return (GtNDC);

	} else
	    return (-1);
}


/* dataLevelType -- Convert a data level type string to an integer code.
 */
dataLevelType (name)
char *name;
{
	if (strcmp (name, "set") == 0)
	    return (GtSet);
	else if (strcmp (name, "clear") == 0)
	    return (GtClear);
	else if (strcmp (name, "invert") == 0)
	    return (GtInvert);
	else
	    return (-1);
}


/* lineStyle -- Convert a line style string to an integer code.
 */
lineStyle (name)
char *name;
{
	if (strcmp (name, "solid") == 0)
	    return (GtSolid);
	else if (strcmp (name, "dashed") == 0)
	    return (GtDashed);
	else if (strcmp (name, "dotted") == 0)
	    return (GtDotted);
	else if (strcmp (name, "dashDot") == 0)
	    return (GtDashDot);
	else if (strcmp (name, "dash3Dot") == 0)
	    return (GtDash3Dot);
	else
	    return (-1);
}


/* fillType -- Convert a fill type string to an integer code.
 */
fillType (name)
char *name;
{
	if (strcmp (name, "solid") == 0)
	    return (GtSolid);
	else if (strcmp (name, "outline") == 0)
	    return (GtOutline);
	else
	    return (-1);
}


/* colorToIndex -- Convert a color name or number to a gterm widget color
 * index.
 */
colorToIndex (name)
char *name;
{
	if (isdigit (*name))
	    return (atoi (name));
	else if (strcmp (name, "background") == 0)
	    return (0);
	else if (strcmp (name, "foreground") == 0)
	    return (1);
	else if (strcmp (name, "black") == 0)
	    return (0);
	else if (strcmp (name, "white") == 0)
	    return (1);
	else if (strcmp (name, "red") == 0)
	    return (2);
	else if (strcmp (name, "green") == 0)
	    return (3);
	else if (strcmp (name, "blue") == 0)
	    return (4);
	else if (strcmp (name, "cyan") == 0)
	    return (5);
	else if (strcmp (name, "magenta") == 0)
	    return (6);
	else if (strcmp (name, "yellow") == 0)
	    return (7);
	else if (strcmp (name, "user1") == 0)
	    return (8);
	else if (strcmp (name, "user2") == 0)
	    return (9);
	else
	    return (-1);
}


/* ncdToPixel -- Convert NDC (integer) to raster pixel (floating) coordinates.
 */
ndcToPixel (w, raster, nx, ny, rx, ry)
Widget w;
int raster;
int nx, ny;
float *rx, *ry;
{
	int rtype, width, height, depth;
	int x2, y2;
	
	GtQueryRaster (w, raster, &rtype, &width, &height, &depth);
	x2 = width;
	y2 = height;

	*rx = (float)(         nx) / MAXNDC * x2;
	*ry = (float)(MAXNDC - ny) / MAXNDC * y2;   /* NDC is flipped in Y */
}

static XPoint *
get_points (points, npoints)
char *points;
int *npoints;
{
	register int i;
	register char *ip;
	register XPoint *pv;
	char *ipp, *ip_save;
	int maxpts, npts;

	maxpts = MAX_POLYPTS;
	if ((pv = (XPoint *) XtMalloc (maxpts * sizeof(XPoint))) == NULL)
	    return (NULL);

	/* Get the points array. */
	for (npts=0, ip=points;  *ip;  ) {
	    while (isspace(*ip) || *ip == '{')
		ip++;

	    ip_save = ip;
	    pv[npts].x = (short) strtod (ip, &ipp);  ip = ipp;
	    pv[npts].y = (short) strtod (ip, &ipp);  ip = ipp;
	    if (ip == ip_save) {
		XtFree ((char *) pv);
		return (NULL);
	    }

	    while (isspace(*ip) || *ip == '}')
		ip++;

	    if (++npts >= maxpts) {
		maxpts *= 2;
		if ((pv = (XPoint *) XtRealloc ((char *)pv,
			maxpts * sizeof(XPoint))) == NULL)
		    return (NULL);
	    }
	}

	*npoints = npts;
	return (pv);
}


/* get_mapping -- Read a mapping from an argument list into local variables.
 */
static void
get_mapping (argv, src, st, sx,sy,snx,sny, dst, dt, dx,dy,dnx,dny)
register char **argv;		/* mapping values */
int *src, *st;
int *sx, *sy, *snx, *sny;
int *dst, *dt;
int *dx, *dy, *dnx, *dny;
{
	register int ndc;
	register double v;

	*src = atoi (argv[0]);
	*st  = (strcmp(argv[1],"pixel")==0 || strcmp(argv[1],"Pixel")==0) ?
		    GtPixel : GtNDC;
	ndc = (*st == GtNDC);

	v = atof(argv[2]);
	    *sx  = ndc ? (v * MAXNDC) : v;
	v = atof(argv[3]);
	    *sy  = ndc ? ((1.0 - v) / 1.0 * MAXNDC) : v;
	v = atof(argv[4]);
	    *snx  = ndc ? (v * MAXNDC) : v;
	v = atof(argv[5]);
	    *sny  = ndc ? (v * MAXNDC) : v;

	*dst = atoi (argv[6]);
	*dt  = (strcmp(argv[7],"pixel")==0 || strcmp(argv[7],"Pixel")==0) ?
		    GtPixel : GtNDC;
	ndc = (*dt == GtNDC);

	v = atof(argv[8]);
	    *dx  = ndc ? (v * MAXNDC) : v;
	v = atof(argv[9]);
	    *dy  = ndc ? ((1.0 - v) / 1.0 * MAXNDC) : v;
	v = atof(argv[10]);
	    *dnx = ndc ? (abs(v) * MAXNDC) : abs(v);
	    if (v < 0)
		*dnx = -(*dnx);
	v = atof(argv[11]);
	    *dny = ndc ? (abs(v) * MAXNDC) : abs(v);
	    if (v < 0)
		*dny = -(*dny);
}


/* put_mapping -- Output a mapping from local variables to a list of output
 * variables.
 */
static void
put_mapping (tcl, argv, src, st, sx,sy,snx,sny, dst, dt, dx,dy,dnx,dny)
register Tcl_Interp *tcl;
register char **argv;		/* mapping variables */
int src, st;
int sx, sy, snx, sny;
int dst, dt;
int dx, dy, dnx, dny;
{
	register int ndc;
	char buf[SZ_NUMBER];

	sprintf (buf, "%d", src);
	Tcl_SetVar (tcl, argv[0], buf, 0);
	Tcl_SetVar (tcl, argv[1], st == GtPixel ? "Pixel" : "NDC", 0);
	ndc = (st == GtNDC);

	sprintf (buf, "%g", ndc ? (float)sx / MAXNDC : (float)sx);
	    Tcl_SetVar (tcl, argv[2], buf, 0);
	sprintf (buf, "%g",
	    ndc ? (1.0 - ((float)sy / MAXNDC)) : (float)sy);
	    Tcl_SetVar (tcl, argv[3], buf, 0);
	sprintf (buf, "%g", ndc ? (float)snx / MAXNDC : (float)snx);
	    Tcl_SetVar (tcl, argv[4], buf, 0);
	sprintf (buf, "%g", ndc ? (float)sny / MAXNDC : (float)sny);
	    Tcl_SetVar (tcl, argv[5], buf, 0);

	sprintf (buf, "%d", src);
	Tcl_SetVar (tcl, argv[6], buf, 0);
	Tcl_SetVar (tcl, argv[7], st == GtPixel ? "Pixel" : "NDC", 0);
	ndc = (st == GtNDC);

	sprintf (buf, "%g", ndc ? (float)dx / MAXNDC : (float)dx);
	    Tcl_SetVar (tcl, argv[8], buf, 0);
	sprintf (buf, "%g",
	    ndc ? (1.0 - ((float)dy / MAXNDC)) : (float)dy);
	    Tcl_SetVar (tcl, argv[9], buf, 0);
	sprintf (buf, "%g", ndc ? (float)dnx / MAXNDC : (float)dnx);
	    Tcl_SetVar (tcl, argv[10], buf, 0);
	sprintf (buf, "%g", ndc ? (float)dny / MAXNDC : (float)dny);
	    Tcl_SetVar (tcl, argv[11], buf, 0);
}
