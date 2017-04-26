/* Copyright(c) 1993 Association of Universities for Research in Astronomy Inc.
 */
/* Copyright 1987, Massachusetts Institute of Technology */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

/*
 * GEOM.C -- Code to get geometry information for a window.  This code was
 * extracted from the xwininfo sources and hacked into the form of a library
 * routine.
 */

#define	SZ_GEOMETRY 64
static char geometry[SZ_GEOMETRY];
static Display *dpy;


/* get_geometry -- Return the absolute size and position of a window.  The
 * geometry specification coresponding to the window size and position is
 * returned as the function value.
 */
char *
get_geometry (display, screen, window, origin)
    Display *display;
    Screen *screen;
    Window window;
    int origin;			/* return only origin-relative coords */
{
    register char *op;
    int screen_number = XScreenNumberOfScreen (screen);
    XWindowAttributes win_attributes;
    XVisualInfo vistemplate, *vinfo;
    XSizeHints hints;

    int dw = DisplayWidth (display, screen_number);
    int dh = DisplayHeight (display, screen_number);
    int showright = 0, showbelow = 0;
    int rx, ry, xright, ybelow;
    Status status;
    Window wmframe;
    long longjunk;
    Window junkwin;
    int junk;

    if (!XGetWindowAttributes (dpy = display, window, &win_attributes))
	return (NULL);
    vistemplate.visualid = XVisualIDFromVisual(win_attributes.visual);
    /*								 	MF036
    vinfo = XGetVisualInfo (dpy, VisualIDMask, &vistemplate, &junk);
    */

    (void) XTranslateCoordinates (dpy, window, win_attributes.root, 
	-win_attributes.border_width,
	-win_attributes.border_width,
	&rx, &ry, &junkwin);

    xright = (dw - rx - win_attributes.border_width * 2 -
	win_attributes.width);
    ybelow = (dh - ry - win_attributes.border_width * 2 -
	win_attributes.height);

    /* compute size in appropriate units */
    status = XGetWMNormalHints (dpy, window, &hints, &longjunk);
    op = geometry;

    if (status && (hints.flags & PResizeInc) &&
	hints.width_inc != 0 && hints.height_inc != 0) {

	if (hints.flags & (PBaseSize|PMinSize)) {
	    if (hints.flags & PBaseSize) {
		win_attributes.width -= hints.base_width;
		win_attributes.height -= hints.base_height;
	    } else {
		/* ICCCM says MinSize is default for BaseSize */
		win_attributes.width -= hints.min_width;
		win_attributes.height -= hints.min_height;
	    }
        }
	sprintf (op, "%dx%d",
	    win_attributes.width / hints.width_inc,
	    win_attributes.height / hints.height_inc);
    } else
	sprintf (op, "%dx%d", win_attributes.width, win_attributes.height);

    while (*op)
	op++;

    if (!(hints.flags&PWinGravity))
	hints.win_gravity = NorthWestGravity;  /* per ICCCM */

    /* Find our window manager frame, if any. */
    wmframe = window;
    while (True) {
	Window root, parent;
	Window *childlist;
	unsigned int ujunk;

	status = XQueryTree (dpy, wmframe, &root, &parent, &childlist, &ujunk);
	if (parent == root || !parent || !status)
	    break;
	wmframe = parent;
	if (status && childlist)
	    XFree ((char *)childlist);
    }

    if (wmframe != window) {
	/* WM reparented, so find edges of the frame.  This only works for
	 * ICCCM-compliant WMs, and then only if the window has corner gravity.
	 * We would need to know the original width of the window to correctly
	 * handle the other gravities.
	 */
	XWindowAttributes frame_attr;
	if (!XGetWindowAttributes (dpy, wmframe, &frame_attr))
	    return (NULL);

	switch (hints.win_gravity) {
	case NorthWestGravity:
	case SouthWestGravity:
	case NorthEastGravity:
	case SouthEastGravity:
	case WestGravity:
	    rx = frame_attr.x;
	}
	switch (hints.win_gravity) {
	case NorthWestGravity:
	case SouthWestGravity:
	case NorthEastGravity:
	case SouthEastGravity:
	case EastGravity:
	    xright = dw - frame_attr.x - frame_attr.width -
		2 * frame_attr.border_width;
	}
	switch (hints.win_gravity) {
	case NorthWestGravity: case SouthWestGravity:
	case NorthEastGravity: case SouthEastGravity:
	case NorthGravity:
	    ry = frame_attr.y;
	}
	switch (hints.win_gravity) {
	case NorthWestGravity: case SouthWestGravity:
	case NorthEastGravity: case SouthEastGravity:
	case SouthGravity:
	    ybelow = dh - frame_attr.y - frame_attr.height -
		2 * frame_attr.border_width;
	}
    }

    /* If edge gravity, offer a corner on that edge (because the application
     * programmer cares about that edge), otherwise offer upper left unless
     * some other corner is close to an edge of the screen.  (For corner
     * gravity, assume gravity was set by XWMGeometry.  For CenterGravity,
     * it doesn't matter.)
     */
    if (hints.win_gravity == EastGravity ||
	    (abs(xright) <= 100 && abs(xright) < abs(rx) &&
	    hints.win_gravity != WestGravity))
	showright = 1;

    if (hints.win_gravity == SouthGravity ||
	    (abs(ybelow) <= 100  &&  abs(ybelow) < abs(ry) &&
	    hints.win_gravity != NorthGravity))
	showbelow = 1;

    if (showright && !origin)
	sprintf (op, "-%d", xright);
    else
	sprintf (op, "+%d", rx);
    while (*op)
	op++;

    if (showbelow && !origin)
	sprintf (op, "-%d", ybelow);
    else
	sprintf (op, "+%d", ry);

    return (geometry);
}
