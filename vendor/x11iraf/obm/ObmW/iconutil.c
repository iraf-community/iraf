/*

The converter from String to Icon knows about a few preloaded icons:
FATAL, ERROR, WARNING, QUESTION, INFO, emptysquare, filledsquare,
emptydiamond, filleddiamond and none. Other names are assumed to be
names of files in XPM format.

An Icon is a combination of a Pixmap and a mask. The mask is
constructed from the pixels that are marked `none' (i.e.,
`transparent') in the XPM data. The other pixels end up in the icon's
pixmap. The actual colors depend on many factors, but the XPM format
offers a way of defining `dynamic' colors, i.e., colors that are
chosen at run time instead of by the icon's designer. The designer can
assign symbolic names to colors, such as `foreground' or `shadow'. The
application can then replace the symbolic names with actual colors
when the icon is loaded.

The type converter tries to automate this process. When an icon is
loaded, the function looks for symbolic color names that match
resources of the widget into which the icon isloaded. E.g., if the
icon has a symbolic color `mainColor' and the widget has resource of
the same name, the value of the resource will be used as the actual
color for the icon.

In this way, icons can be created that fit the widget, whatever the
colors of that widget.

Good symbolic names to use are `background' (defined in every widget),
`foreground' (defined e.g., in |XfwfLabel|), `topShadowColor' and
`bottomShadowColor' (defined in |XfwfFrame|).

The implementation is as follows: the pixmap for the icon actually has
to be created twice; once to get the list of symbolic colors and again
with the replacement colors. When the XPM data is not preloaded, it is
read from a file. The data is converted to an icon, converted back,
and then again converted to an icon with new colors.

The converter is passed one extra argument: the widget that the icon
is loaded into. It must be in |args[0].addr|.

The table of colors to override is set to a fixed size of 20. This
should be enough for most applications: how many widgets have more
than 20 color resources?

*/

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include "Converters.h"
#include "done.h"
#include "bitmaps/FATAL.pm"
#include "bitmaps/ERROR.pm"
#include "bitmaps/WARNING.pm"
#include "bitmaps/QUESTION.pm"
#include "bitmaps/INFO.pm"
#include "bitmaps/square0.pm"
#include "bitmaps/square1.pm"
#include "bitmaps/diamond0.pm"
#include "bitmaps/diamond1.pm"
#include "bitmaps/square0s.pm"
#include "bitmaps/square1s.pm"
#include "bitmaps/diamond0s.pm"
#include "bitmaps/diamond1s.pm"
#include "bitmaps/square0m.pm"
#include "bitmaps/square1m.pm"
#include "bitmaps/diamond0m.pm"
#include "bitmaps/diamond1m.pm"

#define MAXCOLORSYM  20



/* Here are the quarks for the built-in icons names. */

static XrmQuark filleddiamondq, emptydiamondq, filledsquareq, emptysquareq,
    QUESTIONq, FATALq, ERRORq, WARNINGq, INFOq, noneq;
static XrmQuark diamond0sq, diamond1sq, square0sq, square1sq;
static XrmQuark diamond0mq, diamond1mq, square0mq, square1mq;

static Boolean initialized = False;



/* This function initializes the quarks. */

static void init_icon_quarks()
{
    filleddiamondq = XrmPermStringToQuark("filleddiamond");
    emptydiamondq = XrmPermStringToQuark("emptydiamond");
    filledsquareq = XrmPermStringToQuark("filledsquare");
    emptysquareq = XrmPermStringToQuark("emptysquare");

    diamond0sq = XrmPermStringToQuark("diamond0s");
    diamond1sq = XrmPermStringToQuark("diamond1s");
    square0sq = XrmPermStringToQuark("square0s");
    square1sq = XrmPermStringToQuark("square1s");

    diamond0mq = XrmPermStringToQuark("diamond0m");
    diamond1mq = XrmPermStringToQuark("diamond1m");
    square0mq = XrmPermStringToQuark("square0m");
    square1mq = XrmPermStringToQuark("square1m");

    QUESTIONq = XrmPermStringToQuark("QUESTION");
    FATALq = XrmPermStringToQuark("FATAL");
    ERRORq = XrmPermStringToQuark("ERROR");
    WARNINGq = XrmPermStringToQuark("WARNING");
    INFOq = XrmPermStringToQuark("INFO");
    noneq = XrmPermStringToQuark("none");

    initialized = True;
}


/* The function |file_to_xpmimage| is called by the type converter
 * |cvtStringToIcon|.  It reads a file in XPM format into an XPM image and
 * prints error messages in case of failure.
 */
static void file_to_xpmimage (dpy, file, image)
    Display *dpy;
    String file;
    XpmImage *image;
{
    Cardinal one = 1;
    int status;

    status = XpmReadFileToXpmImage (file, image, NULL, NULL);

    switch (status) {
    case XpmOpenFailed:
    case XpmFileInvalid:
    case XpmNoMemory:
	XtAppWarningMsg
	    (XtDisplayToApplicationContext(dpy), "cvtStringToIcon",
	     "fileError", "XtToolkitError", "Failed to create icon \"%s\"",
	     (XtPointer) &file, &one);
	break;
    case XpmColorError:
    case XpmColorFailed:
	XtAppWarningMsg
	    (XtDisplayToApplicationContext(dpy), "cvtStringToIcon",
	     "allocColor", "XtToolkitError",
	     "Could not get (all) colors for image \"%s\"",
	     (XtPointer) &file, &one);
	break;
    case XpmSuccess:
	; /* skip */
    }
}


/* The function |data_to_xpmimage| is also called by the type converter
 * |cvtStringToIcon|.  It converts data in XPM format into an XPM image and
 * prints error messages in case of failure.
 */
static void data_to_xpmimage (dpy, data, image)
    Display *dpy;
    String *data;
    XpmImage *image;
{
    int status;

    status = XpmCreateXpmImageFromData (data, image, NULL);

    switch (status) {
    case XpmOpenFailed:
    case XpmFileInvalid:
    case XpmNoMemory:
	XtAppWarningMsg
	    (XtDisplayToApplicationContext(dpy), "cvtStringToIcon",
	     "fileError", "XtToolkitError", "Failed to create an icon",
	     NULL, NULL);
	break;
    case XpmColorError:
    case XpmColorFailed:
	XtAppWarningMsg
	    (XtDisplayToApplicationContext(dpy), "cvtStringToIcon",
	     "allocColor", "XtToolkitError",
	     "Could not get (all) colors for some icon", NULL, NULL);
	break;
    case XpmSuccess:
	; /* skip */
    }
}


/* The function |build_colortable| is also called from
|cvtStringToIcon|. It looks through all the resources for resources that
specify a color (|Pixel|). All such resources and their values are
entered in the table.

To get at the resource value, the |resource_offset| (an unsigned int)
must be added to the base address of the widget. The widget pointer is
first converted to an unsigned long, tehn the offset is added to it and
the result is converted back to a pointer, in this case a pointer to a
|Pixel|. */

static void build_colortable(self, table, size, n)
    Widget self;
    XpmColorSymbol *table;
    Cardinal size;
    Cardinal *n;
{
    Cardinal nres, i;
    XtResourceList res;

    *n = 0;
    XtGetResourceList(XtClass(self), &res, &nres);
    for (i = 0; i < nres; i++)
	if (strcmp(res[i].resource_type, XtRPixel) == 0 && *n < size) {
	    table[*n].name = res[i].resource_name;
	    table[*n].value = NULL;
	    table[*n].pixel =
		* (Pixel*) ((unsigned long) self + res[i].resource_offset);
	    (*n)++;
	}
    if (res)
        XtFree ((char *)res);					/* MF037 */
}



Boolean cvtStringToIcon(dpy, args, num_args, from, to, converter_data)
    Display *dpy;
    XrmValue *args;
    Cardinal *num_args;
    XrmValue *from;
    XrmValue *to;
    XtPointer *converter_data;
{
    static XpmColorSymbol table[MAXCOLORSYM];
    String *data = NULL, s = (String) from->addr;
    Widget self = (Widget) args[0].addr;
    XpmImage image;
    Icon *view;
    Cardinal n;
    XrmQuark q;

    if (! initialized) init_icon_quarks();

    if (*num_args != 1)
	XtAppErrorMsg
	    (XtDisplayToApplicationContext(dpy),
	     "cvtStringToIcon", "wrongParameters", "XtToolkitError",
	     "String to Icon conversion needs one argument: a widget", 
	     (String*) NULL, (Cardinal*) NULL);

    view = (Icon*) XtCalloc(1, sizeof(*view));
    q = XrmStringToQuark(s);
    /*
     * Convert the input icon or XPM file into an XPM image.
     */
    if (q == filleddiamondq) data_to_xpmimage(dpy, diamond1, &image);
    else if (q == emptydiamondq) data_to_xpmimage(dpy, diamond0, &image);
    else if (q == filledsquareq) data_to_xpmimage(dpy, square1, &image);
    else if (q == emptysquareq) data_to_xpmimage(dpy, square0, &image);
    else if (q == diamond0sq) data_to_xpmimage(dpy, diamond0s, &image);
    else if (q == diamond1sq) data_to_xpmimage(dpy, diamond1s, &image);
    else if (q == square0sq) data_to_xpmimage(dpy, square0s, &image);
    else if (q == square1sq) data_to_xpmimage(dpy, square1s, &image);
    else if (q == diamond0mq) data_to_xpmimage(dpy, diamond0m, &image);
    else if (q == diamond1mq) data_to_xpmimage(dpy, diamond1m, &image);
    else if (q == square0mq) data_to_xpmimage(dpy, square0m, &image);
    else if (q == square1mq) data_to_xpmimage(dpy, square1m, &image);
    else if (q == QUESTIONq) data_to_xpmimage(dpy, QUESTION, &image);
    else if (q == FATALq) data_to_xpmimage(dpy, FATAL, &image);
    else if (q == ERRORq) data_to_xpmimage(dpy, ERROR, &image);
    else if (q == WARNINGq) data_to_xpmimage(dpy, WARNING, &image);
    else if (q == INFOq) data_to_xpmimage(dpy, INFO, &image);
    else if (q == noneq) {XtFree((String)view); done(Icon*, NULL);}
    else file_to_xpmimage(dpy, s, &image);

    /*
     * Convert back to String format
     */
    if (image.width > 0 && image.height > 0)
	XpmCreateDataFromXpmImage(&data, &image, NULL);

    /*
     * Construct color replacement table and create icon.
     */
    if (data) {
	build_colortable(self, table, XtNumber(table), &n);
	view->attributes.colorsymbols = table;
	view->attributes.numsymbols = n;
	view->attributes.valuemask = XpmColorSymbols;
	(void) XpmCreatePixmapFromData(dpy, DefaultRootWindow(dpy), data,
				       &view->pixmap, &view->mask,
				       &view->attributes);
	XtFree((String) data);
	XpmFreeXpmImage (&image);
	done(Icon*, view);

    } else {
	XtFree ((String)view);
	done(Icon*, NULL);
    }
}
