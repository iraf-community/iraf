/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrPFrData.c:                                                             *
*                                                                             *
*  XPM library                                                                *
*  Parse an Xpm array and create the pixmap and possibly its mask             *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmCreatePixmapFromData(display, d, data, pixmap_return,
			shapemask_return, attributes)
    Display *display;
    Drawable d;
    char **data;
    Pixmap *pixmap_return;
    Pixmap *shapemask_return;
    XpmAttributes *attributes;
{
    XpmImage image;
    int ErrorStatus;

    /*
     * create an XpmImage
     */
    ErrorStatus = XpmCreateXpmImageFromData(data, &image, attributes);
    if (ErrorStatus != XpmSuccess)
	return (ErrorStatus);

    /*
     * then create the pixmaps from it
     */
    ErrorStatus = XpmCreatePixmapFromXpmImage(display, d, &image,
					      pixmap_return, shapemask_return,
					      attributes);
    if (ErrorStatus != XpmSuccess)
	XpmFreeAttributes(attributes);

    XpmFreeXpmImage(&image);

    return (ErrorStatus);
}
