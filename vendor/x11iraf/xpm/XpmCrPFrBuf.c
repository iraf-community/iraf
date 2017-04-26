/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrPFrBuf.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Parse an Xpm buffer and create the pixmap and possibly its mask            *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmCreatePixmapFromBuffer(display, d, buffer, pixmap_return,
			  shapemask_return, attributes)
    Display *display;
    Drawable d;
    char *buffer;
    Pixmap *pixmap_return;
    Pixmap *shapemask_return;
    XpmAttributes *attributes;
{
    XpmImage image;
    int ErrorStatus;

    /*
     * create an XpmImage
     */
    ErrorStatus =
	XpmCreateXpmImageFromBuffer(buffer, &image, attributes, NULL);

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
