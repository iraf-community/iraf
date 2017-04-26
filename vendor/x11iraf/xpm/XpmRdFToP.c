/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmRdFToP.c:                                                                *
*                                                                             *
*  XPM library                                                                *
*  Parse an XPM file and create the pixmap and possibly its mask              *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmReadFileToPixmap(display, d, filename, pixmap_return,
		    shapemask_return, attributes)
    Display *display;
    Drawable d;
    char *filename;
    Pixmap *pixmap_return;
    Pixmap *shapemask_return;
    XpmAttributes *attributes;
{
    int ErrorStatus;
    XpmImage image;

    /*
     * get the XpmImage
     */
    ErrorStatus = XpmReadFileToXpmImage(filename, &image, attributes, NULL);

    if (ErrorStatus < 0)
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
