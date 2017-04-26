/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrBufFrP.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Scan a pixmap and possibly its mask and create an XPM buffer               *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"
#ifdef VMS
#include "sys$library:string.h"
#else
#if defined(SYSV) || defined(SVR4)
#include <string.h>
#else
#include <strings.h>
#endif
#endif

int
XpmCreateBufferFromPixmap(display, buffer_return, pixmap, shapemask,
			  attributes)
    Display *display;
    char **buffer_return;
    Pixmap pixmap;
    Pixmap shapemask;
    XpmAttributes *attributes;
{
    int ErrorStatus;
    XpmImage image;

    /*
     * get the XpmImage
     */
    ErrorStatus = XpmCreateXpmImageFromPixmap(display, pixmap, shapemask,
					      &image, attributes);
    if (ErrorStatus == XpmSuccess) {
	/*
	 * create the buffer from the XpmImage
	 */
	ErrorStatus = XpmCreateBufferFromXpmImage(buffer_return, &image,
						  attributes, NULL);
	XpmFreeXpmImage(&image);
    }
    return (ErrorStatus);
}
