/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmWrFFrP.c:                                                                *
*                                                                             *
*  XPM library                                                                *
*  Write a pixmap and possibly its mask to an XPM file                        *
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
XpmWriteFileFromPixmap(display, filename, pixmap, shapemask, attributes)
    Display *display;
    char *filename;
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
	 * write it out
	 */
	ErrorStatus =
	    XpmWriteFileFromXpmImage(filename, &image, attributes, NULL);
	XpmFreeXpmImage(&image);
    }
    return (ErrorStatus);
}
