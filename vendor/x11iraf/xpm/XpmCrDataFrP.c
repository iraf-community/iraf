/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrDataFP.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Scan a pixmap and possibly its mask and create an XPM array                *
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
XpmCreateDataFromPixmap(display, data_return, pixmap, shapemask, attributes)
    Display *display;
    char ***data_return;
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
	 * create data from the XpmImage
	 */
	ErrorStatus =
	    XpmCreateDataFromXpmImage(data_return, &image, attributes);
	XpmFreeXpmImage(&image);
    }
    return (ErrorStatus);
}
