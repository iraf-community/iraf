/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrIFrData.c:                                                             *
*                                                                             *
*  XPM library                                                                *
*  Parse an Xpm array and create the image and possibly its mask              *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmCreateImageFromData(display, data, image_return,
		       shapeimage_return, attributes)
    Display *display;
    char **data;
    XImage **image_return;
    XImage **shapeimage_return;
    XpmAttributes *attributes;
{
    int ErrorStatus;
    XpmImage image;

    /*
     * create an XpmImage then the related XImages
     */
    ErrorStatus = XpmCreateXpmImageFromData(data, &image, attributes);

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = XpmCreateImageFromXpmImage(display, &image,
						 image_return,
						 shapeimage_return,
						 attributes);
    if (ErrorStatus < 0 && attributes)
	XpmFreeAttributes(attributes);

    XpmFreeXpmImage(&image);

    return (ErrorStatus);
}

int
XpmCreateXpmImageFromData(data, image, attributes)
    char **data;
    XpmImage *image;
    XpmAttributes *attributes;
{
    xpmData mdata;
    int ErrorStatus;

    /*
     * init returned values
     */
    xpmInitAttributes(attributes);
    xpmInitXpmImage(image);

    xpmOpenArray(data, &mdata);
    ErrorStatus = xpmParseData(&mdata, image, attributes, NULL);
    xpmDataClose(&mdata);

    return (ErrorStatus);
}

