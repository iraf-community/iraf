/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrIFrBuf.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Parse an Xpm buffer (file in memory) and create the image and possibly its *
*  mask                                                                       *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmCreateImageFromBuffer(display, buffer, image_return,
			 shapeimage_return, attributes)
    Display *display;
    char *buffer;
    XImage **image_return;
    XImage **shapeimage_return;
    XpmAttributes *attributes;
{
    XpmImage image;
    int ErrorStatus;

    /*
     * create an XpmImage then the related XImages
     */
    ErrorStatus =
	XpmCreateXpmImageFromBuffer(buffer, &image, attributes, NULL);

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
XpmCreateXpmImageFromBuffer(buffer, image, attributes, infos)
    char *buffer;
    XpmImage *image;
    XpmAttributes *attributes;
    XpmInfos *infos;
{
    xpmData mdata;
    int ErrorStatus;

    /*
     * init returned values
     */
    xpmInitAttributes(attributes);
    xpmInitXpmImage(image);
    xpmInitXpmInfos(infos);

    xpmOpenBuffer(buffer, &mdata);
    ErrorStatus = xpmParseData(&mdata, image, attributes, infos);
    xpmDataClose(&mdata);

    if (ErrorStatus != XpmSuccess)
	XpmFreeXpmInfos(infos);

    return (ErrorStatus);
}
