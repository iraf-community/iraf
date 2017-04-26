/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmRdFToI.c:                                                                *
*                                                                             *
*  XPM library                                                                *
*  Parse an XPM file and create the image and possibly its mask               *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmReadFileToImage(display, filename,
		   image_return, shapeimage_return, attributes)
    Display *display;
    char *filename;
    XImage **image_return;
    XImage **shapeimage_return;
    XpmAttributes *attributes;
{
    int ErrorStatus;
    XpmImage image;

    ErrorStatus = XpmReadFileToXpmImage(filename, &image, attributes, NULL);

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = XpmCreateImageFromXpmImage(display, &image,
						 image_return,
						 shapeimage_return,
						 attributes);
    XpmFreeXpmImage(&image);

    return (ErrorStatus);
}

int
XpmReadFileToXpmImage(filename, image, attributes, infos)
    char *filename;
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

    if ((ErrorStatus = xpmReadFile(filename, &mdata)) != XpmSuccess)
	return (ErrorStatus);

    ErrorStatus = xpmParseData(&mdata, image, attributes, infos);
    xpmDataClose(&mdata);

    if (ErrorStatus != XpmSuccess)
	XpmFreeXpmInfos(infos);

    return (ErrorStatus);
}
