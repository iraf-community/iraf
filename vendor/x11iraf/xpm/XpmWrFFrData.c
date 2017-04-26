/* Copyright 1990,91 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmWrFFrData.c:                                                             *
*                                                                             *
*  XPM library                                                                *
*  Parse an Xpm array and write a file that corresponds to it.                *
*                                                                             *
*  Developed by Dan Greening dgreen@cs.ucla.edu / dgreen@sti.com              *
\*****************************************************************************/

#include "xpmP.h"

int
XpmWriteFileFromData(filename, data)
    char *filename;
    char **data;
{
    XpmAttributes attributes;
    XpmImage image;
    int ErrorStatus;

    attributes.valuemask = XpmReturnExtensions;

    ErrorStatus = XpmCreateXpmImageFromData(data, &image, &attributes);

    if (ErrorStatus != XpmSuccess)
	return (ErrorStatus);

    ErrorStatus =
	XpmWriteFileFromXpmImage(filename, &image, &attributes, NULL);

    XpmFreeAttributes(&attributes);
    XpmFreeXpmImage(&image);

    return (ErrorStatus);
}
