/* Copyright 1990,91 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmRdFToData.c:                                                             *
*                                                                             *
*  XPM library                                                                *
*  Parse an XPM file and create an array of strings corresponding to it.      *
*                                                                             *
*  Developed by Dan Greening dgreen@cs.ucla.edu / dgreen@sti.com              *
\*****************************************************************************/

#include "xpmP.h"

int
XpmReadFileToData(filename, data_return)
    char *filename;
    char ***data_return;
{
    XpmAttributes attributes;
    XpmImage image;
    int ErrorStatus;

    attributes.valuemask = XpmReturnExtensions;

    /*
     * initialize return value
     */
    if (data_return)
	*data_return = NULL;

    ErrorStatus = XpmReadFileToXpmImage(filename, &image, &attributes, NULL);
    if (ErrorStatus != XpmSuccess)
	return (ErrorStatus);

    ErrorStatus =
	XpmCreateDataFromXpmImage(data_return, &image, &attributes);

    XpmFreeAttributes(&attributes);
    XpmFreeXpmImage(&image);

    return (ErrorStatus);
}
