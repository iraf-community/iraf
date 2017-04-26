/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrBufFrI.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Scan an image and possibly its mask and create an XPM buffer               *
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

LFUNC(WriteColors, int, (char **dataptr, unsigned int *data_size,
			 unsigned int *used_size, XpmColor *colors,
			 unsigned int ncolors, unsigned int cpp));

LFUNC(WritePixels, void, (char *dataptr, unsigned int *used_size,
			  unsigned int width, unsigned int height,
			  unsigned int cpp, unsigned int *pixels,
			  XpmColor *colors));

LFUNC(WriteExtensions, void, (char *dataptr, unsigned int *used_size,
			      XpmExtension *ext, unsigned int num));

LFUNC(ExtensionsSize, int, (XpmExtension *ext, unsigned int num));
LFUNC(CommentsSize, int, (XpmInfos *infos));

int
XpmCreateBufferFromImage(display, buffer_return, image, shapeimage, attributes)
    Display *display;
    char **buffer_return;
    XImage *image;
    XImage *shapeimage;
    XpmAttributes *attributes;
{
    int ErrorStatus;
    XpmImage xpmimage;

    /*
     * initialize return value
     */
    if (buffer_return)
	*buffer_return = NULL;

    /*
     * Scan image then create data
     */
    ErrorStatus = XpmCreateXpmImageFromImage(display, image, shapeimage,
					     &xpmimage, attributes);

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = XpmCreateBufferFromXpmImage(buffer_return, &xpmimage,
						  attributes, NULL);
    XpmFreeXpmImage(&xpmimage);

    return (ErrorStatus);
}


#undef RETURN
#define RETURN(status) \
{ \
    if (ptr) \
	XpmFree(ptr); \
    return(status); \
}

int
XpmCreateBufferFromXpmImage(buffer_return, image, attributes, infos)
    char **buffer_return;
    XpmImage *image;
    XpmAttributes *attributes;
    XpmInfos *infos;
{
    /* calculation variables */
    int ErrorStatus;
    char buf[BUFSIZ];
    unsigned int extensions = 0, ext_size = 0;
    unsigned int l, cmt_size = 0;
    char *ptr = NULL, *p;
    unsigned int ptr_size, used_size;

    *buffer_return = NULL;

    extensions = attributes && (attributes->valuemask & XpmExtensions)
	&& attributes->nextensions;

    /* compute the extensions and comments size */
    if (extensions)
	ext_size = ExtensionsSize(attributes->extensions,
				  attributes->nextensions);
    if (infos)
	cmt_size = CommentsSize(infos);

    /*
     * write the header line
     */
    sprintf(buf, "/* XPM */\nstatic char * image_name[] = {\n");
    used_size = strlen(buf);
    ptr_size = used_size + ext_size + cmt_size + 1;
    ptr = (char *) XpmMalloc(ptr_size);
    if (!ptr)
	return XpmNoMemory;
    strcpy(ptr, buf);

    /*
     * write the values line
     */
    if (infos && infos->hints_cmt) {
	sprintf(ptr + used_size, "/*%s*/\n", infos->hints_cmt);
	used_size += strlen(infos->hints_cmt) + 5;
    }
    sprintf(buf, "\"%d %d %d %d", image->width, image->height,
	    image->ncolors, image->cpp);
    l = strlen(buf);

    if (attributes && (attributes->valuemask & XpmHotspot)) {
	sprintf(buf + l, " %d %d",
		attributes->x_hotspot, attributes->y_hotspot);
	l = strlen(buf);
    }
    if (extensions) {
	sprintf(buf + l, " XPMEXT");
	l = strlen(buf);
    }
    sprintf(buf + l, "\",\n");
    l = strlen(buf);
    ptr_size += l;
    p = (char *) XpmRealloc(ptr, ptr_size);
    if (!p)
	RETURN(XpmNoMemory);
    ptr = p;
    strcpy(ptr + used_size, buf);
    used_size += l;

    /*
     * write colors
     */
    if (infos && infos->colors_cmt) {
	sprintf(ptr + used_size, "/*%s*/\n", infos->colors_cmt);
	used_size += strlen(infos->colors_cmt) + 5;
    }
    ErrorStatus = WriteColors(&ptr, &ptr_size, &used_size,
			      image->colorTable, image->ncolors, image->cpp);
 
    if (ErrorStatus != XpmSuccess)
	RETURN(ErrorStatus);

    /*
     * now we know the exact size we needed, realloc the data 4 = 1 (for
     * '"') + 3 (for '",\n') 1 = - 2 is because the last line does not end
     * with ',\n' + 3 (for '};\n')
     */
    ptr_size += image->height * (image->width * image->cpp + 4) + 1;

    p = (char *) XpmRealloc(ptr, ptr_size);
    if (!p)
	RETURN(XpmNoMemory);
    ptr = p;

    /*
     * print pixels
     */
    if (infos && infos->pixels_cmt) {
	sprintf(ptr + used_size, "/*%s*/\n", infos->pixels_cmt);
	used_size += strlen(infos->pixels_cmt) + 5;
    }
    WritePixels(ptr + used_size, &used_size, image->width, image->height,
		image->cpp, image->data, image->colorTable);

    /*
     * print extensions
     */
    if (extensions)
	WriteExtensions(ptr + used_size, &used_size,
			attributes->extensions, attributes->nextensions);

    /* close the array */
    sprintf(ptr + used_size, "};\n");

    *buffer_return = ptr;

    return (XpmSuccess);
}

static int
WriteColors(dataptr, data_size, used_size, colors, ncolors, cpp)
    char **dataptr;
    unsigned int *data_size;
    unsigned int *used_size;
    XpmColor *colors;
    unsigned int ncolors;
    unsigned int cpp;
{
    char buf[BUFSIZ];
    unsigned int a, key, l;
    char *s, *s2;
    xpmColorDefaults defaults;

    *buf = '"';
    for (a = 0; a < ncolors; a++, colors++) {

	defaults = (xpmColorDefaults) colors;
	s = buf + 1;
	strncpy(s, *defaults++, cpp);
	s += cpp;

	for (key = 1; key <= NKEYS; key++, defaults++) {
	    if (s2 = *defaults) {
		sprintf(s, "\t%s %s", xpmColorKeys[key - 1], s2);
		s += strlen(s);
	    }
	}
	strcpy(s, "\",\n");
	l = strlen(buf);
	s = (char *) XpmRealloc(*dataptr, *data_size + l);
	if (!s)
	    return (XpmNoMemory);
	*data_size += l;
	strcpy(s + *used_size, buf);
	*used_size += l;
	*dataptr = s;
    }
    return (XpmSuccess);
}

static void
WritePixels(dataptr, used_size, width, height, cpp, pixels, colors)
    char *dataptr;
    unsigned int *used_size;
    unsigned int width;
    unsigned int height;
    unsigned int cpp;
    unsigned int *pixels;
    XpmColor *colors;
{
    char *s = dataptr;
    unsigned int x, y, h;

    h = height - 1;
    for (y = 0; y < h; y++) {
	*s++ = '"';
	for (x = 0; x < width; x++, pixels++) {
	    strncpy(s, colors[*pixels].string, cpp);
	    s += cpp;
	}
	strcpy(s, "\",\n");
	s += 3;
    }
    /* duplicate some code to avoid a test in the loop */
    *s++ = '"';
    for (x = 0; x < width; x++, pixels++) {
	strncpy(s, colors[*pixels].string, cpp);
	s += cpp;
    }
    *s++ = '"';
    *used_size += s - dataptr;
}

static int
ExtensionsSize(ext, num)
    XpmExtension *ext;
    unsigned int num;
{
    unsigned int x, y, a, size;
    char **line;

    size = 0;
    for (x = 0; x < num; x++, ext++) {
	/* 11 = 10 (for ',\n"XPMEXT ') + 1 (for '"') */
	size += strlen(ext->name) + 11;
	a = ext->nlines;
	for (y = 0, line = ext->lines; y < a; y++, line++)
	    /* 4 = 3 (for ',\n"') + 1 (for '"') */
	    size += strlen(*line) + 4;
    }
    /* 13 is for ',\n"XPMENDEXT"' */
    return size + 13;
}

static void
WriteExtensions(dataptr, used_size, ext, num)
    char *dataptr;
    unsigned int *used_size;
    XpmExtension *ext;
    unsigned int num;
{
    unsigned int x, y, a;
    char **line;
    char *s = dataptr;

    for (x = 0; x < num; x++, ext++) {
	sprintf(s, ",\n\"XPMEXT %s\"", ext->name);
	s += strlen(ext->name) + 11;
	a = ext->nlines;
	for (y = 0, line = ext->lines; y < a; y++, line++) {
	    sprintf(s, ",\n\"%s\"", *line);
	    s += strlen(*line) + 4;
	}
    }
    strcpy(s, ",\n\"XPMENDEXT\"");
    *used_size += s - dataptr + 13;
}

static int
CommentsSize(infos)
    XpmInfos *infos;
{
    int size = 0;

    /* 5 = 2 (for "/_*") + 3 (for "*_/\n") */
    if (infos->hints_cmt)
	size += 5 + strlen(infos->hints_cmt);

    if (infos->colors_cmt)
	size += 5 + strlen(infos->colors_cmt);

    if (infos->pixels_cmt)
	size += 5 + strlen(infos->pixels_cmt);

    return size;
}
