/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmWrFFrI.c:                                                                *
*                                                                             *
*  XPM library                                                                *
*  Write an image and possibly its mask to an XPM file                        *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

LFUNC(WriteColors, void, (FILE *file, XpmColor *colors, unsigned int ncolors));

LFUNC(WritePixels, int, (FILE *file, unsigned int width, unsigned int height,
			 unsigned int cpp, unsigned int *pixels,
			 XpmColor *colors));

LFUNC(WriteExtensions, void, (FILE *file, XpmExtension *ext,
			      unsigned int num));

int
XpmWriteFileFromImage(display, filename, image, shapeimage, attributes)
    Display *display;
    char *filename;
    XImage *image;
    XImage *shapeimage;
    XpmAttributes *attributes;
{
    XpmImage xpmimage;
    int ErrorStatus;

    /*
     * Scan image
     */
    ErrorStatus = XpmCreateXpmImageFromImage(display, image, shapeimage,
					     &xpmimage, attributes);
    if (ErrorStatus == XpmSuccess)
	ErrorStatus = XpmWriteFileFromXpmImage(filename, &xpmimage,
					       attributes, NULL);

    XpmFreeXpmImage(&xpmimage);

    return (ErrorStatus);
}

int
XpmWriteFileFromXpmImage(filename, image, attributes, infos)
    char *filename;
    XpmImage *image;
    XpmAttributes *attributes;
    XpmInfos *infos;
{
    xpmData mdata;
    char *name, *dot, *s, *new_name = NULL;
    int ErrorStatus;

    if ((ErrorStatus = xpmWriteFile(filename, &mdata)) != XpmSuccess)
	return (ErrorStatus);

    if (filename) {
#ifdef VMS
	name = filename;
#else
	if (!(name = rindex(filename, '/')))
	    name = filename;
	else
	    name++;
#endif
	if (dot = index(name, '.')) {
	    new_name = (char *) strdup(name);
	    if (!new_name) {
		new_name = NULL;
		name = "image_name";
	    } else {
		/* change '.' to '_' to get a valid C syntax name */
		name = s = new_name;
		while (dot = index(s, '.')) {
		    *dot = '_';
		    s = dot;
		}
	    }
	}
    } else
	name = "image_name";

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = xpmWriteData(&mdata, image, name, attributes, infos);

    xpmDataClose(&mdata);
    if (new_name)
	XpmFree(name);

    return (ErrorStatus);
}

int
xpmWriteData(mdata, image, name, attributes, infos)
    xpmData *mdata;
    XpmImage *image;
    char *name;
    XpmAttributes *attributes;
    XpmInfos *infos;
{
    /* calculation variables */
    unsigned int extensions;
    FILE *file;
    int ErrorStatus;

    /* store this to speed up */
    file = mdata->stream.file;

    extensions = attributes && (attributes->valuemask & XpmExtensions)
	&& attributes->nextensions;

    /*
     * print the header line
     */
    fprintf(file, "/* XPM */\nstatic char * %s[] = {\n", name);

    /*
     * print the hints line
     */
    if (infos && infos->hints_cmt)
	fprintf(file, "/*%s*/\n", infos->hints_cmt);

    fprintf(file, "\"%d %d %d %d", image->width, image->height,
	    image->ncolors, image->cpp);

    if (attributes && (attributes->valuemask & XpmHotspot))
	fprintf(file, " %d %d", attributes->x_hotspot, attributes->y_hotspot);

    if (extensions)
	fprintf(file, " XPMEXT");

    fprintf(file, "\",\n");

    /*
     * print colors
     */
    if (infos && infos->colors_cmt)
	fprintf(file, "/*%s*/\n", infos->colors_cmt);

    WriteColors(file, image->colorTable, image->ncolors);

    /*
     * print pixels
     */
    if (infos && infos->pixels_cmt)
	fprintf(file, "/*%s*/\n", infos->pixels_cmt);

    ErrorStatus = WritePixels(file, image->width, image->height, image->cpp,
			      image->data, image->colorTable);
    if (ErrorStatus != XpmSuccess)
	return (ErrorStatus);

    /*
     * print extensions
     */
    if (extensions)
	WriteExtensions(file, attributes->extensions, attributes->nextensions);

    /* close the array */
    fprintf(file, "};\n");

    return (XpmSuccess);
}

static void
WriteColors(file, colors, ncolors)
    FILE *file;
    XpmColor *colors;
    unsigned int ncolors;
{
    unsigned int a, key;
    char *s;
    xpmColorDefaults defaults;

    for (a = 0; a < ncolors; a++, colors++) {

	defaults = (xpmColorDefaults) colors;
	fprintf(file, "\"%s", *defaults++);

	for (key = 1; key <= NKEYS; key++, defaults++) {
	    if (s = *defaults)
		fprintf(file, "\t%s %s", xpmColorKeys[key - 1], s);
	}
	fprintf(file, "\",\n");
    }
}


static int
WritePixels(file, width, height, cpp, pixels, colors)
    FILE *file;
    unsigned int width;
    unsigned int height;
    unsigned int cpp;
    unsigned int *pixels;
    XpmColor *colors;
{
    char *s, *p, *buf;
    unsigned int x, y, h;

    h = height - 1;
    p = buf = (char *) XpmMalloc(width * cpp + 3);
    if (!buf)
	return (XpmNoMemory);
    *buf = '"';
    p++;
    for (y = 0; y < h; y++) {
	s = p;
	for (x = 0; x < width; x++, pixels++) {
	    strncpy(s, colors[*pixels].string, cpp);
	    s += cpp;
	}
	*s++ = '"';
	*s = '\0';
	fprintf(file, "%s,\n", buf);
    }
    /* duplicate some code to avoid a test in the loop */
    s = p;
    for (x = 0; x < width; x++, pixels++) {
	strncpy(s, colors[*pixels].string, cpp);
	s += cpp;
    }
    *s++ = '"';
    *s = '\0';
    fprintf(file, "%s", buf);

    XpmFree(buf);
    return (XpmSuccess);
}

static void
WriteExtensions(file, ext, num)
    FILE *file;
    XpmExtension *ext;
    unsigned int num;
{
    unsigned int x, y, n;
    char **line;

    for (x = 0; x < num; x++, ext++) {
	fprintf(file, ",\n\"XPMEXT %s\"", ext->name);
	n = ext->nlines;
	for (y = 0, line = ext->lines; y < n; y++, line++)
	    fprintf(file, ",\n\"%s\"", *line);
    }
    fprintf(file, ",\n\"XPMENDEXT\"");
}
