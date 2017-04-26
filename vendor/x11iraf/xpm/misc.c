/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* misc.c:                                                                     *
*                                                                             *
*  XPM library                                                               *
*  Miscellaneous utilities                                                    *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"
#ifdef VMS
#include "sys$library:stat.h"
#include "sys$library:fcntl.h"
#else
#include <sys/stat.h>
#include <fcntl.h>
#endif

/*
 * Free the computed color table
 */
void
xpmFreeColorTable(colorTable, ncolors)
    XpmColor *colorTable;
    int ncolors;
{
    int a, b;
    XpmColor *color;
    xpmColorDefaults defaults;
    char **sptr;

    if (colorTable) {
	for (a = 0, color = colorTable; a < ncolors; a++, color++) {
	    defaults = (xpmColorDefaults) color;
	    for (b = 0, sptr = (char **) defaults; b <= NKEYS; b++, sptr++)
		if (*sptr)
		    XpmFree(*sptr);
	}
	XpmFree(colorTable);
    }
}


/*
 * Free array of extensions
 */
void
XpmFreeExtensions(extensions, nextensions)
    XpmExtension *extensions;
    int nextensions;
{
    unsigned int i, j, nlines;
    XpmExtension *ext;
    char **sptr;

    if (extensions) {
	for (i = 0, ext = extensions; i < nextensions; i++, ext++) {
	    if (ext->name)
		XpmFree(ext->name);
	    nlines = ext->nlines;
	    for (j = 0, sptr = ext->lines; j < nlines; j++, sptr++)
		if (*sptr)
		    XpmFree(*sptr);
	    if (ext->lines)
		XpmFree(ext->lines);
	}
	XpmFree(extensions);
    }
}


/*
 * Return the XpmAttributes structure size
 */

int 
XpmAttributesSize()
{
    return sizeof(XpmAttributes);
}

/*
 * Init returned data to free safely later on
 */
void
xpmInitAttributes(attributes)
    XpmAttributes *attributes;
{
    if (attributes) {
	attributes->pixels = NULL;
	attributes->npixels = 0;
	attributes->extensions = NULL;
	attributes->nextensions = 0;
    }
}

/*
 * Free the XpmAttributes structure members
 * but the structure itself
 */
void
XpmFreeAttributes(attributes)
    XpmAttributes *attributes;
{
    if (attributes) {
	if (attributes->valuemask & XpmReturnPixels && attributes->npixels) {
	    XpmFree(attributes->pixels);
	    attributes->pixels = NULL;
	    attributes->npixels = 0;
	}
	if (attributes->valuemask & XpmReturnExtensions
	    && attributes->nextensions) {
	    XpmFreeExtensions(attributes->extensions, attributes->nextensions);
	    attributes->extensions = NULL;
	    attributes->nextensions = 0;
	}
	attributes->valuemask = 0;
    }
}

/*
 * Init returned data to free safely later on
 */
void
xpmInitXpmImage(image)
    XpmImage *image;
{
    image->ncolors = 0;
    image->colorTable = NULL;
    image->data = NULL;
}

/*
 * Free the XpmImage data which have been allocated
 */
void
XpmFreeXpmImage(image)
    XpmImage *image;
{
    if (image->colorTable)
	xpmFreeColorTable(image->colorTable, image->ncolors);
    XpmFree(image->data);
    image->data = NULL;
}

/*
 * Init returned data to free safely later on
 */
void
xpmInitXpmInfos(infos)
    XpmInfos *infos;
{
    if (infos) {
	infos->hints_cmt = NULL;
	infos->colors_cmt = NULL;
	infos->pixels_cmt = NULL;
    }
}

/*
 * Free the XpmInfos data which have been allocated
 */
void
XpmFreeXpmInfos(infos)
    XpmInfos *infos;
{
    if (infos) {
	if (infos->hints_cmt) {
	    XpmFree(infos->hints_cmt);
	    infos->hints_cmt = NULL;
	}
	if (infos->colors_cmt) {
	    XpmFree(infos->colors_cmt);
	    infos->colors_cmt = NULL;
	}
	if (infos->pixels_cmt) {
	    XpmFree(infos->pixels_cmt);
	    infos->pixels_cmt = NULL;
	}
    }
}


#ifdef NEED_STRDUP
/*
 * in case strdup is not provided by the system here is one
 * which does the trick
 */
char *
strdup(s1)
    char *s1;
{
    char *s2;
    int l = strlen(s1) + 1;

    if (s2 = (char *) XpmMalloc(l))
	strncpy(s2, s1, l);
    return s2;
}

#endif

/*
 *  File / Buffer utilities
 */
int
XpmReadFileToBuffer(filename, buffer_return)
    char *filename;
    char **buffer_return;
{
    int fd, fcheck, len;
    char *ptr;
    struct stat stats;
    FILE *fp;

    *buffer_return = NULL;

    fd = open(filename, O_RDONLY);
    if (fd < 0)
	return XpmOpenFailed;

    if (fstat(fd, &stats)) {
	close(fd);
	return XpmOpenFailed;
    }
    fp = fdopen(fd, "r");
    if (!fp) {
	close(fd);
	return XpmOpenFailed;
    }
    len = (int) stats.st_size;
    ptr = (char *) XpmMalloc(len + 1);
    if (!ptr) {
	fclose(fp);
	return XpmNoMemory;
    }
    fcheck = fread(ptr, len, 1, fp);
    fclose(fp);
    if (fcheck != 1) {
	XpmFree(ptr);
	return XpmOpenFailed;
    }
    ptr[len] = '\0';
    *buffer_return = ptr;
    return XpmSuccess;
}

int
XpmWriteFileFromBuffer(filename, buffer)
    char *filename;
    char *buffer;
{
    int fcheck, len;
    FILE *fp = fopen(filename, "w");

    if (!fp)
	return XpmOpenFailed;

    len = strlen(buffer);
    fcheck = fwrite(buffer, len, 1, fp);
    fclose(fp);
    if (fcheck != 1)
	return XpmOpenFailed;

    return XpmSuccess;
}


/*
 * Small utility function
 */
char *
XpmGetErrorString(errcode)
    int errcode;
{
    switch (errcode) {
    case XpmColorError:
	return ("XpmColorError");
    case XpmSuccess:
	return ("XpmSuccess");
    case XpmOpenFailed:
	return ("XpmOpenFailed");
    case XpmFileInvalid:
	return ("XpmFileInvalid");
    case XpmNoMemory:
	return ("XpmNoMemory");
    case XpmColorFailed:
	return ("XpmColorFailed");
    default:
	return ("Invalid XpmError");
    }
}
