/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* xpmP.h:                                                                     *
*                                                                             *
*  XPM library                                                                *
*  Private Include file                                                       *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#ifndef XPMP_h
#define XPMP_h

/*
 * lets try to solve include files
 */
#ifdef VMS

#include "sys$library:stdio.h"
#include "sys$library:string.h"

#else  /* VMS */

#include <stdio.h>
/* stdio.h doesn't declare popen on a Sequent DYNIX OS */
#ifdef sequent
extern FILE *popen();
#endif

#if defined(SYSV) || defined(SVR4)
#include <string.h>

#ifndef index
#define index strchr
#endif

#ifndef rindex
#define rindex strrchr
#endif

#else  /* defined(SYSV) || defined(SVR4) */
#include <strings.h>
#endif

#endif /* VMS */

#include "xpm.h"

#if defined(SYSV) || defined(SVR4) || defined(VMS)
#define bcopy(source, dest, count) memcpy(dest, source, count)
#define bzero(b, len) memset(b, 0, len)
#endif


/* the following should help people wanting to use their own functions */
#define XpmMalloc(size) malloc((size))
#define XpmRealloc(ptr, size) realloc((ptr), (size))
#define XpmCalloc(nelem, elsize) calloc((nelem), (elsize))


typedef struct {
    unsigned int type;
    union {
	FILE *file;
	char **data;
    }     stream;
    char *cptr;
    unsigned int line;
    int CommentLength;
    char Comment[BUFSIZ];
    char *Bcmt, *Ecmt, Bos, Eos;
    int format;			/* 1 if XPM1, 0 otherwise */
}      xpmData;

#define XPMARRAY 0
#define XPMFILE  1
#define XPMPIPE  2
#define XPMBUFFER 3

typedef unsigned char byte;
typedef int Boolean;

#define EOL '\n'
#define TAB '\t'
#define SPC ' '

typedef struct {
    char *type;				/* key word */
    char *Bcmt;				/* string beginning comments */
    char *Ecmt;				/* string ending comments */
    char Bos;				/* character beginning strings */
    char Eos;				/* character ending strings */
    char *Strs;				/* strings separator */
    char *Dec;				/* data declaration string */
    char *Boa;				/* string beginning assignment */
    char *Eoa;				/* string ending assignment */
}      xpmDataType;

extern xpmDataType xpmDataTypes[];

/*
 * rgb values and ascii names (from rgb text file) rgb values,
 * range of 0 -> 65535 color mnemonic of rgb value
 */
typedef struct {
    int r, g, b;
    char *name;
}      xpmRgbName;

/* Maximum number of rgb mnemonics allowed in rgb text file. */
#define MAX_RGBNAMES 1024

extern char *xpmColorKeys[];

#define TRANSPARENT_COLOR "None"	/* this must be a string! */

/* number of xpmColorKeys */
#define NKEYS 5

/* define some type to access the XpmColor struct as an array */
typedef char **xpmColorDefaults;

#define UNDEF_PIXEL 0x80000000

/* XPM private routines */

FUNC(xpmWriteData, int, (xpmData *mdata, XpmImage *image, char *name,
			 XpmAttributes *attributes, XpmInfos *infos));

FUNC(xpmParseData, int, (xpmData *data, XpmImage *image,
			 XpmAttributes *attributes, XpmInfos *infos));

FUNC(xpmFreeColorTable, void, (XpmColor *colorTable, int ncolors));

FUNC(xpmInitAttributes, void, (XpmAttributes *attributes));

FUNC(xpmInitXpmImage, void, (XpmImage *image));

FUNC(xpmInitXpmInfos, void, (XpmInfos *infos));

/* I/O utility */

FUNC(xpmNextString, int, (xpmData *mdata));
FUNC(xpmNextUI, int, (xpmData *mdata, unsigned int *ui_return));
FUNC(xpmGetString, int, (xpmData *mdata, char **sptr, unsigned int *l));

#define xpmGetC(mdata) \
	((!mdata->type || mdata->type == XPMBUFFER) ? \
	 (*mdata->cptr++) : (getc(mdata->stream.file)))

FUNC(xpmNextWord, unsigned int,
     (xpmData *mdata, char *buf, unsigned int buflen));
FUNC(xpmGetCmt, int, (xpmData *mdata, char **cmt));
FUNC(xpmReadFile, int, (char *filename, xpmData *mdata));
FUNC(xpmWriteFile, int, (char *filename, xpmData *mdata));
FUNC(xpmOpenArray, void, (char **data, xpmData *mdata));
FUNC(xpmDataClose, int, (xpmData *mdata));
FUNC(xpmParseHeader, int, (xpmData *mdata));
FUNC(xpmOpenBuffer, void, (char *buffer, xpmData *mdata));

/* RGB utility */

FUNC(xpmReadRgbNames, int, (char *rgb_fname, xpmRgbName *rgbn));
FUNC(xpmGetRgbName, char *, (xpmRgbName *rgbn, int rgbn_max,
			     int red, int green, int blue));
FUNC(xpmFreeRgbNames, void, (xpmRgbName *rgbn, int rgbn_max));

FUNC(xpm_xynormalizeimagebits, void, (register unsigned char *bp,
				      register XImage *img));
FUNC(xpm_znormalizeimagebits, void, (register unsigned char *bp,
				     register XImage *img));

/*
 * Macros
 *
 * The XYNORMALIZE macro determines whether XY format data requires
 * normalization and calls a routine to do so if needed. The logic in
 * this module is designed for LSBFirst byte and bit order, so
 * normalization is done as required to present the data in this order.
 *
 * The ZNORMALIZE macro performs byte and nibble order normalization if
 * required for Z format data.
 *
 * The XYINDEX macro computes the index to the starting byte (char) boundary
 * for a bitmap_unit containing a pixel with coordinates x and y for image
 * data in XY format.
 *
 * The ZINDEX* macros compute the index to the starting byte (char) boundary
 * for a pixel with coordinates x and y for image data in ZPixmap format.
 *
 */

#define XYNORMALIZE(bp, img) \
    if ((img->byte_order == MSBFirst) || (img->bitmap_bit_order == MSBFirst)) \
	xpm_xynormalizeimagebits((unsigned char *)(bp), img)

#define ZNORMALIZE(bp, img) \
    if (img->byte_order == MSBFirst) \
	xpm_znormalizeimagebits((unsigned char *)(bp), img)

#define XYINDEX(x, y, img) \
    ((y) * img->bytes_per_line) + \
    (((x) + img->xoffset) / img->bitmap_unit) * (img->bitmap_unit >> 3)

#define ZINDEX(x, y, img) ((y) * img->bytes_per_line) + \
    (((x) * img->bits_per_pixel) >> 3)

#define ZINDEX32(x, y, img) ((y) * img->bytes_per_line) + ((x) << 2)

#define ZINDEX16(x, y, img) ((y) * img->bytes_per_line) + ((x) << 1)

#define ZINDEX8(x, y, img) ((y) * img->bytes_per_line) + (x)

#define ZINDEX1(x, y, img) ((y) * img->bytes_per_line) + ((x) >> 3)

#if __STDC__
#define Const const
#else
#define Const				/**/
#endif

/*
 * there are structures and functions related to hastable code
 */

typedef struct _xpmHashAtom {
    char *name;
    void *data;
}      *xpmHashAtom;

typedef struct {
    int size;
    int limit;
    int used;
    xpmHashAtom *atomTable;
}      xpmHashTable;

FUNC(xpmHashTableInit, int, (xpmHashTable *table));
FUNC(xpmHashTableFree, void, (xpmHashTable *table));
FUNC(xpmHashSlot, xpmHashAtom *, (xpmHashTable *table, char *s));
FUNC(xpmHashIntern, int, (xpmHashTable *table, char *tag, void *data));

#define HashAtomData(i) ((void *)i)
#define HashColorIndex(slot) ((unsigned int)((*slot)->data))
#define USE_HASHTABLE (cpp > 2 && ncolors > 4)

#ifdef NEED_STRDUP
FUNC(strdup, char *, (char *s1));
#endif

#endif
