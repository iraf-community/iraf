/* Copyright 1990-93 GROUPE BULL -- See license conditions in file COPYRIGHT */
/*****************************************************************************\
* scan.c:                                                                     *
*                                                                             *
*  XPM library                                                                *
*  Scanning utility for XPM file format                                        *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

#define MAXPRINTABLE 92			/* number of printable ascii chars
					 * minus \ and " for string compat
					 * and ? to avoid ANSI trigraphs. */

static char *printable =
" .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZ\
ASDFGHJKLPIUYTREWQ!~^/()_`'][{}|";

/*
 * printable begin with a space, so in most case, due to my algorithm, when
 * the number of different colors is less than MAXPRINTABLE, it will give a
 * char follow by "nothing" (a space) in the readable xpm file
 */


typedef struct {
    Pixel *pixels;
    unsigned int *pixelindex;
    unsigned int size;
    unsigned int ncolors;
    unsigned int mask_pixel;		/* whether there is or not */
}      PixelsMap;

LFUNC(storePixel, int, (Pixel pixel, PixelsMap *pmap,
			unsigned int *index_return));

LFUNC(storeMaskPixel, int, (Pixel pixel, PixelsMap *pmap,
			    unsigned int *index_return));

LFUNC(GetImagePixels, int, (XImage *image, unsigned int width,
			    unsigned int height, PixelsMap *pmap));

LFUNC(GetImagePixels32, int, (XImage *image, unsigned int width,
			      unsigned int height, PixelsMap *pmap));

LFUNC(GetImagePixels16, int, (XImage *image, unsigned int width,
			      unsigned int height, PixelsMap *pmap));

LFUNC(GetImagePixels8, int, (XImage *image, unsigned int width,
			     unsigned int height, PixelsMap *pmap));

LFUNC(GetImagePixels1, int, (XImage *image, unsigned int width,
			     unsigned int height, PixelsMap *pmap,
			     int (*storeFunc) ()));

LFUNC(ScanTransparentColor, int, (XpmColor *color, unsigned int cpp));

LFUNC(ScanOtherColors, int, (Display *display, XpmColor *colors, int ncolors,
			     Pixel *pixels, unsigned int cpp,
			     XpmAttributes *attributes));

/*
 * This function stores the given pixel in the given arrays which are grown
 * if not large enough.
 */
static int
storePixel(pixel, pmap, index_return)
    Pixel pixel;
    PixelsMap *pmap;
    unsigned int *index_return;
{
    unsigned int a;
    Pixel *p;
    unsigned int ncolors;

    if (*index_return) {		/* this is a transparent pixel! */
	*index_return = 0;
	return 0;
    }
    ncolors = pmap->ncolors;
    p = &(pmap->pixels[pmap->mask_pixel]);
    for (a = pmap->mask_pixel; a < ncolors; a++, p++)
	if (*p == pixel)
	    break;
    if (a == ncolors) {
	if (ncolors >= pmap->size) {
	    pmap->size *= 2;
	    p = (Pixel *) XpmRealloc(pmap->pixels, sizeof(Pixel) * pmap->size);
	    if (!p)
		return (1);
	    pmap->pixels = p;

	}
	(pmap->pixels)[ncolors] = pixel;
	pmap->ncolors++;
    }
    *index_return = a;
    return 0;
}

static int
storeMaskPixel(pixel, pmap, index_return)
    Pixel pixel;
    PixelsMap *pmap;
    unsigned int *index_return;
{
    if (!pixel) {
	if (!pmap->ncolors) {
	    pmap->ncolors = 1;
	    (pmap->pixels)[0] = 0;
	    pmap->mask_pixel = 1;
	}
	*index_return = 1;
    } else
	*index_return = 0;
    return 0;
}

/* function call in case of error, frees only locally allocated variables */
#undef RETURN
#define RETURN(status) \
{ \
    if (pmap.pixelindex) XpmFree(pmap.pixelindex); \
    if (pmap.pixels) XpmFree(pmap.pixels); \
    if (colorTable) xpmFreeColorTable(colorTable, pmap.ncolors); \
    return(status); \
}

/*
 * This function scans the given image and stores the found informations in
 * the given XpmImage structure.
 */
int
XpmCreateXpmImageFromImage(display, image, shapeimage,
			   xpmimage, attributes)
    Display *display;
    XImage *image;
    XImage *shapeimage;
    XpmImage *xpmimage;
    XpmAttributes *attributes;
{
    /* variables stored in the XpmAttributes structure */
    unsigned int cpp;

    /* variables to return */
    PixelsMap pmap;
    XpmColor *colorTable = NULL;
    int ErrorStatus;

    /* calculation variables */
    unsigned int width = 0;
    unsigned int height = 0;
    unsigned int cppm;			/* minimum chars per pixel */
    unsigned int c;
    unsigned int offset;

    /* initialize pmap */
    pmap.pixels = NULL;
    pmap.pixelindex = NULL;
    pmap.size = 256;			/* should be enough most of the time */
    pmap.ncolors = 0;
    pmap.mask_pixel = 0;

    /*
     * get geometry
     */
    if (image) {
	width = image->width;
	height = image->height;
    } else if (shapeimage) {
	width = shapeimage->width;
	height = shapeimage->height;
    }

    /*
     * retrieve information from the XpmAttributes
     */
    if (attributes && (attributes->valuemask & XpmCharsPerPixel))
	cpp = attributes->cpp;
    else
	cpp = 0;

    pmap.pixelindex =
	(unsigned int *) XpmCalloc(width * height, sizeof(unsigned int));
    if (!pmap.pixelindex)
	RETURN(XpmNoMemory);

    pmap.pixels = (Pixel *) XpmMalloc(sizeof(Pixel) * pmap.size);
    if (!pmap.pixels)
	RETURN(XpmNoMemory);

    /*
     * scan shape mask if any
     */
    if (shapeimage) {
	ErrorStatus = GetImagePixels1(shapeimage, width, height, &pmap,
				      storeMaskPixel);
	if (ErrorStatus != XpmSuccess)
	    RETURN(ErrorStatus);
    }

    /*
     * scan the image data
     * 
     * In case depth is 1 or bits_per_pixel is 4, 6, 8, 24 or 32 use optimized
     * functions, otherwise use slower but sure general one.
     * 
     */

    if (image) {
	if (image->depth == 1)
	    ErrorStatus = GetImagePixels1(image, width, height, &pmap,
					  storePixel);
	else if (image->bits_per_pixel == 8)
	    ErrorStatus = GetImagePixels8(image, width, height, &pmap);
	else if (image->bits_per_pixel == 16)
	    ErrorStatus = GetImagePixels16(image, width, height, &pmap);
	else if (image->bits_per_pixel == 32)
	    ErrorStatus = GetImagePixels32(image, width, height, &pmap);
	else
	    ErrorStatus = GetImagePixels(image, width, height, &pmap);

	if (ErrorStatus != XpmSuccess)
	    RETURN(ErrorStatus);
    }

    /*
     * get rgb values and a string of char, and possibly a name for each
     * color
     */

    colorTable = (XpmColor *) XpmCalloc(pmap.ncolors, sizeof(XpmColor));
    if (!colorTable)
	RETURN(XpmNoMemory);

    /* compute the minimal cpp */
    for (cppm = 1, c = MAXPRINTABLE; pmap.ncolors > c; cppm++)
	c *= MAXPRINTABLE;
    if (cpp < cppm)
	cpp = cppm;

    if (pmap.mask_pixel) {
	ErrorStatus = ScanTransparentColor(colorTable, cpp);
	if (ErrorStatus != XpmSuccess)
	    RETURN(ErrorStatus);
	offset = 1;
    } else
	offset = 0;

    ErrorStatus = ScanOtherColors(display, colorTable + offset,
				  pmap.ncolors - offset,
				  pmap.pixels, cpp, attributes);
    if (ErrorStatus != XpmSuccess)
	RETURN(ErrorStatus);

    /*
     * store found informations in the XpmImage structure
     */
    xpmimage->width = width;
    xpmimage->height = height;
    xpmimage->cpp = cpp;
    xpmimage->ncolors = pmap.ncolors;
    xpmimage->colorTable = colorTable;
    xpmimage->data = pmap.pixelindex;

    XpmFree(pmap.pixels);
    return (XpmSuccess);
}

static int
ScanTransparentColor(color, cpp)
    XpmColor *color;
    unsigned int cpp;
{
    char *s;
    unsigned int a, b, c;

    /* first get a character string */
    a = 0;
    if (!(s = color->string = (char *) XpmMalloc(cpp + 1)))
	return (XpmNoMemory);
    *s++ = printable[c = a % MAXPRINTABLE];
    for (b = 1; b < cpp; b++, s++)
	*s = printable[c = ((a - c) / MAXPRINTABLE) % MAXPRINTABLE];
    *s = '\0';

    color->c_color = (char*)strdup(TRANSPARENT_COLOR);
    if (!color->c_color)
	return (XpmNoMemory);

    return (XpmSuccess);
}

static int
ScanOtherColors(display, colors, ncolors, pixels, cpp, attributes)
    Display *display;
    XpmColor *colors;
    int ncolors;
    Pixel *pixels;
    unsigned int cpp;
    XpmAttributes *attributes;
{
    /* variables stored in the XpmAttributes structure */
    Colormap colormap;
    char *rgb_fname;

    xpmRgbName rgbn[MAX_RGBNAMES];
    int rgbn_max = 0;
    unsigned int a, b, c;
    XpmColor *color;
    XColor *xcolors = NULL, *xcolor;
    char *colorname, *s;

    /* retrieve information from the XpmAttributes */
    if (attributes && (attributes->valuemask & XpmColormap))
	colormap = attributes->colormap;
    else
	colormap = DefaultColormap(display, DefaultScreen(display));
    if (attributes && (attributes->valuemask & XpmRgbFilename))
	rgb_fname = attributes->rgb_fname;
    else
	rgb_fname = NULL;

    /* first get character strings and rgb values */
    xcolors = (XColor *) XpmMalloc(sizeof(XColor) * ncolors);
    if (!xcolors)
	return (XpmNoMemory);

    for (a = 0, color = colors, xcolor = xcolors; a < ncolors;
	 a++, color++, xcolor++, pixels++) {

	if (!(s = color->string = (char *) XpmMalloc(cpp + 1))) {
	    XpmFree(xcolors);
	    return (XpmNoMemory);
	}
	*s++ = printable[c = a % MAXPRINTABLE];
	for (b = 1; b < cpp; b++, s++)
	    *s = printable[c = ((a - c) / MAXPRINTABLE) % MAXPRINTABLE];
	*s = '\0';

	xcolor->pixel = *pixels;
    }
    XQueryColors(display, colormap, xcolors, ncolors);

    /* read the rgb file if any was specified */
    if (rgb_fname)
	rgbn_max = xpmReadRgbNames(attributes->rgb_fname, rgbn);

    for (a = 0, color = colors, xcolor = xcolors; a < ncolors
	 ; a++, color++, xcolor++) {

	/* look for a color name */
	colorname = NULL;
	if (rgbn_max)
	    colorname = xpmGetRgbName(rgbn, rgbn_max, xcolor->red,
				      xcolor->green, xcolor->blue);
	if (colorname)
	    color->c_color = (char*)strdup(colorname);
	else {
	    /* at last store the rgb value */
	    char buf[BUFSIZ];

	    sprintf(buf, "#%04X%04X%04X",
		    xcolor->red, xcolor->green, xcolor->blue);
	    color->c_color = (char*)strdup(buf);
	}
	if (!color->c_color) {
	    XpmFree(xcolors);
	    xpmFreeRgbNames(rgbn, rgbn_max);
	    return (XpmNoMemory);
	}
    }

    XpmFree(xcolors);
    xpmFreeRgbNames(rgbn, rgbn_max);
    return (XpmSuccess);
}


/*
 * The functions below are written from X11R5 MIT's code (XImUtil.c)
 *
 * The idea is to have faster functions than the standard XGetPixel function
 * to scan the image data. Indeed we can speed up things by suppressing tests
 * performed for each pixel. We do exactly the same tests but at the image
 * level. Assuming that we use only ZPixmap images.
 */

static unsigned long Const low_bits_table[] = {
    0x00000000, 0x00000001, 0x00000003, 0x00000007,
    0x0000000f, 0x0000001f, 0x0000003f, 0x0000007f,
    0x000000ff, 0x000001ff, 0x000003ff, 0x000007ff,
    0x00000fff, 0x00001fff, 0x00003fff, 0x00007fff,
    0x0000ffff, 0x0001ffff, 0x0003ffff, 0x0007ffff,
    0x000fffff, 0x001fffff, 0x003fffff, 0x007fffff,
    0x00ffffff, 0x01ffffff, 0x03ffffff, 0x07ffffff,
    0x0fffffff, 0x1fffffff, 0x3fffffff, 0x7fffffff,
    0xffffffff
};

/*
 * Default method to scan pixels of a Z image data structure.
 * The algorithm used is:
 *
 *	copy the source bitmap_unit or Zpixel into temp
 *	normalize temp if needed
 *	extract the pixel bits into return value
 *
 */

static int
GetImagePixels(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    char *src;
    char *dst;
    unsigned int *iptr;
    char *data;
    int x, y, i;
    int bits, depth, ibu, ibpp;
    unsigned long lbt;
    Pixel pixel, px;

    data = image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
    ibpp = image->bits_per_pixel;
    if (image->depth == 1) {
	ibu = image->bitmap_unit;
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		src = &data[XYINDEX(x, y, image)];
		dst = (char *) &pixel;
		pixel = 0;
		for (i = ibu >> 3; --i >= 0;)
		    *dst++ = *src++;
		XYNORMALIZE(&pixel, image);
		bits = x % ibu;
		pixel = ((((char *) &pixel)[bits >> 3]) >> (bits & 7)) & 1;
		if (ibpp != depth)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XpmNoMemory);
	    }
    } else {
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		src = &data[ZINDEX(x, y, image)];
		dst = (char *) &px;
		px = 0;
		for (i = (ibpp + 7) >> 3; --i >= 0;)
		    *dst++ = *src++;
		ZNORMALIZE(&px, image);
		pixel = 0;
		for (i = sizeof(unsigned long); --i >= 0;)
		    pixel = (pixel << 8) | ((unsigned char *) &px)[i];
		if (ibpp == 4) {
		    if (x & 1)
			pixel >>= 4;
		    else
			pixel &= 0xf;
		}
		if (ibpp != depth)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XpmNoMemory);
	    }
    }
    return (XpmSuccess);
}

/*
 * scan pixels of a 32-bits Z image data structure
 */

#ifndef WORD64
static unsigned long byteorderpixel = MSBFirst << 24;

#endif

static int
GetImagePixels32(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    unsigned char *addr;
    unsigned char *data;
    unsigned int *iptr;
    int x, y;
    unsigned long lbt;
    Pixel pixel;
    int depth;

    data = (unsigned char *) image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
#ifndef WORD64
    if (*((char *) &byteorderpixel) == image->byte_order) {
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = *((unsigned long *) addr);
		if (depth != 32)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XpmNoMemory);
	    }
    } else
#endif
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = ((unsigned long) addr[0] << 24 |
			 (unsigned long) addr[1] << 16 |
			 (unsigned long) addr[2] << 8 |
			 addr[4]);
		if (depth != 32)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XpmNoMemory);
	    }
    else
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX32(x, y, image)];
		pixel = (addr[0] |
			 (unsigned long) addr[1] << 8 |
			 (unsigned long) addr[2] << 16 |
			 (unsigned long) addr[3] << 24);
		if (depth != 32)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XpmNoMemory);
	    }
    return (XpmSuccess);
}

/*
 * scan pixels of a 16-bits Z image data structure
 */

static int
GetImagePixels16(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    unsigned char *addr;
    unsigned char *data;
    unsigned int *iptr;
    int x, y;
    unsigned long lbt;
    Pixel pixel;
    int depth;

    data = (unsigned char *) image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
    if (image->byte_order == MSBFirst)
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX16(x, y, image)];
		pixel = addr[0] << 8 | addr[1];
		if (depth != 16)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XpmNoMemory);
	    }
    else
	for (y = 0; y < height; y++)
	    for (x = 0; x < width; x++, iptr++) {
		addr = &data[ZINDEX16(x, y, image)];
		pixel = addr[0] | addr[1] << 8;
		if (depth != 16)
		    pixel &= lbt;
		if (storePixel(pixel, pmap, iptr))
		    return (XpmNoMemory);
	    }
    return (XpmSuccess);
}

/*
 * scan pixels of a 8-bits Z image data structure
 */

static int
GetImagePixels8(image, width, height, pmap)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
{
    unsigned int *iptr;
    unsigned char *data;
    int x, y;
    unsigned long lbt;
    Pixel pixel;
    int depth;

    data = (unsigned char *) image->data;
    iptr = pmap->pixelindex;
    depth = image->depth;
    lbt = low_bits_table[depth];
    for (y = 0; y < height; y++)
	for (x = 0; x < width; x++, iptr++) {
	    pixel = data[ZINDEX8(x, y, image)];
	    if (depth != 8)
		pixel &= lbt;
	    if (storePixel(pixel, pmap, iptr))
		return (XpmNoMemory);
	}
    return (XpmSuccess);
}

/*
 * scan pixels of a 1-bit depth Z image data structure
 */

static int
GetImagePixels1(image, width, height, pmap, storeFunc)
    XImage *image;
    unsigned int width;
    unsigned int height;
    PixelsMap *pmap;
    int (*storeFunc) ();

{
    unsigned int *iptr;
    int x, y;
    char *data;
    Pixel pixel;

    if (image->byte_order != image->bitmap_bit_order)
	return (GetImagePixels(image, width, height, pmap));
    else {
	data = image->data;
	iptr = pmap->pixelindex;
	if (image->bitmap_bit_order == MSBFirst)
	    for (y = 0; y < height; y++)
		for (x = 0; x < width; x++, iptr++) {
		    pixel = (data[ZINDEX1(x, y, image)] & (0x80 >> (x & 7)))
			? 1 : 0;
		    if ((*storeFunc) (pixel, pmap, iptr))
			return (XpmNoMemory);
		}
	else
	    for (y = 0; y < height; y++)
		for (x = 0; x < width; x++, iptr++) {
		    pixel = (data[ZINDEX1(x, y, image)] & (1 << (x & 7)))
			? 1 : 0;
		    if ((*storeFunc) (pixel, pmap, iptr))
			return (XpmNoMemory);
		}
    }
    return (XpmSuccess);
}


int
XpmCreateXpmImageFromPixmap(display, pixmap, shapemask,
			    xpmimage, attributes)
    Display *display;
    Pixmap pixmap;
    Pixmap shapemask;
    XpmImage *xpmimage;
    XpmAttributes *attributes;
{
    XImage *image = NULL;
    XImage *shapeimage = NULL;
    unsigned int width = 0;
    unsigned int height = 0;
    int ErrorStatus;
    unsigned int dum;
    int dummy;
    Window win;

    /*
     * get geometry
     */
    if (attributes && attributes->valuemask & XpmSize) {
	width = attributes->width;
	height = attributes->height;
    } else {
	if (pixmap)
	    XGetGeometry(display, pixmap, &win, &dummy, &dummy,
			 &width, &height, &dum, &dum);
	else if (shapemask)
	    XGetGeometry(display, shapemask, &win, &dummy, &dummy,
			 &width, &height, &dum, &dum);
    }

    /*
     * get the images
     */
    if (pixmap)
	image = XGetImage(display, pixmap, 0, 0, width, height,
			  AllPlanes, ZPixmap);
    if (shapemask)
	shapeimage = XGetImage(display, shapemask, 0, 0, width, height,
			       AllPlanes, ZPixmap);

    ErrorStatus = XpmCreateXpmImageFromImage(display, image, shapeimage,
					     xpmimage, attributes);
    if (image)
	XDestroyImage(image);
    if (shapeimage)
	XDestroyImage(shapeimage);

    return (ErrorStatus);
}
