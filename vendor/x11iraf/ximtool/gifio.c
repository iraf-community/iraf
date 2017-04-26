/*
 * GIFIO.C -- Routines to load and save simple GIF files.
 *
 *  ival = isGIF (fname)
 *       loadGIF (fname, pixels, pixtype, w,h, r,g,b, ncolors)
 *      writeGIF (fp, pixels, pixtype, w,h, r,g,b, ncolors, gray)

 * LoadGIF(fname, numcols)  -  loads a GIF file
 * WriteGIF(fp, pic, w, h, rmap, gmap, bmap, numcols)
 *
 * isGIF    -- returns nonzero if the named file is a GIF file.
 * loadGIF  -- reads a GIF file and returns the decoded pixel array and gray-
 *	       scale 8 bit colormap.  The caller is responsible for freeing
 * 	       the pixels buffer.  
 * writeGIF -- performs the converse operation, writing the given pixel array
 * 	       and colormap to the output GIF file.
 *
 * Based on GIFENCOD by David Rowley <mgardi@watdscu.waterloo.edu>.A
 * Lempel-Zim compression based on "compress".
 */

#include <stdio.h>
#include <math.h>


/* a code_int must be able to hold 2**GIFBITS values of type int, and also -1
 */
typedef int	    	code_int;
typedef long int	count_int;
typedef unsigned char	byte;
#ifndef AIXV3
#ifndef OSF1
typedef unsigned char	uchar;
#endif
#endif

#define MAXCOLORS 256
#define	TRUE	  1
#define	FALSE	  0

#define CM_RED			0
#define CM_GREEN		1
#define CM_BLUE			2

#define	GIFBITS			12

#define INTERLACE		0x40
#define LOCALCOLORMAP		0x80

#define BitSet(byte, bit)	(((byte) & (bit)) == (bit))
#define	ReadOK(file,buffer,len)	(fread(buffer, len, 1, file) != 0)
#define LM_to_uint(a,b)		    (((b)<<8)|(a))

static struct {
	unsigned int	Width;
	unsigned int	Height;
	unsigned int	BitPixel;
	unsigned int	ColorResolution;
	unsigned int	Background;
	unsigned int	AspectRatio;
} GifScreen;

static struct {
	int	transparent;
	int	delayTime;
	int	inputFlag;
	int	disposal;
} Gif89 = { 
	-1, -1, -1, 0 };



/* MONO returns total intensity of r,g,b components */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)  /*.33R+ .5G+ .17B*/


/* Function prototypes */
#ifdef __STDC__

#include <stddef.h>
#include <stdlib.h>

static int colorstobpp (int colors);
static int GetPixel (int x, int y);
static void BumpPixel (void);
static int GIFNextPixel ();
static void GIFEncode (FILE* fp, int GWidth, int GHeight, int GInterlace, 
          int Background, int BitsPerPixel, int Red[], int Green[], int Blue[]);
static void Putword (int w, FILE* fp);
static void compress (int init_bits, FILE* outfile);
static void output (code_int code);
static void cl_block (void);
static void cl_hash (count_int hsize);
static void writeerr (void);
static void char_init (void);
static void char_out (int c);
static void flush_char (void);

static char *ReadGIF (FILE *fd, int imageNumber, uchar **pix, int *nx, int *ny,
	uchar *r, uchar *g, uchar *b, int *ncolors);
static int  ReadColorMap (FILE *fd, int number, uchar *r, uchar *g, uchar *b );
static int  DoExtension (FILE *fd, int label );
static int  GetDataBlock (FILE *fd, uchar  *buf );
static int  GetCode (FILE *fd, int code_size, int flag );
static int  LWZReadByte (FILE *fd, int flag, int input_code_size );
static char *ReadImage (FILE *fd, int len, int height, 
	int interlace, int ignore, uchar **image );

#else
static int	colorstobpp(), GIFNextPixel();
static void 	BumpPixel (), GIFEncode (), Putword (), compress ();
static void 	output (), cl_block (), cl_hash (), writeerr (), char_init ();
static void 	char_out (), flush_char ();

static char 	*ReadGIF (), *ReadImage ();
static int  	ReadColorMap (), DoExtension (), GetDataBlock ();
static int  	GetCode (), LWZReadByte ();
#endif

static byte	*pixels;
static int	nrows, ncols;
static char	*errstr = NULL;


/* ----------------
 * Public routines.
 * ----------------*/


/* loadGIF - Load a GIF file.
 */

char	*
loadGIF  (fname, pix, nx, ny, r,g,b, ncolors, gray)
char    *fname;                         /* input filename       */
uchar   **pix;                          /* output pixels        */
int     *nx, *ny;                       /* dimensions           */
uchar   *r, *g, *b;                     /* colormap             */
int     *ncolors;                       /* number of colors     */
int	gray;				/* read as a grayscale? */
{
	register FILE *fd;
	register int i, imageNumber = 1;

        if ((fd = fopen(fname, "rb")) == NULL)
            return "Unable to open FITS file";
 
        for (i=0; i < 256;  i++)
            r[i] = g[i] = b[i] = 0;

	errstr = ReadGIF(fd, imageNumber, pix, nx, ny, r, g, b, ncolors);

        /* If we requested grayscale reset the colormap and pixels. */
        if (gray)
            for (i=0; i < 256;  i++)
                r[i] = g[i] = b[i] = MONO(r[i],g[i],b[i]);

	fclose (fd);
	return (errstr);
}


/* writeGIF -- Write a GIF image.
 */

int
writeGIF (fp, image, w, h, rmap, gmap, bmap, numcols, gray)
FILE 	*fp;				/* output file descriptor */
byte 	*image;				/* image pixels 	  */
int	w, h;				/* image dimensions	  */
byte 	*rmap, *gmap, *bmap;		/* colormap		  */
int	numcols;			/* number of colors       */
int	gray;				/* save as grayscale?	  */
{
	register int	i, bpp, npix;
	int	 r[MAXCOLORS], g[MAXCOLORS], b[MAXCOLORS];

	/* Change type of the colormap. */
	if (gray) 
	    for (i = 0; i < MAXCOLORS; i++) 
	        r[i] = g[i] = b[i] = (int) MONO(rmap[i],gmap[i],bmap[i]);
	else
	    for (i = 0; i < MAXCOLORS; i++) {
	        r[i] = (int) rmap[i];
	        g[i] = (int) gmap[i];
	        b[i] = (int) bmap[i];
	    }
	bpp = colorstobpp (numcols);

	/* Load the pixel buffer */
	npix = w * h;
	ncols = w;
	nrows = h;
	pixels = (byte *) malloc (npix * sizeof(byte));
	for (i = 0; i < npix; i++)
	    pixels[i] = image[i];

	/* All set, write it out. */
	GIFEncode (fp, w, h, 0, 0, bpp, r, g, b);

	/* Clean up. */
	free ((byte *)pixels);
}


/* IsGIF -- Test a file to see if it is a GIF file.
 */
int	
isGIF (fname)
char	*fname;	    	    /* input filename */
{
	register FILE *fp;
	int	value = 0;
	char	tag[5];

        if (fp = fopen (fname, "r")) {
            fread (tag, sizeof(char), 4, fp);
            if (strncmp ("GIF8", tag, 4) == 0)
                value = 1;
            fclose (fp);
        }
	return value;
}


/* getGIFHdr -- Get some set of header information for the GUI.
 */

char *
getGIFHdr (fname)
char    *fname;
{
        FILE    *fp;
        char    *line;
        uchar   buf[16], version[4];
	int	nx, ny, ncolors;


        /* Open the image. */
        fp = fopen (fname, "r");
        if (!fp)
            return NULL;

	/* Get the version. */
        if (!ReadOK(fp, buf, 6) || strncmp(buf, "GIF", 3) != 0)
            return NULL;
        strncpy(version, buf + 3, 3);
        version[3] = '\0';


        /* Read the image header. */
        if (!ReadOK(fp, buf, 7))
            return NULL;
        nx      = LM_to_uint(buf[0], buf[1]);
        ny      = LM_to_uint(buf[2], buf[3]);
        ncolors = 2 << (buf[4] & 0x07);

        /* Format the description. */
        line = (char *) malloc (80);
        sprintf (line, "%-16.16s   8   %5dx%-5d  GIF%s Image (%d colors)",
            fname, nx, ny, version, ncolors);

        fclose (fp);
        return (line);
}



/* ------------------
 * Private Procedures
 * ------------------*/


/* COLORSTOBPP -- Convert the numbers of colors we're writing to a
 * bits-per-pixel value.
 */
static int	
colorstobpp (colors)
int	colors;
{
	int	bpp;

	if (colors <= 2)
	    bpp = 1;
	else if (colors <= 4)
	    bpp = 2;
	else if (colors <= 8)
	    bpp = 3;
	else if (colors <= 16)
	    bpp = 4;
	else if (colors <= 32)
	    bpp = 5;
	else if (colors <= 64)
	    bpp = 6;
	else if (colors <= 128)
	    bpp = 7;
	else if (colors <= 256)
	    bpp = 8;
	else
	    perror ("can't happen");

	return bpp;
}



/*----------------------
 * GIF Input Procedures
 *----------------------*/


/* +-------------------------------------------------------------------+
 * | Copyright 1990, David Koblas.                                     |
 * |   Permission to use, copy, modify, and distribute this software   |
 * |   and its documentation for any purpose and without fee is hereby |
 * |   granted, provided that the above copyright notice appear in all |
 * |   copies and that both that copyright notice and this permission  |
 * |   notice appear in supporting documentation.  This software is    |
 * |   provided "as is" without express or implied warranty.           |
 * +-------------------------------------------------------------------+
 */


static char *
ReadGIF(fd, imageNumber, pix, nx, ny, r, g, b, ncolors)
FILE	*fd;
int	imageNumber;
uchar	**pix;
int	*nx, *ny;
uchar	*r, *g, *b;
int	*ncolors;
{
	uchar	buf[16];
	uchar	c;
	int	useGlobalColormap;
	int	bitPixel;
	int	imageCount = 0;
	char	version[4];

	if (!ReadOK(fd, buf, 6))
	    return "error reading magic number";

	if (strncmp(buf, "GIF", 3) != 0)
	    return "not a GIF file";

	strncpy(version, buf + 3, 3);
	version[3] = '\0';

	if ((strcmp(version, "87a") != 0) && (strcmp(version, "89a") != 0))
	    return "bad version number, not '87a' or '89a'";

	if (!ReadOK(fd, buf, 7))
	    return "failed to read screen descriptor";

	*nx = GifScreen.Width          = LM_to_uint(buf[0], buf[1]);
	*ny = GifScreen.Height         = LM_to_uint(buf[2], buf[3]);
	*ncolors = GifScreen.BitPixel  = 2 << (buf[4] & 0x07);
	GifScreen.ColorResolution      = (((buf[4] & 0x70) >> 3) + 1);
	GifScreen.Background           = buf[5];
	GifScreen.AspectRatio          = buf[6];

	if (BitSet(buf[4], LOCALCOLORMAP)) {	/* Global Colormap */
	    if (ReadColorMap(fd, *ncolors, r, g, b))
		return "error reading global colormap";
	}

	if (GifScreen.AspectRatio != 0 && GifScreen.AspectRatio != 49) {
	    float	r;
	    r = ( (float) GifScreen.AspectRatio + 15.0 ) / 64.0;
	}

	for (; ; ) {
	    if (!ReadOK(fd, &c, 1))
		return "EOF / read error on image data";

	    if (c == ';') {		/* GIF terminator */
		if (imageCount < imageNumber)
		    return "requested image number not found";
		return;
	    }

	    if (c == '!') { 	/* Extension */
		if (!ReadOK(fd, &c, 1))
		    return "OF / read error on extention function code";
		DoExtension(fd, c);
		continue;
	    }

	    if (c != ',') 		/* Not a valid start character */
		continue;

	    ++imageCount;

	    if (!ReadOK(fd, buf, 9))
		return "couldn't read left/top/width/height";

	    useGlobalColormap = !BitSet(buf[8], LOCALCOLORMAP);

	    bitPixel = 1 << ((buf[8] & 0x07) + 1);

	    if (!useGlobalColormap) {
		if (ReadColorMap(fd, bitPixel, r, g, b))
		    return "error reading local colormap";
		return (ReadImage(fd, LM_to_uint(buf[4], buf[5]),
		    LM_to_uint(buf[6], buf[7]), BitSet(buf[8], INTERLACE), 
		    imageCount != imageNumber, pix ));
	    } else {
		return (ReadImage(fd, LM_to_uint(buf[4], buf[5]),
		    LM_to_uint(buf[6], buf[7]), BitSet(buf[8], INTERLACE), 
		    imageCount != imageNumber, pix ));
	    }

	}
}


static int	
ReadColorMap(fd, number, r, g, b)
FILE		*fd;
int	number;
uchar	*r, *g, *b;
{
	int	i;
	uchar	rgb[3];

	for (i = 0; i < number; ++i) {
	    if (!ReadOK(fd, rgb, sizeof(rgb)))
		return TRUE;

	    r[i] = rgb[0] ;
	    g[i] = rgb[1] ;
	    b[i] = rgb[2] ;
	}
	return FALSE;
}


static int	
DoExtension(fd, label)
FILE	*fd;
int	label;
{
	static char	buf[256];
	char	*str;

	switch (label) {
	case 0x01:		/* Plain Text Extension */
	    str = "Plain Text Extension";
	    break;
	case 0xff:		/* Application Extension */
	    str = "Application Extension";
	    break;
	case 0xfe:		/* Comment Extension */
	    str = "Comment Extension";
	    while (GetDataBlock(fd, (uchar * ) buf) != 0) 
		;
	    return FALSE;
	case 0xf9:		/* Graphic Control Extension */
	    str = "Graphic Control Extension";
	    (void) GetDataBlock(fd, (uchar * ) buf);
	    Gif89.disposal    = (buf[0] >> 2) & 0x7;
	    Gif89.inputFlag   = (buf[0] >> 1) & 0x1;
	    Gif89.delayTime   = LM_to_uint(buf[1], buf[2]);
	    if ((buf[0] & 0x1) != 0)
		Gif89.transparent = buf[3];

	    while (GetDataBlock(fd, (uchar * ) buf) != 0)
		;
	    return FALSE;
	default:
	    str = buf;
	    sprintf(buf, "UNKNOWN (0x%02x)", label);
	    break;
	}

	while (GetDataBlock(fd, (uchar * ) buf) != 0) ;

	return FALSE;
}


static int ZeroDataBlock = FALSE;

static int	
GetDataBlock(fd, buf)
FILE		*fd;
uchar	*buf;
{
	uchar	count;

	if (!ReadOK(fd, &count, 1))
	    return - 1;

	ZeroDataBlock = count == 0;

	if ((count != 0) && (!ReadOK(fd, buf, count)))
	    return - 1;

	return count;
}


static int	
GetCode(fd, code_size, flag)
FILE	*fd;
int	code_size;
int	flag;
{
	static uchar	buf[280];
	static int	curbit, lastbit, done, last_byte;
	int	i, j, ret;
	uchar	count;

	if (flag) {
	    curbit = 0;
	    lastbit = 0;
	    done = FALSE;
	    return 0;
	}

	if ( (curbit + code_size) >= lastbit) {
	    if (done)
		return - 1;
	    buf[0] = buf[last_byte-2];
	    buf[1] = buf[last_byte-1];

	    if ((count = GetDataBlock(fd, &buf[2])) == 0)
		done = TRUE;

	    last_byte = 2 + count;
	    curbit = (curbit - lastbit) + 16;
	    lastbit = (2 + count) * 8 ;
	}

	ret = 0;
	for (i = curbit, j = 0; j < code_size; ++i, ++j)
	    ret |= ((buf[ i / 8 ] & (1 << (i % 8))) != 0) << j;

	curbit += code_size;

	return ret;
}


static int	
LWZReadByte(fd, flag, input_code_size)
FILE	*fd;
int	flag;
int	input_code_size;
{
	static int	fresh = FALSE;
	int	code, incode;
	static int	code_size, set_code_size;
	static int	max_code, max_code_size;
	static int	firstcode, oldcode;
	static int	clear_code, end_code;
	static int	table[2][(1<< GIFBITS)];
	static int	stack[(1<<(GIFBITS))*2], *sp;
	register int	i;

	if (flag) {
	    set_code_size = input_code_size;
	    code_size = set_code_size + 1;
	    clear_code = 1 << set_code_size ;
	    end_code = clear_code + 1;
	    max_code_size = 2 * clear_code;
	    max_code = clear_code + 2;

	    GetCode(fd, 0, TRUE);

	    fresh = TRUE;

	    for (i = 0; i < clear_code; ++i) {
		table[0][i] = 0;
		table[1][i] = i;
	    }
	    for (; i < (1 << GIFBITS); ++i)
		table[0][i] = table[1][0] = 0;

	    sp = stack;

	    return 0;
	} else if (fresh) {
	    fresh = FALSE;
	    do {
		firstcode = oldcode = 
		    GetCode(fd, code_size, FALSE);
	    } while (firstcode == clear_code);
	    return firstcode;
	}

	if (sp > stack)
	    return * --sp;

	while ((code = GetCode(fd, code_size, FALSE)) >= 0) {
	    if (code == clear_code) {
		for (i = 0; i < clear_code; ++i) {
		    table[0][i] = 0;
		    table[1][i] = i;
		}
		for (; i < (1 << GIFBITS); ++i)
		    table[0][i] = table[1][i] = 0;
		code_size = set_code_size + 1;
		max_code_size = 2 * clear_code;
		max_code = clear_code + 2;
		sp = stack;
		firstcode = oldcode = 
		    GetCode(fd, code_size, FALSE);
		return firstcode;
	    } else if (code == end_code) {
		int	count;
		uchar	buf[260];

		if (ZeroDataBlock)
		    return - 2;

		while ((count = GetDataBlock(fd, buf)) > 0)
		    ;

		return - 2;
	    }

	    incode = code;

	    if (code >= max_code) {
		*sp++ = firstcode;
		code = oldcode;
	    }

	    while (code >= clear_code) {
		*sp++ = table[1][code];
		code = table[0][code];
	    }

	    *sp++ = firstcode = table[1][code];

	    if ((code = max_code) < (1 << GIFBITS)) {
		table[0][code] = oldcode;
		table[1][code] = firstcode;
		++max_code;
		if ((max_code >= max_code_size) && 
		    (max_code_size < (1 << GIFBITS))) {
		    max_code_size *= 2;
		    ++code_size;
		}
	    }

	    oldcode = incode;

	    if (sp > stack)
		return * --sp;
	}
	return code;
}


static char *
ReadImage(fd, len, height, interlace, ignore, image)
FILE	*fd;
int	len, height;
int	interlace, ignore;
uchar	**image;
{
	uchar	c;
	int	v;
	int	xpos = 0, ypos = 0, pass = 0;

	/* Initialize the Compression routines */
	if (!ReadOK(fd, &c, 1))
	    return "EOF / read error on image data";

	if (LWZReadByte(fd, TRUE, c) < 0)
	    return "error reading image";

	/* If this is an "uninteresting picture" ignore it.  */
	if (ignore) {
	    while (LWZReadByte(fd, FALSE, c) >= 0)
		;
	    return;
	}

	if ((*image = (uchar *)malloc(len * height)) == NULL)
	    return "couldn't alloc space for image";

	while ((v = LWZReadByte(fd, FALSE, c)) >= 0 ) {
	    (*image)[ypos * len + xpos] = v;

	    ++xpos;
	    if (xpos == len) {
		xpos = 0;
		if (interlace) {
		    switch (pass) {
		    case 0:
		    case 1:
		    	ypos += 8; 
		    	break;
		    case 2:
		    	ypos += 4; 
		    	break;
		    case 3:
		    	ypos += 2; 
		    	break;
		    }

		    if (ypos >= height) {
		    	++pass;
		    	switch (pass) {
		    	case 1:
		    	    ypos = 4; 
		    	    break;
		    	case 2:
		    	    ypos = 2; 
		    	    break;
		    	case 3:
		    	    ypos = 1; 
		    	    break;
		    	default:
		    	    goto fini;
		    	}
		    }
		} else {
		    ++ypos;
		}
	    }
	    if (ypos >= height)
		break;
	}

fini:
	return NULL;
}



/*-----------------------
 * GIF Output Procedures
 *-----------------------*/

/*****************************************************************************
 *
 * GIFENCODE.C    - GIF Image compression interface
 *
 * GIFEncode (FName, GHeight, GWidth, GInterlace, Background,
 *            BitsPerPixel, Red, Green, Blue)
 *
 *****************************************************************************/

#define TRUE 1
#define FALSE 0

static int	Width, Height;
static int	curx, cury;
static long	CountDown;
static int	Pass = 0;
static int	Interlace;



static void
GIFEncode (fp, GWidth, GHeight, GInterlace, Background,
BitsPerPixel, Red, Green, Blue)

FILE*fp;
int	GWidth, GHeight;
int	GInterlace;
int	Background;
int	BitsPerPixel;
int	Red[], Green[], Blue[];
{
	int	B;
	int	RWidth, RHeight;
	int	LeftOfs, TopOfs;
	int	Resolution;
	int	ColorMapSize;
	int	InitCodeSize;
	int	i;

	Interlace = GInterlace;

	ColorMapSize = 1 << BitsPerPixel;

	RWidth = Width = GWidth;
	RHeight = Height = GHeight;
	LeftOfs = TopOfs = 0;

	Resolution = BitsPerPixel;

	/* Calculate number of bits we are expecting */
	CountDown = (long)Width * (long)Height;

	/* Indicate which pass we are on (if interlace) */
	Pass = 0;

	/* The initial code size */
	if (BitsPerPixel <= 1)
	    InitCodeSize = 2;
	else
	    InitCodeSize = BitsPerPixel;

	/* Set up the current x and y position */
	curx = cury = 0;

	/* Write the Magic header */
	fwrite ("GIF87a", 1, 6, fp);

	/* Write out the screen width and height */
	Putword (RWidth, fp);
	Putword (RHeight, fp);

	/* Indicate that there is a global colour map */
	B = 0x80;       /* Yes, there is a color map */

	/* OR in the resolution */
	B |= (Resolution - 1) << 5;

	/* OR in the Bits per Pixel */
	B |= (BitsPerPixel - 1);

	/* Write it out */
	fputc (B, fp);

	/* Write out the Background colour */
	fputc (Background, fp);

	/* Byte of 0's (future expansion) */
	fputc (0, fp);

	/* Write out the Global Colour Map */
	for  (i = 0; i < ColorMapSize; ++i) {
	    fputc (Red[i], fp);
	    fputc (Green[i], fp);
	    fputc (Blue[i], fp);
	}

	/* Write an Image separator */
	fputc (',', fp);

	/* Write the Image header */
	Putword (LeftOfs, fp);
	Putword (TopOfs, fp);
	Putword (Width, fp);
	Putword (Height, fp);

	/* Write out whether or not the image is interlaced */
	if (Interlace)
	    fputc (0x40, fp);
	else
	    fputc (0x00, fp);

	/* Write out the initial code size */
	fputc (InitCodeSize, fp);

	/* Go and actually compress the data */
	compress (InitCodeSize + 1, fp);

	/* Write out a Zero-length packet (to end the series) */
	fputc (0, fp);

	/* Write the GIF file terminator */
	fputc (';', fp);
}


/* Bump the 'curx' and 'cury' to point to the next pixel
 */
static void
BumpPixel()
{
	/* Bump the current X position */
	++curx;

	/* If we are at the end of a scan line, set curx back to the beginning
         * If we are interlaced, bump the cury to the appropriate spot,
         * otherwise, just increment it.
         */
	if (curx == Width) {
	    curx = 0;

	    if (!Interlace)
	    	++cury;
	    else {
	    	switch  (Pass) {
	    	case 0:
	    	    cury += 8;
	    	    if (cury >= Height) {
	    	    	++Pass;
	    	    	cury = 4;
	    	    }
	    	    break;
	    	case 1:
	    	    cury += 8;
	    	    if (cury >= Height) {
	    	    	++Pass;
	    	    	cury = 2;
	    	    }
	    	    break;
	    	case 2:
	    	    cury += 4;
	    	    if (cury >= Height) {
	    	    	++Pass;
	    	    	cury = 1;
	    	    }
	    	    break;
	    	case 3:
	    	    cury += 2;
	    	    break;
	    	}
	    }
	}
}


/* Return the next pixel from the image
 */
static int	
GIFNextPixel ()
{
	int	r;

	if (CountDown == 0)
	    return EOF;

	--CountDown;
	r = (int) pixels[ cury * ncols + curx ] ;
	BumpPixel();
	return r;
}


/* Write out a word to the GIF file
 */
static void
Putword (w, fp)
int	w;
FILE*fp;
{
	fputc (w & 0xff, fp);
	fputc ((w / 256) & 0xff, fp);
}


/***************************************************************************
 *
 *  GIFCOMPR.C       - GIF Image compression routines
 *
 *  Lempel-Ziv compression based on 'compress'.  GIF modifications by
 *  David Rowley (mgardi@watdcsu.waterloo.edu)
 *
 ***************************************************************************/

/*
 * General DEFINEs
 */

#define GIFBITS    12
#define HSIZE  	   5003            		/* 80% occupancy */

#ifdef NO_UCHAR
typedef char		char_type;
#else /*NO_UCHAR*/
typedef unsigned char	char_type;
#endif /*NO_UCHAR*/

/*
 *
 * GIF Image compression - modified 'compress'
 *
 * Based on: compress.c - File compression ala IEEE Computer, June 1984.
 *
 * By Authors:  Spencer W. Thomas       (decvax!harpo!utah-cs!utah-gr!thomas)
 *              Jim McKie               (decvax!mcvax!jim)
 *              Steve Davies            (decvax!vax135!petsd!peora!srd)
 *              Ken Turkowski           (decvax!decwrl!turtlevax!ken)
 *              James A. Woods          (decvax!ihnp4!ames!jaw)
 *              Joe Orost               (decvax!vax135!petsd!joe)
 *
 */
#include <ctype.h>

#define ARGVAL() (*++(*argv) || (--argc && *++argv))

static int	n_bits;                    /* number of bits/code */
static int	maxbits = GIFBITS;         /* user settable max # bits/code */
static code_int maxcode;                   /* maximum code, given n_bits */
					   /* should NEVER generate this code */
static code_int maxmaxcode = (code_int) 1 << GIFBITS; 
#ifdef COMPATIBLE               /* But wrong! */
# define MAXCODE(n_bits)        ((code_int) 1 << (n_bits) - 1)
#else /*COMPATIBLE*/
# define MAXCODE(n_bits)        (((code_int) 1 << (n_bits)) - 1)
#endif /*COMPATIBLE*/

static count_int htab [HSIZE];
static unsigned short	codetab [HSIZE];
#define HashTabOf(i)       htab[i]
#define CodeTabOf(i)    codetab[i]

/*static*/ code_int hsize = HSIZE;                 /* for dynamic table sizing */

/* To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**GIFBITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i) CodeTabOf(i)
#define tab_suffixof(i)        ((char_type*)(htab))[i]
#define de_stack               ((char_type*)&tab_suffixof((code_int)1<<GIFBITS))

static code_int free_ent = 0;                  /* first unused entry */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int	clear_flg = 0;

static int	offset;
static long int	in_count = 1;            /* length of input */
static long int	out_count = 0;           /* # of codes output (for debugging) */

/*
 * compress stdin to stdout
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

static int	g_init_bits;
static FILE	*g_outfile;

static int	ClearCode;
static int	EOFCode;

static unsigned long	cur_accum = 0;
static int		cur_bits = 0;
static unsigned long	masks[] = { 
	    0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
	    0x001F, 0x003F, 0x007F, 0x00FF,
	    0x01FF, 0x03FF, 0x07FF, 0x0FFF,
	    0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF 	};

static int a_count; 	     /* Number of characters so far in this 'packet' */
static char accum[ 256 ];    /* Define the storage for the packet accumulator */

static void
compress (init_bits, outfile)
int	init_bits;
FILE*outfile;
{
	register long	fcode;
	register code_int i /* = 0 */;
	register int	c;
	register code_int ent;
	register code_int disp;
	register code_int hsize_reg;
	register int	hshift;

	/*
         * Set up the globals:  g_init_bits - initial number of bits
         *                      g_outfile   - pointer to output file
         */
	g_init_bits = init_bits;
	g_outfile = outfile;

	/*
         * Set up the necessary values
         */
	offset = 0;
	out_count = 0;
	clear_flg = 0;
	cur_accum = 0;
	cur_bits = 0;
	in_count = 1;
	maxbits = GIFBITS;
	maxcode = MAXCODE(n_bits = g_init_bits);

	ClearCode = (1 << (init_bits - 1));
	EOFCode = ClearCode + 1;
	free_ent = ClearCode + 2;

	char_init();
	for (i=0; i<HSIZE; i++) {
	    htab[i] = 0;
	    codetab[i] = 0;
	}

	ent = GIFNextPixel ();

	hshift = 0;
	for  (fcode = (long) hsize; fcode < 65536L; fcode *= 2L)
	    ++hshift;
	hshift = 8 - hshift;                /* set hash code range bound */

	hsize_reg = hsize;
	cl_hash ((count_int) hsize_reg);            /* clear hash table */

	output ((code_int)ClearCode);

	while  ((c = GIFNextPixel ()) != EOF) {

	    ++in_count;

	    fcode = (long) (((long) c << maxbits) + ent);
	    i = (((code_int)c << hshift) ^ ent);    /* xor hashing */

	    if (HashTabOf (i) == fcode) {
	    	ent = CodeTabOf (i);
	    	continue;
	    } else if ((long)HashTabOf (i) < 0)      /* empty slot */
	    	goto nomatch;
	    disp = hsize_reg - i;         /* secondary hash (after G. Knott) */
	    if (i == 0)
	    	disp = 1;
probe:
	    if ((i -= disp) < 0)
	    	i += hsize_reg;

	    if (HashTabOf (i) == fcode) {
	    	ent = CodeTabOf (i);
	    	continue;
	    }
	    if ((long)HashTabOf (i) > 0)
	    	goto probe;
nomatch:
	    output  ((code_int) ent);
	    ++out_count;
	    ent = c;
	    if (free_ent < maxmaxcode) {	/* } */
	        CodeTabOf (i) = free_ent++; /* code -> hashtable */
	        HashTabOf (i) = fcode;
	    } else
	        cl_block();
	}

	/*
	 * Put out the final code.
	 */
	output ((code_int)ent);
	++out_count;
	output ((code_int) EOFCode);
}

/*****************************************************************
 * TAG (output)
 *
 * Output the given code.
 * Inputs:
 *      code:   A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *              that n_bits =< (long)wordsize - 1.
 * Outputs:
 *      Outputs code to the file.
 * Assumptions:
 *      Chars are 8 bits long.
 * Algorithm:
 *      Maintain a GIFBITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */


static void
output (code)
code_int  code;
{
	cur_accum &= masks[ cur_bits ];

	if (cur_bits > 0)
	    cur_accum |= ((long)code << cur_bits);
	else
	    cur_accum = code;

	cur_bits += n_bits;

	while  (cur_bits >= 8) {
	    char_out ((unsigned int)(cur_accum & 0xff));
	    cur_accum >>= 8;
	    cur_bits -= 8;
	}

   	/*
         * If the next entry is going to be too big for the code size,
         * then increase it, if possible.
         */
	if (free_ent > maxcode || clear_flg) {

	    if (clear_flg) {
	    	maxcode = MAXCODE (n_bits = g_init_bits);
	    	clear_flg = 0;
	    } else {
	        ++n_bits;
	        if (n_bits == maxbits)
	    	    maxcode = maxmaxcode;
	        else
	    	    maxcode = MAXCODE(n_bits);
	    }
	}

	if (code == EOFCode) {
	    /* At EOF, write the rest of the buffer.  */
	    while  (cur_bits > 0) {
	        char_out ((unsigned int)(cur_accum & 0xff));
	        cur_accum >>= 8;
	        cur_bits -= 8;
	    }
	    flush_char();
	    fflush (g_outfile);
	    if (ferror (g_outfile))
	      writeerr();
	}
}


/*
 * Clear out the hash table
 */
static void
cl_block ()             /* table clear for block compress */
{

	cl_hash  ((count_int) hsize);
	free_ent = ClearCode + 2;
	clear_flg = 1;

	output ((code_int)ClearCode);
}

static void
cl_hash(hsize)          /* reset code table */
register count_int hsize;
{

	register count_int *htab_p = htab + hsize;

	register long	i;
	register long	m1 = -1;

	i = hsize - 16;
	do {                            /* might use Sys V memset(3) here */
	    *(htab_p - 16) = m1;
	    *(htab_p - 15) = m1;
	    *(htab_p - 14) = m1;
	    *(htab_p - 13) = m1;
	    *(htab_p - 12) = m1;
	    *(htab_p - 11) = m1;
	    *(htab_p - 10) = m1;
	    *(htab_p -  9) = m1;
	    *(htab_p -  8) = m1;
	    *(htab_p -  7) = m1;
	    *(htab_p -  6) = m1;
	    *(htab_p -  5) = m1;
	    *(htab_p -  4) = m1;
	    *(htab_p -  3) = m1;
	    *(htab_p -  2) = m1;
	    *(htab_p -  1) = m1;
	    htab_p -= 16;
	} while ((i -= 16) >= 0);

	for  (i += 16; i > 0; --i)
	    *--htab_p = m1;
}

static void
writeerr()
{
	perror ("error writing output file");
}


/******************************************************************************
 *
 * GIF Specific routines
 *
 ******************************************************************************/


/* Set up the 'byte output' routine
 */
static void
char_init()
{
	register int i;

	a_count = 0;
	for (i=0; i<256; i++)
	    accum[i] = 0;
}


/*
 * Add a character to the end of the current packet, and if it is 254
 * characters, flush the packet to disk.
 */
static void
char_out (c)
int	c;
{
	accum[ a_count++ ] = c;
	if (a_count >= 254)
	    flush_char();
}

/*
 * Flush the packet to disk, and reset the accumulator
 */
static void
flush_char()
{
    	if (a_count > 0) {
    	    fputc (a_count, g_outfile);
    	    fwrite (accum, 1, a_count, g_outfile);
    	    a_count = 0;
    	}
}
