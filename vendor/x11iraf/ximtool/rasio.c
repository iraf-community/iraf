#include <stdio.h>
#include <math.h>
#include <ctype.h>

/*
 * RASIO.C -- Routines to load and save Sun rasterfiles.
 *
 *  ival = isSunRas (fname)
 *	 loadSunRas (fname, pixels, pixtype, w,h, r,g,b, ncolors, colorstyle)
 *      writeSunRas (fp, pixels, pixtype, w,h, r,g,b, ncolors, colorstyle)
 *     getSunRasHdr (fname)
 *
 * isSunRas returns nonzero if the named file is a Sun rasterfile.
 * loadSunRas reads a Sun rasterfile and returns the decoded pixel array and
 * 8 bit colormap if any.  The caller is responsible for freeing the pixels
 * buffer.  writeSunRas performs the converse operation, writing the given
 * pixel array and colormap to the output Sun rasterfile.
 */

#define	DEBUG	0

/* MONO returns total intensity of r,g,b components */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)  /*.33R+ .5G+ .17B*/

/*
 * Info on sun rasterfiles taken from rasterfile(5) man page.
 * ------------------------------------------------------------
 */

#define RAS_MAGIC 0x59a66a95

struct rasterfile {
	long	ras_magic;
	long	ras_width;
	long	ras_height;
	long	ras_depth;
	long	ras_length;
	long	ras_type;
	long	ras_maptype;
	long	ras_maplength;
};

static char	errstr[128];

#define RT_OLD          0       /* Raw pixrect image in 68000 byte order */
#define RT_STANDARD     1       /* Raw pixrect image in 68000 byte order */
#define RT_BYTE_ENCODED 2       /* Run-length compression of bytes */
#define RT_FORMAT_RGB   3       /* XRGB or RGB instead of XBGR or BGR */

#define RMT_RAW		2
#define RMT_NONE	0
#define RMT_EQUAL_RGB	1

#define RAS_RLE 0x80


#ifdef __STDC__
static char *sunRasError(char *, char *);
static int  rle_read(unsigned char *, int, int, FILE *, int);
static void SunRas1to8(unsigned char *, unsigned char *, int);
static void SunRas8to1(unsigned char *, unsigned char *, int, int);
static int  read_sun_long(long *, FILE *);
static int  write_sun_long(long, FILE *);
static void fixBGR(unsigned char *, int, int);
#else
static char *sunRasError();
static int  rle_read();
static void SunRas1to8();
static void SunRas8to1();
static int  read_sun_long(), write_sun_long();
static void fixBGR();
#endif


/*
 * Public routines.
 * ----------------
 */


/* LoadSunRas -- Load the Sun rasterfile.  The Sun rasterfile is input in
 * the file fname and the decoded pixel array and colormap are output in
 * the output variables PIXELS and R/G/B.  The caller is responsible for
 * freeing PIXELS when it is no longer needed.  The RGB arrays should be
 * contain space for at least 256 elements.
 */
char *
loadSunRas (fname, pixels, pixtype, o_w,o_h, r,g,b, ncolors, colorstyle)
char 	*fname;				/* input filename                    */
unsigned char **pixels;			/* output pixels                     */
int 	*pixtype;			/* 8-bit or 24-bit                   */
int 	*o_w, *o_h;			/* dimensions                        */
unsigned char *r, *g, *b;		/* colormap                          */
int 	*ncolors;			/* number of colors                  */
int	colorstyle;			/* return color (0) or grayscale (1) */
{
	register FILE *fp;
	register int i, j;
	int linesize, lsize, csize, isize, w, h, d, nc, ct;
	unsigned char *image, *line, p;
	struct rasterfile sunhdr;

	/* Read in the Sun Rasterfile picture. */
	fp = fopen (fname, "r");
	if (!fp) 
	    return (sunRasError(fname, "unable to open file"));

	read_sun_long (&sunhdr.ras_magic, fp);
	read_sun_long (&sunhdr.ras_width, fp);
	read_sun_long (&sunhdr.ras_height, fp);
	read_sun_long (&sunhdr.ras_depth, fp);
	read_sun_long (&sunhdr.ras_length, fp);
	read_sun_long (&sunhdr.ras_type, fp);
	read_sun_long (&sunhdr.ras_maptype, fp);
	read_sun_long (&sunhdr.ras_maplength, fp);

	if (sunhdr.ras_magic != RAS_MAGIC) {
	    fclose (fp);
	    return (sunRasError(fname, "not a Sun rasterfile"));
	}

	/* Make sure that the input picture can be dealt with. */
	if (sunhdr.ras_depth != 1 && 
	    sunhdr.ras_depth != 8 && 
	    sunhdr.ras_depth != 24 && 
	    sunhdr.ras_depth != 32) {

	    fprintf (stderr, "Sun rasterfile image has depth %d\n", 
	        sunhdr.ras_depth);
	    fprintf (stderr, "Depths supported are 1, 8, 24, and 32\n");
	    fclose (fp);
	    return (sunRasError(fname, "Unsupported rasterfile depth"));
	}

	if (sunhdr.ras_type != RT_OLD && 
	    sunhdr.ras_type != RT_STANDARD && 
	    sunhdr.ras_type != RT_BYTE_ENCODED && 
	    sunhdr.ras_type != RT_FORMAT_RGB) {

	    fprintf (stderr, "Sun rasterfile of unsupported type %d\n",
	        sunhdr.ras_type);
	    fclose (fp);
	    return (sunRasError(fname, "Unsupported rasterfile type"));
	}

	if (sunhdr.ras_maptype != RMT_RAW && 
	    sunhdr.ras_maptype != RMT_NONE && 
	    sunhdr.ras_maptype != RMT_EQUAL_RGB) {

	    fprintf (stderr, "Sun rasterfile colormap of unsupported type %d\n",
	        sunhdr.ras_maptype);
	    fclose (fp);
	    return (sunRasError(fname, "Unsupported rasterfile colormap"));
	}

	w = sunhdr.ras_width;
	h = sunhdr.ras_height;
	d = sunhdr.ras_depth;
	isize = sunhdr.ras_length ? sunhdr.ras_length : (w * h * d) / 8;
	csize = (sunhdr.ras_maptype == RMT_NONE) ? 0 : sunhdr.ras_maplength;

	/* Compute length of the output image. */
	lsize = w * h;
	if (d == 24 || d == 32) 
	    lsize = lsize * 3;

	linesize = w * d;
	if (linesize % 16) 
	    linesize += (16 - (linesize % 16));
	linesize /= 8;

	if (DEBUG) {
	    fprintf (stderr, "LoadSunRas() - loading a %dx%d pic, %d planes\n",
		w, h, d);
	    fprintf (stderr, 
	    "type %d, maptype %d, isize %d, csize %d, lsize %d, linesize %d\n",
		sunhdr.ras_type, sunhdr.ras_maptype,
		isize, csize, lsize, linesize);
	    fprintf (stderr, "colorstyle=%d nc=%d\n", colorstyle,
		sunhdr.ras_maplength/3);
	}

	/* Read in the colormap, if any. */
	if (sunhdr.ras_maptype == RMT_EQUAL_RGB && csize) {
	    nc = sunhdr.ras_maplength / 3;
	    ct = 8;
	    fread (r, 1, nc, fp);
	    fread (g, 1, nc, fp);
	    fread (b, 1, nc, fp);
	} else if (sunhdr.ras_maptype == RMT_RAW && csize) {
	    /* We don't know how to handle raw colormap, ignore. */
	    fseek (fp, (long) csize, 1);
	    nc = ct = 0;
	} else {
	    /* No colormap, make one up. */
	    if (sunhdr.ras_depth == 1) {
	    	r[0] = g[0] = b[0] = 0;
	    	r[1] = g[1] = b[1] = 255;
		nc = 2;
		ct = 1;
	    } else if (sunhdr.ras_depth == 8) {
	    	for (i=0, nc=256;  i < nc;  i++)
	    	    r[i] = g[i] = b[i] = i;
		ct = 8;
	    }
	}

	/* Allocate memory for picture and read it in.  Note we may slightly
	 * overallocate here (if image is padded)
	 */
	image = (unsigned char *) malloc (lsize);
	line = (unsigned char *) malloc (linesize);
	if (!image || !line) {
	    fclose (fp);
	    return (sunRasError (fname, "out of memory"));
	}

	for (i=0;  i < h;  i++) {
	    if (sunhdr.ras_type == RT_BYTE_ENCODED) {
	    	if (rle_read (line, 1, linesize, fp, (i == 0)) != linesize) 
		    break;
	    } else {
	    	if (fread (line, 1, linesize, fp) != linesize) {
		    free ((char *)image);  
		    free ((char *)line);  
		    fclose (fp);
		    return (sunRasError (fname, "file read error"));
	    	}
	    }

	    switch (d) {
	    case 1:  
	    	SunRas1to8 (image + w * i, line, w);	
	    	break;
	    case 8:  
		if (colorstyle) {
		    for (j=0; j < w; j++) {
			p = line[j];
			image[w*i + j] = MONO(r[p],g[p],b[p]);
		    }
		} else
	    	    bcopy(line, image + w * i, w);		
	    	break;
	    case 24: 
	    	bcopy(line, image + w * i * 3, w * 3); 
	    	break;
	    case 32: 
	    	{
	    	    int	k;
	    	    unsigned char *ip, *op;

	    	    ip = line;
	    	    op = (unsigned char *) (image + w * i * 3);
	    	    for (k = 0; k < w; k++) {
	    	    	*ip++;           /* skip 'alpha' */
	    	    	*op++ = *ip++;   /* red   */
	    	    	*op++ = *ip++;   /* green */
	    	    	*op++ = *ip++;   /* blue  */
	    	    }
	    	}
	    }
	}
	free ((char *)line);

	if (d == 24 || d == 32) {
	    if (sunhdr.ras_type != RT_FORMAT_RGB) 
	    	fixBGR(image, w, h);
	    *pixtype = 24;
	} else 
	    *pixtype = 8;

	*pixels = (unsigned char *) image;
	*o_w = w;
	*o_h = h;

	/* If we requested grayscale reset the colormap. */
	if (colorstyle)
	    for (i=0, nc=256;  i < nc;  i++)
	        r[i] = g[i] = b[i] = i;
	*ncolors = nc;

	fclose (fp);
	return (NULL);
}


/* WriteSunRas -- Write a pixel array and colormap to a file in Sun rasterfile
 * format.  Writes a sun rasterfile to the already open stream.  Writes either
 * 24-bit, 8-bit or 1-bit.  Currently will not write RLE files.  If PIC24 and
 * F_GREYSCALE, writes an 8-bit grayscale image.
 */
int
writeSunRas (fp, pixels, pixtype, w,h, r,g,b, ncolors, colorstyle)
FILE *fp;
unsigned char *pixels;
int pixtype, w, h;
unsigned char *r, *g, *b;
int ncolors, colorstyle;
{
	unsigned char *line, *graypic, graymap[256], *sp, *dp;
	int linesize, i, color, d, y, flipbw;
	struct rasterfile sunhdr;

	/* Biggest problem w/ RLE file: should we compute image size first
	 * (nicer) or go back and write it in when we are done (kludgy)?
	 */
	graypic = NULL;

	/* Special case: if PIC24 and writing GREYSCALE, write 8-bit file.
	 */
	if (pixtype == 24  && colorstyle == 1) {
	    graypic = (unsigned char *) malloc (w * h);
	    for (i=0, sp=pixels, dp=graypic;  i < w * h;  i++, sp += 3, dp++) {
	    	*dp = MONO(sp[0], sp[1], sp[2]);
	    }

	    for (i = 0; i < 256; i++) 
	    	graymap[i] = i;
	    r = g = b = graymap;
	    ncolors = 256;
	    pixtype = 8;
	    pixels = graypic;
	}

	if (pixtype == 24) { 
	    d = 24;  
	    linesize = w * 3; 
	} else if (colorstyle != 0) { 
	    d = 8;   
	    linesize = w;     
	} else {
	    d = 1;
	    linesize = w;
	    if (linesize % 8) 
	    	linesize += (8 - linesize % 8);
	    linesize /= 8;
	}

	if (linesize % 2) 
	    linesize++;
	line = (unsigned char *) malloc (linesize);
	if (!line) {
	    if (graypic) 
	    	free ((char *) graypic);
	    return (1);
	}

	if (DEBUG)
	    fprintf (stderr,
	        "WriteSunRas: d %d, linesize %d ncolors %d\n",
	        d, linesize, ncolors);

	/* Set flipbw if color#0 is black. */
	if (d == 1)
	    flipbw = (MONO(r[0], g[0], b[0]) < MONO(r[1], g[1], b[1]));

	/* Set up the header.
	 */
	sunhdr.ras_magic = RAS_MAGIC;
	sunhdr.ras_width = w;
	sunhdr.ras_height = h;
	sunhdr.ras_depth = d;
	sunhdr.ras_length = linesize * h;
	sunhdr.ras_type	= RT_STANDARD;
	sunhdr.ras_maptype = (d == 1 || d == 24) ? RMT_NONE : RMT_EQUAL_RGB;
	sunhdr.ras_maplength = (d == 1 || d == 24) ? 0 : 3 * ncolors;

	write_sun_long (sunhdr.ras_magic, fp);
	write_sun_long (sunhdr.ras_width, fp);
	write_sun_long (sunhdr.ras_height, fp);
	write_sun_long (sunhdr.ras_depth, fp);
	write_sun_long (sunhdr.ras_length, fp);
	write_sun_long (sunhdr.ras_type, fp);
	write_sun_long (sunhdr.ras_maptype, fp);
	write_sun_long (sunhdr.ras_maplength, fp);

	/* Write the colormap.
	 */
	if (d == 8) {
	    if (colorstyle == 1) {
		/* grayscale */
	    	for (color = 0; color < 3; color++)
	    	    for (i = 0; i < ncolors; i++)
	    	    	putc (MONO(r[i], g[i], b[i]), fp);
	    } else {
	    	fwrite ((char *)r, sizeof(char), ncolors, fp);
	    	fwrite ((char *)g, sizeof(char), ncolors, fp);
	    	fwrite ((char *)b, sizeof(char), ncolors, fp);
	    }
	}

	/* Write the image.
	 */
	line[linesize-1] = 0;
	for (y = 0; y < h; y++) {
	    if (d == 24) {
	    	unsigned char *lptr, *pix;

	    	pix = pixels + y * w * 3;
	    	for (i=0, lptr=line; i < w; i++, pix += 3) {
	    	    *lptr++ = pix[2];          /* write data out in BGR order */
	    	    *lptr++ = pix[1];
	    	    *lptr++ = pix[0];
	    	}
	    } else if (d == 8) {
	    	bcopy (pixels + y * w, line, w);
	    } else {
		 /* d == 1 */
	    	SunRas8to1 (line, pixels + y * w, w, flipbw);
	    }

	    if (fwrite ((char *)line, sizeof(char), linesize, fp) != linesize) {
	    	if (graypic) 
	    	    free ((char *)graypic);
	    	free ((char *)line);
	    	return (2);
	    }
	}

	free ((char *)line);
	if (graypic) 
	    free ((char *)graypic);
	return (0);
}


/* IsSunRas -- Test a file to see if it is a Sun rasterfile.
 */
isSunRas (fname)
char *fname;				/* input filename */
{
	register FILE *fp;
	struct rasterfile sunhdr;
	int value = 0;

	if (fp = fopen (fname, "r")) {
	    read_sun_long (&sunhdr.ras_magic, fp);
	    value = (sunhdr.ras_magic == RAS_MAGIC);
	    fclose (fp);
	}

	return (value);
}


/* getSunRasHdr -- Get some set of header information for the GUI.
 */

char *
getSunRasHdr (fname)
char    *fname;
{
        FILE    *fp;
        char 	*line;
        struct rasterfile hdr;

        /* Open the image. */
        fp = fopen (fname, "r");
        if (!fp) 
            return NULL;

	/* Read the image header. */
        read_sun_long (&hdr.ras_magic, fp);
        read_sun_long (&hdr.ras_width, fp);
        read_sun_long (&hdr.ras_height, fp);
        read_sun_long (&hdr.ras_depth, fp);
        read_sun_long (&hdr.ras_length, fp);
        read_sun_long (&hdr.ras_type, fp);
        read_sun_long (&hdr.ras_maptype, fp);
        read_sun_long (&hdr.ras_maplength, fp);

	/* Format the description. */
        line = (char *) malloc (80);
        sprintf (line, "%-16.16s  %3d  %5dx%-5d  %s %s",
            fname, hdr.ras_depth, hdr.ras_width, hdr.ras_height, 
	    "Sun Rasterfile",
	    ((((hdr.ras_type == RT_OLD) ? "(OLD)" : 
	       (hdr.ras_type == RT_STANDARD) ? "(Standard)" : 
	       (hdr.ras_type == RT_BYTE_ENCODED) ? "(Byte-Encoded)" : 
	       (hdr.ras_type == RT_FORMAT_RGB) ? "(RGB)" : " "))) ); 

	fclose (fp);
        return (line);
}


/*
 * Internal routines.
 * -------------------
 */

static int
rle_read (ptr, size, nitems, fp, init)
register unsigned char *ptr;
int size, nitems, init;
FILE *fp;
{
	static int count, ch;
	int readbytes, c, read;

	if (init)
	    count = ch = 0; 

	readbytes = size * nitems;
	for (read = 0; read < readbytes; read++) {
	    if (count) {
	    	*ptr++ = (unsigned char) ch;
	    	count--;
	    } else {
	    	c = getc (fp);
	    	if (c == EOF) 
		    break;

	    	if (c == RAS_RLE) {   /* 0x80 */
		    count = getc(fp);
		    if (count == EOF) 
			break;

		    if (count < 0) 
			count &= 0xff;
		    if (count == 0) 
			*ptr++ = (unsigned char) c;
		    else {
			if ((ch = getc(fp)) == EOF) 
			    break;
			*ptr++ = (unsigned char) ch;
		    }
	    	} else 
		    *ptr++ = (unsigned char) c;
	    }
	}

	return (read / size);
}


static char *
sunRasError (fname, st)
char *fname, *st;
{
	sprintf (errstr, "%s: %s\n", fname, st);
	return (errstr);
}


static void
SunRas1to8 (dest, src, len)
unsigned char *dest, *src;
int len;
{
	register int i, b;
	int c = 0;

	for (i = 0, b = -1; i < len; i++) {
	    if (b < 0) {
	    	b = 7;
	    	c = ~(*src++);
	    }
	    *dest++ = (unsigned char)((c >> (b--)) & 1);
	}
}


static void
SunRas8to1 (dest, src, len, flip)
unsigned char *dest, *src;
int len, flip;
{
	int i, b;
	int c;

	for (c = b = i = 0; i < len; i++) {
	    c <<= 1;
	    c |= (*src++ ? 1 : 0);
	    if (b++ == 7) {
	    	if (flip) 
		    c = ~c;
	    	*dest++ = (unsigned char) (c & 0xff);
	    	b = c = 0;
	    }
	}
	if (b) {
	    if (flip) 
	    	c = ~c;
	    *dest = (unsigned char) ((c << (8 - b)) & 0xff);
	}
}


/* Reads a 4-byte int in Sun byteorder.
 * Returns 0 for success, EOF for failure.
 */
static int
read_sun_long (l, fp)
long *l;
FILE *fp;
{
	int c0, c1, c2, c3;

	c0 = fgetc(fp);
	c1 = fgetc(fp);
	c2 = fgetc(fp);
	c3 = fgetc(fp);

	*l = (((unsigned long) c0 & 0xff) << 24) | 
	    (((unsigned long) c1 & 0xff) << 16) | 
	    (((unsigned long) c2 & 0xff) <<  8) | 
	    (((unsigned long) c3 & 0xff));

	if (ferror(fp)) 
	    return EOF;

	return (0);
}


/* Write a long word in sun byte-order.
 * Returns 0 for success, EOF for failure.
 */
static int
write_sun_long (l, fp)
long l;
FILE *fp;
{
	char c;

	c = ((l >> 24) & 0xff);
	if (putc (c, fp) == EOF) 
	    return (EOF);
	c = ((l >> 16) & 0xff);
	if (putc (c, fp) == EOF) 
	    return (EOF);
	c = ((l >> 8) & 0xff);
	if (putc (c, fp) == EOF) 
	    return (EOF);
	c = (l & 0xff);
	if (putc (c, fp) == EOF) 
	    return (EOF);
	return (0);
}


/* kr3 - fix up BGR order SUN 24-bit rasters to be RGB order
 */
static void
fixBGR (img, w, h)
unsigned char *img;
int w, h;
{
	int i, npixels;
	unsigned char	tmp;

	npixels = w * h;
	for (i = 0; i < npixels; i++) {
	    tmp = img[0];                   /* swap red and blue channels */
	    img[0] = img[2];
	    img[2] = tmp;
	    img += 3;                       /* bump to next pixel */
	}
}
