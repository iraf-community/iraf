/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "sgiUtil.h"


/*
 *  SGI2GIF.C -- Read an IRAF SGI bitmap file on standard input and convert
 *  to a GIF format image on standard outout.
 *
 *  Usage:
 *		sgi2gif.e [-params] [ [sgi_bitmap] [sgi_bitmap] ... ]
 *
 *	-w N	     width of input bitmap and output image
 *	-h N	     height of input bitmap and output image
 *	-i	     invert the bitmap values before conversion
 *	-t	     set background color as transparent
 *	-root	     set the root rame for output file (default stdout)
 *	-fg R G B    specify foreground color
 *	-bg R G B    specify background color
 *
 *  The input file name and the switches may occur in any order.  The
 *  foreground/background flags require three arguments giving the values
 *  of the RGB components of the color as a decimal number in the range 0-255.
 *  Enabling the transparency flag will cause a GIF 89 image to be written,
 *  otherwise the default will be a GIF 87 format image.  The transparent
 *  color will always be the backgrund color.  The bitmap may be inverted
 *  here using the -i flag.
 *
 *  Sample graphcaps for this translator might look like:
 *
 *      g-gif|UNIX generic interface to multi-frame GIF file generator:\
 *          :DD=ugif,tmp$sgk,!{ sgidispatch sgi2gif -w $(PX) -h $(PY) \
 *          -bg 0 0 0 -fg 255 255 255 -root sgigif $F.[1-8] ; \
 *          rm $F.[1-8]; }&:MF#8:NF:tc=sgi_image_format:
 *
 *      sgi_image_format|Generic raster file format specification:\
 *          :kf=bin$x_sgikern.e:tn=sgikern:ar#.75:\
 *          :xr#640:yr#480:PX#640:PY#480:XW#640:YW#480:\
 *          :BI:MF#1:YF:NB#8:LO#1:LS#0:XO#0:YO#0:
 *
 *  The 'g-gif' entry takes one or more graphics file input and converts
 *  each input frame to a redirected file on output called 'sgigifXXX.gif'
 *  where the 'XXX' is frame number.
 *
 *  To change the image size the graphcap :xr, :PX, :XW (X-dimension) and
 *  :yr, :PY, :XY (Y-dimension) fields all need to be changed.  The -i 
 *  or -t flags must be specified in the graphcap DD string along with the
 *  -fg/bg flags and their arguments.
 */


#define	NBITS_CHAR	8		/* number of bits in a char 	*/
#define DEF_WIDTH	640		/* default image width		*/
#define DEF_HEIGHT	480		/* default image height		*/
#define DEF_BG		255		/* default background RGB	*/
#define DEF_FG		0		/* default foreground RGB	*/
#define	MAX_INFILES	16		/* max number of input bitmaps	*/
#define	SZ_FNAME	64		/* size of a filename		*/

typedef int	    	code_int;
typedef long int	count_int;
typedef unsigned char	byte;

static byte	*pixels;

static int px = DEF_WIDTH;
static int py = DEF_HEIGHT;
static int nrows = DEF_HEIGHT;
static int ncols = DEF_WIDTH;
static int transparent = 0;
static int invert = 0;
static int red[]   = { DEF_BG, DEF_FG } ;
static int green[] = { DEF_BG, DEF_FG } ;
static int blue[]  = { DEF_BG, DEF_FG } ;
static char *infile[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
static char *s_root = "sgigif_";

static int	GIFNextPixel(void);
static void 	BumpPixel(void), GIFEncode(FILE *fp, int GWidth, int GHeight, int GInterlace, int Background, int Bpp, int *Red, int *Green, int *Blue), Putword(int w, FILE *fp), compress(int init_bits, FILE *outfile);
static void 	output(code_int code), cl_block(void), cl_hash(register count_int hsize), char_init(void);
static void 	char_out(int c), flush_char(void), unpack1to8(byte *dest, byte *src, int len);



/* MAIN -- Main entry point for the task.
 */
int
main (int argc, char *argv[])
{
	FILE	*fdi, *fdo;
	char	fname[SZ_FNAME];
	char	*root = s_root;
	byte	*buffer, *ip;
	int	i, index, numin=0, len_buf;
	int	interlace, background, bpp;


        /* Process the command line.
         */
        for (i=1;  i < argc;  i++) {
            if (argv[i][0] == '-') {
		if (strcmp (argv[i], "-w") == 0) {
		    ncols = px = atoi (argv[++i]);
		} else if (strcmp (argv[i], "-h") == 0) {
		    nrows = py = atoi (argv[++i]);
		} else if (strcmp (argv[i], "-i") == 0) {
		    invert++;
		} else if (strcmp (argv[i], "-root") == 0) {
		    root = argv[++i];
		} else if (strcmp (argv[i], "-t") == 0) {
		    transparent++;
		} else if (strcmp (argv[i], "-bg") == 0) {
		    if (isdigit(argv[++i][0]))
		        red[0]   = atoi (argv[i]);
		    else
		        fprintf (stderr,
			    "sgi2gif: invalid -bg arg '%s'\n", argv[i]);
		    if (isdigit(argv[++i][0]))
		        green[0] = atoi (argv[i]);
		    else
		        fprintf (stderr,
			    "sgi2gif: invalid -bg arg '%s'\n", argv[i]);
		    if (isdigit(argv[++i][0]))
		        blue[0]  = atoi (argv[i]);
		    else
		        fprintf (stderr,
			    "sgi2gif: invalid -bg arg '%s'\n", argv[i]);
		} else if (strcmp (argv[i], "-fg") == 0) {
		    if (isdigit(argv[++i][0]))
		        red[1]   = atoi (argv[i]);
		    else
		        fprintf (stderr,
			    "sgi2gif: invalid -bg arg '%s'\n", argv[i]);
		    if (isdigit(argv[++i][0]))
		        green[1] = atoi (argv[i]);
		    else
		        fprintf (stderr,
			    "sgi2gif: invalid -bg arg '%s'\n", argv[i]);
		    if (isdigit(argv[++i][0]))
		        blue[1]  = atoi (argv[i]);
		    else
		        fprintf (stderr,
			    "sgi2gif: invalid -bg arg '%s'\n", argv[i]);
		} else {
		    fprintf (stderr, "sgi2gif: unknown switch '%s'\n", argv[i]);
		}
	    } else {
		/* input sgi-bitmap file specification */
		if (numin < MAX_INFILES)
                    infile[numin++] = argv[i];     
	    }
	}

	/* Allocate space for the images. */
	len_buf = px / NBITS_CHAR;
	buffer = (byte *) malloc (len_buf);
	ip = pixels = (byte *) malloc (px * (py + 1));

	/* Loop over the input bitmaps, writing the converted output to
	 * either stdout or a filename.
	 */
	for (index = 0; index == 0 || index < numin; index++) {

	    /* Open the input file. */
            fdi = (infile[index] ? fopen (infile[index], "r") : stdin);

	    /* Open the output file.  For multiple input files force each
	     * output to a new image, when reading from stdin or only one
	     * bitmap write to stdout if we didn't set the rootname.
	     */
	    if (numin <= 1 && strcmp (root, s_root) == 0) {
		fdo = stdout;
	    } else {
		if (numin > 1)
		    sprintf (fname, "%s%d.gif", root, index);
		else
		    sprintf (fname, "%s.gif", root);
		fdo = fopen (fname, "w+");
	    }

	    /* Now unpack this bitmap to the output image as byte data. */
	    ip = pixels;
	    while (fread (buffer, len_buf, 1, fdi)) {
	        /* If we're on a MSB ordered machine wordswap the bitmap so
		 * it's in the correct order for unpacking to be interpreted
		 * as an LSB-ordered image.
	         */
	        if ( ! isSwapped ())
		    bswap4 (buffer, buffer, len_buf);

	        unpack1to8 ((ip+=px), buffer, px);
	    }

	    /* All set, write it out. */
	    GIFEncode (fdo, px, py, (interlace=0), (background=0), (bpp=1),
		red, green, blue);

	    fflush (fdi);
	    fflush (fdo);
	    if (fdi != stdin)
	        fclose (fdi);
	    if (fdo != stdout)
	        fclose (fdo);
	}

	/* Clean up. */
	free (buffer);
	free (pixels);

	return (0);
}


/* UNPACK1TO8 -- Unpack each bit in the bitmap to a byte on output.
 */

static void
unpack1to8 (byte *dest, byte *src, int len)
{
        register int i, b;
        byte 	c = 0;

        for (i = 0, b = 0; i < len; i++) {
            if (b > 7) {
                b = 0;
                c = (invert ? ~(*src++) : (*src++) );
            }
            *dest++ = (byte) ((c >> (b++)) & 1);
        }
}


/* GIF Writing Procedures.
 *
 * Based on GIFENCOD by David Rowley <mgardi@watdscu.waterloo.edu>.  A
 * Lempel-Zim compression based on "compress". Original Copyright 1990, 
 * David Koblas, heavily modified since then....
 */

#define	GIFBITS			12

static int	Width, Height;
static int	curx, cury;
static long	CountDown;
static int	Interlace;


/* GIFENCODE -- GIF Image compression interface.
 */

static void
GIFEncode (FILE *fp, int GWidth, int GHeight, int GInterlace, int Background, int Bpp, int *Red, int *Green, int *Blue)
{
	int	B;
	int	RWidth, RHeight;
	int	LeftOfs, TopOfs;
	int	Resolution;
	int	ColorMapSize;
	int	InitCodeSize;
	int	i;

	Interlace = GInterlace;

	ColorMapSize = 1 << Bpp;

	RWidth = Width = GWidth;
	RHeight = Height = GHeight;
	LeftOfs = TopOfs = 0;

	Resolution = Bpp;

	/* Calculate number of bits we are expecting */
	CountDown = (long)Width * (long)Height;

	/* The initial code size */
	if (Bpp <= 1)
	    InitCodeSize = 2;
	else
	    InitCodeSize = Bpp;

	/* Set up the current x and y position */
	curx = cury = 0;

	/* Write the Magic header */
	fwrite ((transparent ? "GIF89a" : "GIF87a"), 1, 6, fp);

	/* Write out the screen width and height */
	Putword (RWidth, fp);
	Putword (RHeight, fp);

	/* Indicate that there is a global colour map */
	B = 0x80;       /* Yes, there is a color map */

	/* OR in the resolution */
	B |= (Resolution - 1) << 5;

	/* OR in the Bits per Pixel */
	B |= (Bpp - 1);

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

	/* If doing transparency, write the extension. */
	if (transparent) {
            fputc (0x21, fp);		/* graphics extension... */
            fputc (0xf9, fp);		/* transparency... */
            fputc (0x4, fp);
            fputc (0x1, fp);
            fputc (0x0, fp);
            fputc (0x0, fp);
            fputc ((char) 0, fp);	/* background color index */
            fputc (0x0, fp);
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
BumpPixel(void)
{
	/* Bump the current X position */
	++curx;

	/* If at the end of a scan line, set curx back to the beginning. */
	if (curx == Width) {
	    curx = 0;
	    ++cury;
	}
}


/* Return the next pixel from the image
 */
static int	
GIFNextPixel (void)
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
Putword (int w, FILE *fp)
{
	unsigned short	val = w;

	fputc (val & 0xff, fp);
	fputc ((val / 256) & 0xff, fp);
}


/*
 * GIF Image compression - modified 'compress'
 *
 * Based on: compress.c - File compression ala IEEE Computer, June 1984.
 *
 * By Authors:   Spencer W. Thomas, Jim McKie, Steve Davies, Ken Turkowski,
 *		 James A. Woods, Joe Orost
 *
 *  Lempel-Ziv compression based on 'compress'.  GIF modifications by
 *  David Rowley (mgardi@watdcsu.waterloo.edu)
 */

#define HSIZE  	   5003            		/* 80% occupancy */

static int	n_bits;                    /* number of bits/code */
static int	maxbits = GIFBITS;         /* user settable max # bits/code */
static code_int maxcode;                   /* maximum code, given n_bits */
					   /* should NEVER generate this code */
static code_int maxmaxcode = (code_int) 1 << GIFBITS; 
#define MAXCODE(n_bits)        (((code_int) 1 << (n_bits)) - 1)

static count_int 	htab[HSIZE];
static unsigned short	codetab [HSIZE];
#define HashTabOf(i)    htab[i]
#define CodeTabOf(i)    codetab[i]

/* To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**GIFBITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i)    CodeTabOf(i)
#define tab_suffixof(i)    ((unsigned char *)(htab))[i]

static code_int free_ent = 0;           /* first unused entry 		*/
static code_int hsize = HSIZE;          /* for dynamic table sizing 	*/


/* block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int	clear_flg = 0;

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

static FILE	*g_outfile;
static int	g_init_bits;
static int	ClearCode;
static int	EOFCode;
static int	cur_bits = 0;

static unsigned long	cur_accum = 0;
static unsigned long	masks[] = { 
	    			0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
	    			0x001F, 0x003F, 0x007F, 0x00FF,
	    			0x01FF, 0x03FF, 0x07FF, 0x0FFF,
	    			0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF 	};

static int a_count; 	    /* Number of characters so far in this 'packet'  */
static char accum[256];     /* Define the storage for the packet accumulator */

static void
compress (int init_bits, FILE *outfile)
{
	register long	fcode;
	register code_int i /* = 0 */;
	register int	c;
	register code_int ent;
	register code_int disp;
	register code_int hsize_reg;
	register int	hshift;

	/* Set up the globals:  g_init_bits - initial number of bits
         *                      g_outfile   - pointer to output file
         */
	g_init_bits = init_bits;
	g_outfile = outfile;

	/* Set up the necessary values */
	clear_flg = 0;
	cur_accum = 0;
	cur_bits = 0;
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
	output ((code_int) EOFCode);
}

/*
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
output (code_int code)
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
		perror ("error writing output file");
	}
}

/*
 * Clear out the hash table
 */
static void
cl_block (void)             /* table clear for block compress */
{

	cl_hash  ((count_int) hsize);
	free_ent = ClearCode + 2;
	clear_flg = 1;

	output ((code_int)ClearCode);
}

static void
cl_hash(register count_int hsize)          /* reset code table */
                         
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

/* Set up the 'byte output' routine
 */
static void
char_init(void)
{
	register int i;

	a_count = 0;
	for (i=0; i<256; i++)
	    accum[i] = 0;
}

/* Add a character to the end of the current packet, and if it is 254
 * characters, flush the packet to disk.
 */
static void
char_out (int c)
{
	accum[ a_count++ ] = c;
	if (a_count >= 254)
	    flush_char();
}

/* Flush the packet to disk, and reset the accumulator */
static void
flush_char(void)
{
    	if (a_count > 0) {
    	    fputc (a_count, g_outfile);
    	    fwrite (accum, 1, a_count, g_outfile);
    	    a_count = 0;
    	}
}
