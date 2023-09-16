/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <suntool/sunview.h>
#include <suntool/fullscreen.h>
#include <pixrect/pr_planegroups.h>
#include <stdio.h>
#include <pwd.h>

#define	SEGSIZE		16		/* output segment size (bytes)	*/
#define	SEGBITS		(SEGSIZE*8)
#define	BITFLIP		0		/* bit-flip each byte for P.S.?	*/
#define	NGREY		256		/* max color table size		*/
#define	PAGE_WIDTH	2550		/* 8.5x11, 300 dpi		*/
#define	PAGE_HEIGHT	3300		/* 8.5x11, 300 dpi		*/
#define	PAGE_XOFFSET	0		/* offset to drawing area	*/
#define	PAGE_YOFFSET	0		/* offset to drawing area	*/
#define	MARGIN		150		/* 1/2 inch margin		*/
#define	MAXGROUPS	(PIXPG_OVERLAY+1)

#define	RT_DATA		'a'		/* record type codes		*/
#define	RT_ZERO		'b'
#define	RT_FULL		'c'
#define	RT_BKG1		'd'
#define	RT_BKG2		'e'
#define	BKGPAT_1	"22"		/* atom for stipple pattern	*/
#define	BKGPAT_2	"88"		/* atom for stipple pattern	*/

/* The following are provided by the calling program and specify what type
 * of output is desired.
 */
extern	int  r_type;			/* 0=postscript, 1=rasterfile */
extern	char r_dispose[];		/* dispose command */
extern	char r_filename[];		/* output file template */

int	gt_bitflip_postscript = BITFLIP;
static	unsigned char	red[NGREY], green[NGREY], blue[NGREY];
static	void bitmap_to_postscript();
static	char *make_label();


/* SCREENDUMP -- Make a hardcopy of the indicated region of the screen on a
 * hardcopy device.  Currently only two output formats are supported, Sun
 * rasterfile output, or Postscript.  A dispose command may be given to
 * postprocess the output file, e.g., send it to the printer.
 */
screendump (win_fd, pw, width, height, left, top, nbits_out)
int	win_fd;			/* window fd, for bell */
struct	pixwin *pw;		/* arbitrary pixwin, used to lock display */
int	width, height;		/* region to be printed: size, */
int	left, top;		/* origin */
int	nbits_out;		/* output 1 bit or 8 bit postscript image? */
{
	register int	v, i, j;
	register unsigned char	*ip, *op;
	unsigned char	*pr_data, *obuf, *zero;

	struct	rect pw_r;
	struct	timeval tv_bell;
	struct	pixrect *screen, *s_pr, *o_pr, *m_pr;
	static	int filenum = 0;

	float	scale, xs, ys;
	int	overlay, ngroups;
	char	groups[MAXGROUPS];
	char	tempfile[80], dispose[80];
	int	depth, cache_v, cache_greyval, rasterout;
	int	status=0, bit, byte, pr_linebytes, ob_linebytes, fd; 
	char	*str, *getenv();
	FILE	*fp;

	/* Open the hardware frame buffer, create a memory pixrect to hold user
	 * specified rect, lock the display and read in the data and colormap.
	 * The Sun 3-110, 3/60, etc., have separate monochrome and color
	 * planes plus an overlay-enable plane, whereas the older frame buffers
	 * have only a single monochrome or color plane.
	 */
	screen = pr_open ("/dev/fb");

	depth = screen->pr_depth;
	ngroups = pr_available_plane_groups (screen, MAXGROUPS, groups);
	overlay = (ngroups >= PIXPG_OVERLAY);

	/* Get memory pixrects to hold frame buffer data. */
	zero = (unsigned char *) malloc (height);
	s_pr = mem_create (width, height, depth);
	if (overlay) {
	    o_pr = mem_create (width, height, 1);
	    m_pr = mem_create (width, height, 1);
	}

	/* Lock the frame buffer to avoid edits during readout. */
	pw_get_region_rect (pw, &pw_r);
	pw_lock (pw, &pw_r);

	/* Get the color map and the main frame buffer pixrect. */
	pr_getcolormap (screen, 0, NGREY, red, green, blue);
	pr_set_plane_group (s_pr, PIXPG_8BIT_COLOR);
	pr_rop (s_pr, 0, 0, width, height, PIX_SRC, screen, left, top);

	/* If the device has an overlay plane, readout it out as well as the
	 * enable plane.
	 */
	if (overlay) {
	    int    o_pg;
	    o_pg = pr_get_plane_group (screen);
	    pr_set_plane_group (screen, PIXPG_OVERLAY);
	    pr_rop (o_pr,0,0, width, height,
		PIX_SRC, screen, left, top);
	    pr_set_plane_group (screen, PIXPG_OVERLAY_ENABLE);
	    pr_rop (m_pr,0,0, width, height,
		PIX_SRC, screen, left, top);
	    pr_set_plane_group (screen, o_pg);
	}

	pw_unlock (pw);
	pr_close (screen);

	/* Combine the color plane and overlay plane to produce a single
	 * color plane.  Use only those overlay plane pixels which have their
	 * bit set in the enable plane.
	 */
	if (overlay) {
	    pr_stencil (s_pr,0,0, width, height,
		PIX_COLOR(255) | PIX_SRC | PIX_DONTCLIP, m_pr,0,0, o_pr,0,0);
	    pr_close (m_pr);
	    pr_close (o_pr);
	}

	/* Output can be either Postscript or a Sun rasterfile.  Rasterfile
	 * output is handled here.
	 */
	if (rasterout = (r_type && r_filename[0])) {
	    colormap_t cmap;

	    /* Setup colormap descriptor. */
	    cmap.type = RMT_EQUAL_RGB;
	    cmap.length = NGREY;
	    cmap.map[0] = red;
	    cmap.map[1] = green;
	    cmap.map[2] = blue;

	    /* Open raster file. */
	    sprintf (tempfile, r_filename, filenum++);
	    if ((fp = fopen (tempfile, "w")) == NULL) {
		fprintf (stderr, "cannot create %s\n", tempfile);
		return (-1);
	    }

	    pr_dump (s_pr, fp, &cmap, RT_STANDARD, 0);

	    fclose (fp);
	    pr_close (s_pr);
	    goto dispose_;
	}

	/* If the frame buffer is only 1 bit deep we can set obuf to point
	 * to the pixrect data and we are done.  Otherwise we must process
	 * the image pixels through the color table and convert the output
	 * into a monochrome pixrect.  NOTE: the bits may need to be flipped
	 * in the monochrome pixrect to satisfy Postscript; I wasn't sure.
	 */
	if (depth == 1) {
	    /* This option is currently untested. */
	    obuf = (unsigned char *) mpr_d(s_pr)->md_image;
	    ob_linebytes = mpr_d(s_pr)->md_linebytes;

	    if (gt_bitflip_postscript) {
		unsigned char flip[256];

		/* Set up lookup table. */
		for (j=0;  j < 256;  j++) {
		    for (v=0, i=0;  i < 8;  i++)
			v |= (((j >> i) & 1) << (7-i));
		    flip[j] = v;
		}
		    
		/* Bitflip and set the zero-line vector. */
		for (j=0;  j < height;  j++) {
		    v = 1;
		    for (op=obuf+j*ob_linebytes, i=ob_linebytes;  --i >= 0;  ) {
			if (v && *op)
			    v = 0;
			*op++ = flip[*op];
		    }
		    zero[j] = v;
		}
	    } else {
		/* Set the zero-line vector for the pixrect. */
		for (j=0;  j < height;  j++) {
		    v = 1;
		    for (op=obuf+j*ob_linebytes, i=ob_linebytes;  --i >= 0;  ) {
			if (v && *op) {
			    v = 0;
			    break;
			}
		    }
		    zero[j] = v;
		}
	    }

	} else if (nbits_out == 1) {
	    ob_linebytes = (width + 7) / 8;
	    obuf = (unsigned char *) calloc (ob_linebytes * height, 1);
	    if (obuf == NULL) {
		fprintf (stderr, "out of memory\n");
		return (-1);
	    }
	    pr_data = (unsigned char *) mpr_d(s_pr)->md_image;
	    pr_linebytes = mpr_d(s_pr)->md_linebytes;

	    for (j=0, cache_v=(-1);  j < height;  j++) {
		ip = pr_data + j * pr_linebytes;
		op = obuf    + j * ob_linebytes;

		for (byte=(-1), i=0;  i < width;  i++) {
		    if ((v = ip[i]) == cache_v)
			v = cache_greyval;
		    else {
			cache_v = v;
			v = cache_greyval = (red[v] + green[v] + blue[v]) / 3;
		    }
		    if (v <= NGREY/2) {
			byte = i / 8;
			bit  = 8 - (i % 8) - 1;
			op[byte] |= (1 << bit);
		    }
		}

		/* Set flag if entire line is zero. */
		zero[j] = (byte < 0);
	    }

	    pr_close (s_pr);

	} else if (nbits_out == 8 && depth == 8) {
	    /* Eight bits out; transform the image in place in the input
	     * pixrect to save memory.
	     */
	    obuf = pr_data = (unsigned char *) mpr_d(s_pr)->md_image;
	    ob_linebytes = pr_linebytes = mpr_d(s_pr)->md_linebytes;

	    for (j=0, cache_v=(-1);  j < height;  j++) {
		ip = pr_data + j * pr_linebytes;
		op = obuf    + j * ob_linebytes;

		for (i=0;  i < width;  i++) {
		    if ((v = ip[i]) == cache_v)
			v = cache_greyval;
		    else {
			cache_v = v;
			v = cache_greyval = (red[v] + green[v] + blue[v]) / 3;
		    }
		    op[i] = v;
		}

		/* Set flag if entire line is zero. */
		zero[j] = 0;
	    }

	} else {
	    fprintf (stderr, "can only create 1 bit or 8 bit output image\n");
	    return (-1);
	}

	/* Create the output file to hold postscript program.  If no filename
	 * has been specified create a unique file in /tmp.
	 */
	if (!r_filename[0]) {
	    strcpy (tempfile, "/tmp/psXXXXXX");
	    if ((fd = mkstemp (tempfile)) == -1) {
		fprintf (stderr, "cannot create temporary file %s\n", tempfile);
		return (-1);
	    } else
		fp = fdopen (fd, "a");
	} else {
	    sprintf (tempfile, r_filename, filenum++);
	    if ((fp = fopen (tempfile, "w")) == NULL) {
		fprintf (stderr, "cannot create %s\n", tempfile);
		return (-1);
	    }
	}

	/* Scale to fit output page.  */
	xs = (PAGE_WIDTH  - MARGIN*2) / (float)width;
	ys = (PAGE_HEIGHT - MARGIN*2) / (float)height;
	scale = (xs < ys) ? xs : ys;

	/* Translate the bitmap into a postscript program. */
	bitmap_to_postscript (fp,
	    obuf, width, height, nbits_out, ob_linebytes, zero, scale);

	free ((char *)zero);
	if (depth == 1 || (depth == 8 && nbits_out == 8))
	    pr_close (s_pr);
	else
	    free ((char *)obuf);

	fclose (fp);
	close (fd);

	/* Dispose of tempfile to the printer.  We leave it up to the dispose
	 * command to delete the temporary file when finished.
	 */
dispose_:
	if (r_dispose[0]) {
	    sprintf (dispose, r_dispose, tempfile);
	    if ((status = system (dispose)) != 0)
		fprintf (stderr, "screendump: exit status %d\n", status);
	}

	/* Flash the screen to signal the user that we are done. */
	tv_bell.tv_usec = 0*1000;  tv_bell.tv_sec = 0;
	win_bell (win_fd, tv_bell, pw);

	return (status);
}


/* BITMAP_TO_POSTSCRIPT -- Translate a memory bitmap into a postscript program
 * using image compression where regions of the image are all zeroes.  This is
 * done as follows: [1] lines of the bitmap are divided into segments of N
 * bytes, [2] if all N bytes are zero a single zero byte is transmitted,
 * otherwise a byte with the value one is transmitted, followed by N bytes of 
 * literal data.  Lines which are entirely zero are not transmitted at all.
 * The goal is to significantly reduce the amount of data to be pushed through
 * the laserwriter serial interface while keeping things simple enough that
 * postscript will hopefully be able to process the bitmap efficiently.
 *
 * NOTE: Postscript is supposed to be able to copy bitmaps directly without
 * any transformations if all the right conditions are met, e.g., unitary
 * matrices, pixrect resolution matches device resolution, etc.  We do not
 * make use of this here due to the great volume of data which would have to
 * pushed through the laserwriter serial interface at 9600 baud to transmit
 * a fully resolved bitmap.  If a parallel interface were available, e.g.,
 * if the laserwriter is on the ethernet, then this would be the way to go.
 */
static void
bitmap_to_postscript (fp, bitmap, width, height, depth, linebytes, zero, scale)
register FILE *fp;
unsigned char *bitmap;
int	width, height, depth;
int	linebytes;
unsigned char *zero;
float	scale;
{
	register unsigned char *ip;
	register char	*op, *hp;
	register int	n;
	unsigned char	*segp;
	char	hbuf[NGREY*2];
	char	obuf[SEGSIZE*2];
	char	rt_full[SEGSIZE*2+1];
	char	bkg_1[SEGSIZE*2+1];
	char	bkg_2[SEGSIZE*2+1];
	int	partseg, seg, nsegs, allzeroes, i, j, last_j;

	/* Initialize the hbuf array, which contains the hex encoded
	 * representations of the NGREY possible binary byte values.
	 */
	for (n=0, op=hbuf;  n < NGREY;  n++) {
	    i = ((n >> 4) & 017);
	    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
	    i = (n & 017);
	    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
	}

	/* Set up the background (stipple) pattern arrays, used to represent
	 * the Sunview background pattern outside of windows.
	 */
	for (op=bkg_1, hp=BKGPAT_1, n=SEGSIZE;  --n >= 0;  ) {
	    *op++ = hp[0];
	    *op++ = hp[1];
	}   *op++ = '\0';
	for (op=bkg_2, hp=BKGPAT_2, n=SEGSIZE;  --n >= 0;  ) {
	    *op++ = hp[0];
	    *op++ = hp[1];
	}   *op++ = '\0';

	/* RT_FULL is a solid line, another common pattern. */
	for (op=rt_full, n=SEGSIZE*2;  --n >= 0;  )
	    *op++ = 'F';
	*op++ = '\0';

	/* Initialize obuf, in case a partseg call causes the full buffer to
	 * be written out before the garbage elements at the end have been
	 * initialized to legal values.
	 */
	bcopy (rt_full, obuf, SEGSIZE*2);

	/* Define the postscript necessary to receive and output the lines
	 * of the pixrect with image compression.
	 */
	fprintf (fp, "%%! GTERM screendump\n");
	fprintf (fp, "erasepage initgraphics\n");

	/* fprintf (fp, "[%6.3f 0 0 %6.3f 2350 3180] setmatrix\n",
	    -scale, -scale); */
        fprintf (fp, "initmatrix\n");
        fprintf (fp, "%6.3f 72 mul 300 div\n", -scale);
        fprintf (fp, "%6.3f 72 mul 300 div scale\n", scale);
        fprintf (fp, "%f %f translate\n", 2409/(-scale), (-88)/(-scale));

	fprintf (fp, "%d %d translate\n", PAGE_XOFFSET, PAGE_YOFFSET);
	fprintf (fp, "/r_data %d string def\n", SEGSIZE);
	fprintf (fp, "/r_zero %d string def\n", SEGSIZE);
	fprintf (fp, "/r_full %d string def\n", SEGSIZE);
	fprintf (fp, "/r_bkg1 %d string def\n", SEGSIZE);
	fprintf (fp, "/r_bkg2 %d string def\n", SEGSIZE);
	fprintf (fp, "currentfile r_full readhexstring %s\n", rt_full);
	fprintf (fp, "currentfile r_bkg1 readhexstring %s\n", bkg_1);
	fprintf (fp, "currentfile r_bkg2 readhexstring %s\n", bkg_2);
	fprintf (fp, "clear\n");

	if (depth == 8) {
	    fprintf (fp,
		"/dline {0 exch translate %d %d 8 matrix\n", width, 1);
	} else {
	    fprintf (fp,
		"/dline {0 exch translate %d %d true matrix\n", width, 1);
	}

	fprintf (fp, " { currentfile read pop dup %d eq\n", RT_DATA);
	fprintf (fp, "     { pop currentfile r_data readhexstring pop }\n");
	fprintf (fp, "     { dup %d eq\n", RT_ZERO);
	fprintf (fp, "       { pop r_zero }\n");
	fprintf (fp, "       { dup %d eq\n", RT_FULL);
	fprintf (fp, "         { pop r_full }\n");
	fprintf (fp, "         { %d eq\n", RT_BKG1);
	fprintf (fp, "           { r_bkg1 }\n");
	fprintf (fp, "           { r_bkg2 }\n");
	fprintf (fp, "           ifelse }\n");
	fprintf (fp, "         ifelse }\n");
	fprintf (fp, "       ifelse }\n");
	fprintf (fp, "     ifelse\n");

	if (depth == 8)
	    fprintf (fp, " } image} def\n");
	else
	    fprintf (fp, " } imagemask} def\n");

	nsegs = width / (SEGBITS / depth);
	partseg = linebytes - (nsegs * SEGSIZE);

	/* Output successive lines of the pixrect.  All zero lines are omitted
	 * and data compression is used for large regions of zeroes embedded
	 * within a line.
	 */
	for (j=0, last_j=0;  j < height;  j++) {
	    if (zero[j])
		continue;

	    fprintf (fp, "\n%d dline\n", j - last_j);
	    last_j = j;

	    /* Output an integral number of line segments in hexstring format,
	     * i.e., two hex digits output per binary input byte.
	     */
	    segp = bitmap + j*linebytes;
	    for (seg=0;  seg < nsegs;  seg++, segp += SEGSIZE) {
		/* Quick scan of the data to see if it is all zeroes. */
		allzeroes = 1;
		for (ip=segp, n=SEGSIZE;  --n >= 0;  )
		    if (*ip++) {
			allzeroes = 0;
			break;
		    }

		if (allzeroes) {
		    putc (RT_ZERO, fp);
		} else {
		    /* Encode the data segment in hex format. */
		    for (ip=segp, op=obuf, n=SEGSIZE;  --n >= 0;  ) {
			hp = hbuf + (*ip++ * 2);
			*op++ = *hp++;
			*op++ = *hp++;
		    }

		    if (obuf[0] == rt_full[0] &&
			strncmp (obuf, rt_full, SEGSIZE*2) == 0) {
			putc (RT_FULL, fp);
		    } else if (obuf[0] == bkg_1[0] &&
			strncmp (obuf, bkg_1, SEGSIZE*2) == 0) {
			putc (RT_BKG1, fp);
		    } else if (obuf[0] == bkg_2[0] &&
			strncmp (obuf, bkg_2, SEGSIZE*2) == 0) {
			putc (RT_BKG2, fp);
		    } else {
			putc (RT_DATA, fp);
			fwrite (obuf, SEGSIZE*2, 1, fp);
		    }
		}
	    }

	    /* Write out any partial segment at the end of the line.  We must
	     * always write a full segment, even if the data at the end is
	     * garbage, else synchronization will be lost.
	     */
	    if (partseg) {
		for (op=obuf, n=partseg;  --n >= 0;  ) {
		    hp = hbuf + (*ip++ * 2);
		    *op++ = *hp++;
		    *op++ = *hp++;
		}
		putc (RT_DATA, fp);
		fwrite (obuf, SEGSIZE*2, 1, fp);
	    }
	}

	/* Add the NOAO logo and timestamp at the bottom of the page and
	 * output the page.
	 */
	fprintf (fp, "\n");
	fprintf (fp, "/Times-Roman findfont 24 scalefont setfont\n");

	/* fprintf (fp, "[-1 0 0 -1 2350 3180] setmatrix\n"); */
        fprintf (fp, "initmatrix\n");
        fprintf (fp, "-1 72 mul 300 div 1 72 mul 300 div scale\n");
        fprintf (fp, "-2409 88 translate\n");

	fprintf (fp, "%d %d moveto\n", 1600, 3150);
	fprintf (fp, "[1 0 0 -1 0 0] concat\n");
	fprintf (fp, "(%s) show\n", make_label());
	fprintf (fp, "showpage\n");
}


/* MAKE_LABEL -- Generate the label for the output printer page.
 */
static char *
make_label()
{
	static	char buf[128];
	char	hostname[32];
	char	username[32];
	struct	passwd *pw;
	long	clock;

	clock = time(0);
	gethostname (hostname, 32);
	pw = getpwuid (getuid());
	strcpy (username, pw->pw_name);
	endpwent();

	sprintf (buf, "NOAO/IRAF  %s@%s  %s",
	    username, hostname, asctime(localtime(&clock)));

	return (buf);
}
