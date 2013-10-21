/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "sgiUtil.h"


/*
 * SGI2UIMP.C -- Read IRAF SGI metacode from standard input, translate into
 * the Impress language and write to standard output.
 *
 * Usage
 *	sgi2uimp.e [-params] [sgi_metacode] [| lpr -v -Pimagen]
 *
 *	-l N  left edge; x plot origin in device pixels       def DEF_LEFT
 *	-b N  bottom edge; y plot origin in device pixels     def DEF_BOTTOM
 *	-w N  width of plot, device pixels starting from l    def DEF_WIDTH
 *	-h N  height of plot, device pixels starting from b   def DEF_HEIGHT
 *	-p O.S pen width `origin' and `slope'	 	      def DEF_PENBASE
 *								 .DEF_PENSLOPE
 *
 * Numeric values may be appended directly to their flags or separated by a
 * space; the input file name and the switches may occur in any order.
 * The windowing parameters are specified explicitly rather than using the
 * plotter resolution and paper size due to differences in exactly where
 * each different Imagen begins and ends plotting within the paper window.
 *
 */

#define	OSOK		0		/* normal successful completion	   */
#define	LEN_MCBUF	1024		/* number of SGK instrs in buffer  */
#define	SGK_FRAME	1		/* new frame instruction	   */
#define	SGK_MOVE	2		/* move pen	  		   */
#define	SGK_DRAW	3		/* draw pen			   */
#define	SGK_SETLW	4		/* set line width		   */
#define GKI_MAXNDC	32767		/* SGK units			   */

/* Device opcodes and parameters.  The default edge and width parameters
 * (DEF_), given in device pixels for a 240 dot/inch Imagen with 8 1/2" x 11"
 * paper, should be modified to fit the local plotter.
 */
#define DEF_LEFT	85		/* origin in device pixels in x	   */
#define DEF_WIDTH	2490		/* width in x (240d/i, 11" paper)  */
#define DEF_BOTTOM	50		/* origin in device pixels in y    */
#define DEF_HEIGHT	1905		/* height in y (240d/i, 8.5" paper)*/
#define	DEF_PENBASE	2		/* base pen width 		   */
#define DEF_PENSLOPE	2		/* pen slope (b.s=3.2 ==> 3,5,7,9) */
#define SZ_PENPARAM	5		/* max chars in penwidth parameter */
#define BLACK_LINE	15		/* draw solid line		   */
#define CREATE_PATH	230		/* set up set of vertices	   */
#define DRAW_PATH	234		/* draw that set of vertices	   */
#define END_DOCUMENT	255		/* end of document		   */
#define END_PAGE	219		/* formfeed			   */
#define HV_VALUE	125		/* 0 11 11 101 (orig, axes, orient)*/
#define SET_ABS_H	135		/* move absolute in h              */
#define SET_ABS_V	137		/* move absolute in v		   */
#define SET_HV_SYSTEM	205		/* establish coordinate system     */
#define SET_PEN		232		/* set pen width		   */
#define SZ_HEAD		3		/* # header opcode bytes in obuf   */
#define SZ_TAIL		2		/* # trailing opcode bytes in obuf */
#define	COUNT_OFFSET	1		/* byte offset to npoints in obuf  */

/* Output macros -- watch out for SZ_OBUF; some Imagens have a limited amount
 * of memory for the DRAW buffer:
 */
#define SZ_OBUF		(1024)
#define DECL_OBUF	register char *op; char *np; char obuf[SZ_OBUF+1];
#define o_clear		(op=obuf)
#define o_flush(o)	fwrite(obuf,op-obuf,1,o)
#define putbyte(v)	(*op++ = (v))
#define putword(v)	((*op++ = (v)/256), (*op++ = (v)%256))
#define setcount(b,v)	((np = obuf+b), (*np++ = (v)/256), (*np = (v)%256))
#define npoints		((op-obuf - SZ_HEAD)/4)
#define obuf_full	((op-obuf + SZ_TAIL) >= SZ_OBUF)

struct sgi_inst {
	short	opcode;
	short	x;
	short	y;
};

int	imp_left;
int	imp_bottom;
int	imp_width;
int	imp_height;
int	imp_penbase  = DEF_PENBASE;
int	imp_penslope = DEF_PENSLOPE;

static void translate (FILE *in, FILE *out);


/* MAIN -- Main entry point for SGI2UIMP.  Optional arguments are device
 * window parameters and name of input file.
 */
int
main (int argc, char *argv[])
{
	FILE	*in;
	char	*infile;
	char	*argp;
	int	argno;
	int	np;
	char	penparam[SZ_PENPARAM];


	infile = "stdin";

	/* Process the command line.
	 */
	for (argno=1;  (argp = argv[argno]) != NULL;  argno++) {
	    if (argp[0] == '-') {
		/* A window-control or pen width switch.
		 */
		switch (argp[1]) {
		case 'l':
		    imp_left = get_iarg (argp[2], argv, argno, DEF_LEFT);
		    break;
		case 'b':
		    imp_bottom = get_iarg (argp[2], argv, argno, DEF_BOTTOM);
		    break;
		case 'w':
		    imp_width = get_iarg (argp[2], argv, argno, DEF_WIDTH);
		    break;
		case 'h':
		    imp_height = get_iarg (argp[2], argv, argno, DEF_HEIGHT);
		    break;
		case 'p':
		    if (argp[2] == (char) 0)
			if (argv[argno+1] == NULL) {
			    fprintf (stderr, "missing arg to switch `%s';",
				argp);
			    fprintf (stderr, " reset to %d.%d\n", imp_penbase,
				imp_penslope);
			} else
			    strcpy (penparam, argv[++argno]);
		    else
			strcpy (penparam, argv[argno]+2);
		    
		    np = sscanf (penparam, "%d . %d", &imp_penbase,
			&imp_penslope);
		    if (np == 1) {
			imp_penslope = imp_penbase;
		    } else if (np < 1) {
			imp_penbase = DEF_PENBASE;
			imp_penslope = DEF_PENSLOPE;
		    }

		    break;
		default:
		    break;
		}

	    } else {
		/* Input sgi-metacode file specification.
		 */
		infile = argp;
	    }
	}

	if (strcmp (infile, "stdin") == 0)
	    in = stdin;
	else
	    in = fopen (infile, "r");

	if (in == NULL) {
	    fprintf (stderr, "Fatal error (sgi2uimp):  Cannot open `%s'\n",
		infile);
	    fflush (stderr);
	    exit (OSOK+1);
	}

	/* Process the metacode.
	 */
	translate (in, stdout);

	if (in != stdin)
	    fclose (in);

	return (0);
}


/* TRANSLATE -- Interpret input SGI metacode instructions into the device
 * language and write to stdout.
 */
static void
translate (FILE *in, FILE *out)
{
	int	 n, x, y, swap_bytes;
	float	 xscale, yscale;
	register struct sgi_inst *sgip;
	struct	 sgi_inst inbuf[LEN_MCBUF], *buftop;
	DECL_OBUF;

	swap_bytes = isSwapped();

	xscale = (float) imp_width / (float) GKI_MAXNDC;
	yscale = (float) imp_height / (float) GKI_MAXNDC;

	/* Output device header instructions.
	 */
	fprintf (out, "@Document(%s, %s, %s, %s, %s, %s)",
	    "language impress", "pagecollation on", "jamresistance on",
	    "name \"IRAF SGI plot\"", "prerasterization on", "jobheader off");

	/* Output page orientation and coordinate system initialization.
	 */
	putc (SET_HV_SYSTEM, out);
	putc (HV_VALUE, out);

	/* Initialize pen width.
	 */
	putc (SET_PEN, out);
	putc (1 * imp_penbase, out);

	o_clear;

	/* Process the metacode:
	 */
	while ((n = fread ((char *)inbuf, sizeof(*sgip), LEN_MCBUF, in)) > 0) {

	    if (swap_bytes)
		bswap2 ((unsigned char *)inbuf, (unsigned char *)inbuf, 
		    sizeof(*sgip) * n);

	    buftop = inbuf + n;

	    for (sgip = inbuf;  sgip < buftop;  sgip++) {
		switch (sgip->opcode) {
		case SGK_FRAME:
		    /* Terminate and output any DRAW buffer contents.
		     */
		    if (npoints > 1) {
			setcount (COUNT_OFFSET, npoints);
			putbyte (DRAW_PATH);
			putbyte (BLACK_LINE);
			o_flush (out);
		    }

		    o_clear;
		    putbyte (END_PAGE);
		    putbyte (SET_ABS_H);
		    putword (0);
		    putbyte (SET_ABS_V);
		    putword (0);
		    o_flush (out);
		    break;

		case SGK_MOVE:
		    /* Terminate and output any DRAW buffer contents.
		     */
		    if (npoints > 1) {
			setcount (COUNT_OFFSET, npoints);
			putbyte (DRAW_PATH);
			putbyte (BLACK_LINE);
			o_flush (out);
		    }

		    x = imp_left + sgip->x * xscale;
		    y = imp_bottom + sgip->y * yscale;

		    /* Initialize output buffer for start of draw instruction.
		     */
		    o_clear;
		    putbyte (CREATE_PATH);
		    putword (1);
		    putword (x); putword (y);

		    break;

		case SGK_DRAW:
		    x = imp_left + sgip->x * xscale;
		    y = imp_bottom + sgip->y * yscale;
		    putword (x); putword (y);

		    /* If we are about to exceed output buffer, flush and re-
		     * initialize starting with current point.
		     */
		    if (obuf_full) {
			setcount (COUNT_OFFSET, npoints);
			putbyte (DRAW_PATH);
			putbyte (BLACK_LINE);
			o_flush (out);

			/* Reinitialize DRAW buffer.
			 */
			o_clear;
			putbyte (CREATE_PATH);
			putword (1);
			putword (x); putword (y);
		    }

		    break;

		case SGK_SETLW:
		    /* Terminate and output any DRAW buffer contents.
		     */
		    if (npoints > 1) {
			setcount (COUNT_OFFSET, npoints);
			putbyte (DRAW_PATH);
			putbyte (BLACK_LINE);
			o_flush (out);
			o_clear;
		    }

		    /* Set pen width.
		     */
		    putc (SET_PEN, out);
		    putc ((imp_penbase + ((sgip->x) - 1) * imp_penslope), out);
		    break;
		
		default:
		    fprintf (stderr, "sgi2uimp: unrecognized sgi opcode %d\n",
			sgip->opcode);
		    break;
		}
	    }
	}

	/* Flush any remaining buffered points.
	 */
	if (npoints > 1) {
	    setcount (COUNT_OFFSET, npoints);
	    putbyte (DRAW_PATH);
	    putbyte (BLACK_LINE);
	    o_flush (out);
	}

	/* Signal end of page and end of document.
	 */
	putc (END_PAGE, out);
	putc (END_DOCUMENT, out);
}
