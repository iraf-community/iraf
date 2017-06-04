/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <time.h>
#include <pwd.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "sgiUtil.h"


/*
 * SGI2UAPL.C -- Read IRAF SGI metacode from standard input, translate into
 * Postscript for the Apple Laserwriter and write to standard output.
 *
 * Usage:
 *
 *	sgi2uapl.e [-params] [sgi_metacode] [| lpr -Plw]
 *
 *	-4	use 4 byte point encoding scheme (default is 7 byte)
 *	-t	omit the timestamp/logo normally written to corner of plot
 *	-l N	left edge; x plot origin in device pixels
 *	-b N	bottom edge; y plot origin in device pixels
 *	-w N	width of plot, device pixels starting from l
 *	-h N	height of plot, device pixels starting from b
 *	-p O.S	pen width `origin' and `slope'
 *
 * Numeric values may be appended directly to their flags or separated by a
 * space; the input file name and the switches may occur in any order.
 * Note that you can arrange for the Postscript output to be saved in a file
 * as well as directed to the printer by calling the translator as
 *
 *	sgi2uapl.e [params] | tee -a file | lpr -Plw
 */

#define	OSOK		0		/* normal successful completion	   */
#define	LEN_MCBUF	1024		/* number of SGK instrs in buffer  */
#define	SGK_FRAME	1		/* new frame instruction	   */
#define	SGK_MOVE	2		/* move pen	  		   */
#define	SGK_DRAW	3		/* draw pen			   */
#define	SGK_SETLW	4		/* set line width		   */
#define SGK_MAXNDC	32767		/* SGK units			   */

/* Device opcodes and parameters.  The default edge and width parameters (DEF_) 
 * are given in Apple Laserwriter pixels, at 300 dots/inch.  Although bypassing
 * the device-independence of Postscript, this achieves greater efficiency 
 * both in translator performance and in transmission rate to the plotter.
 * The device y-origin is at the lower left corner of the page, but we take
 * care of this with a Postscript coordinate transformation.  Thus the maximum
 * page `width' is 11*300 pixels, `height' is 8.5*300 pixels.
 */
#define DEV_FRAME	"showpage setcoords setjoins\n"		/* newframe   */
#define DEV_SETLW	"setlinewidth\n"			/* linewidth  */

#define	MAX_POINTS	512	/* maximum points on stack before a STROKE    */
#define DEF_LEFT	30	/* origin in device pixels in x		      */
#define DEF_WIDTH	3180	/* width in x (300d/i, 11" paper)	      */
#define DEF_BOTTOM	60	/* origin in device pixels in y		      */
#define DEF_HEIGHT	2415	/* height in y (300d/i, 8.5" paper)	      */
#define	DEF_PENBASE	3	/* base pen width (LW 1->2)	      	      */
#define DEF_PENSLOPE	4	/* pen width slope (LW 2->4, 3->6 etc.)	      */
#define SZ_PENCMD	16	/* total no. of chars in penwidth instruction */
#define	SZ_PENVAL	2	/* no. of chars in penwidth value	      */
#define	SZ_VCODE	7	/* no. of chars in MOVE or DRAW opcode	      */
#define	SZ_VECT		17	/* total no. chars in a MOVE or DRAW inst.    */
#define	SZ_COORD	4	/* no. of chars in device coordinate          */
#define	XY_MOVE		0
#define	XY_DRAW		1

struct sgi_inst {
	short	opcode;
	short	x;
	short	y;
};

/* Commands to setup Postscript environment.
 */
static char *ps_init[] = {
	"%!PS-Adobe-2.0",
	"/devppi 300 def",
	"/userppi 72 def",
	"/pagewidth 8.5 def",
	"/devpixtouser { userppi mul devppi div } def",
	"/pagetolandscape 90 def",
	"/setcoords { pagewidth userppi mul 0 translate",
	"    pagetolandscape rotate 1 devpixtouser 1 devpixtouser scale } def",
	"/setjoins { 1 setlinejoin 1 setlinecap } def",
	"erasepage initgraphics setcoords setjoins",
	NULL };

/* 7BYTE -- Postscript procedures to move or draw, with the X,Y coordinates
 * encoded as a 4 byte sequence (7 bytes including the command name and spaces)
 * following the command name.  This is the fastest encoding, since it
 * minimizes the Postscript interpreter execution time.
 *
 *	bit	76543210
 * ------------------------------------------
 *	     0  01XXXXXX	hi 6 X bits
 *	     1  01XXXXXX	li 6 X bits
 *	     2  01YYYYYY	hi 6 Y bits
 *	     3  01YYYYYY	lo 6 Y bits
 */
static char *ps_7byte[] = {
	"/getpoint {",
	"	currentfile read pop 8#77 and 6 bitshift",
	"	currentfile read pop 8#77 and or",
	"	currentfile read pop 8#77 and 6 bitshift",
	"	currentfile read pop 8#77 and or",
	"	} def",
	"/m { getpoint moveto } def",
	"/d { getpoint lineto } def",
	NULL };

/* 4BYTE -- Postscript procedure to draw an arbitrary series of vectors,
 * encoded 4 bytes per point.  This is the most space efficient encoding,
 * but the runtime may be somewhat longer than for the 7byte encoding,
 * due to the Postscript interpreter overhead.
 *
 * Optional Flag byte:
 *		>		move instruction
 *		+		draw instruction
 *		$		terminate plotting
 *		other		ignored (e.g., whitespace)
 *
 * Point Encoding:
 *	       bit 76543210
 *	 ------------------------------------------
 *		1  01XXXXXX	hi 6 X bits
 *		2  01XXXXXX	lo 6 X bits
 *		3  01YYYYYY	hi 6 Y bits
 *		4  01YYYYYY	lo 6 Y bits
 *
 * Data points are encoded 12 bits (0-4095) per X,Y coordinate.  The flag byte
 * is used as the terminator, to toggle the move/draw state, and to permit
 * whitespace to be ignored.  A high bit is set in each byte to ensure a
 * printable character, as control codes would interfere with the operation of
 * the Postscript interpreter.
 */
#define	CH_MOVE		'>'
#define	CH_DRAW		'+'
#define	CH_EXITPLOT	'$'

static char *ps_4byte[] = {
	"/plot { /movepen { moveto } def",
	"	 { currentfile read not { exit } if",
	"	   { dup 8#100 ge",
	"	     { exit }",
	"	     { dup 8#076 eq",
	"	       { pop /movepen { moveto } def",
	"		 currentfile read not { exit } if }",
	"	       { dup 8#053 eq",
	"	         { pop /movepen { lineto } def",
	"		   currentfile read not { exit } if }",
	"	         { dup 8#044 eq",
	"		   { exit }",
	"		   { pop currentfile read not { exit } if }",
	"		   ifelse }",
	"		 ifelse }",
	"	       ifelse }",
	"	     ifelse",
	"	   } loop",
	"	   dup 8#044 eq { pop exit } { 8#77 and 6 bitshift } ifelse",
	"	   currentfile read pop 8#77 and or",
	"	   currentfile read pop 8#77 and 6 bitshift",
	"	   currentfile read pop 8#77 and or",
	"	   movepen",
	"	 } loop",
	"      } def",
	NULL };

static	int fourbyte = 0;
static	int penmode = -1;
static	int omit_logo = 0;
static	int dev_left;
static	int dev_bottom;
static	int dev_width;
static	int dev_height;
static	int dev_penbase  = DEF_PENBASE;
static	int dev_penslope = DEF_PENSLOPE;
static	int npts = 0;


static void  translate (FILE *in, FILE *out);
static char *make_label (void);
static void  textout (FILE *out, char *text[]);
static char *penencode (int val, char *code);
static void  xy_flush (FILE *out);
static void  xy_point (FILE *out, register int x, register int y, int flag);



/* MAIN -- Main entry point for SGI2UAPL.  Optional arguments are device
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
	char	penparam[SZ_PENCMD];


	infile = "stdin";

	/* Process the command line.
	 */
	for (argno=1;  (argp = argv[argno]) != NULL;  argno++) {
	    if (argp[0] == '-') {
		/* A window-control or pen width switch.
		 */
		switch (argp[1]) {
		case '4':
		    fourbyte++;
		    break;
		case 't':
		    omit_logo++;
		    break;
		case 'l':
		    dev_left = get_iarg (argp[2], argv, argno, DEF_LEFT);
		    break;
		case 'b':
		    dev_bottom = get_iarg (argp[2], argv, argno, DEF_BOTTOM);
		    break;
		case 'w':
		    dev_width = get_iarg (argp[2], argv, argno, DEF_WIDTH);
		    break;
		case 'h':
		    dev_height = get_iarg (argp[2], argv, argno, DEF_HEIGHT);
		    break;
		case 'p':
		    if (argp[2] == (char) 0)
			if (argv[argno+1] == NULL) {
			    fprintf (stderr, "missing arg to switch `%s';",
				argp);
			    fprintf (stderr, " reset to %d.%d\n", dev_penbase,
				dev_penslope);
			} else
			    strcpy (penparam, argv[++argno]);
		    else
			strcpy (penparam, argv[argno]+2);
		    
		    np = sscanf (penparam, "%d . %d", &dev_penbase,
			&dev_penslope);
		    if (np == 1) {
			dev_penslope = dev_penbase;
		    } else if (np < 1) {
			dev_penbase = DEF_PENBASE;
			dev_penslope = DEF_PENSLOPE;
		    }

		    break;
		default:
		    fprintf (stderr, "sgi2uapl: unknown switch '%s'\n", argp);
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
	    fprintf (stderr, "Fatal error (sgi2uapl):  Cannot open `%s'\n",
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


/* TRANSLATE -- Interpret input SGI metacode instructions into device 
 * instructions and write to stdout.
 */
static void
translate (FILE *in, FILE *out)
{
	register struct sgi_inst *sgip;
	struct	 sgi_inst inbuf[LEN_MCBUF], *buftop;
	int	 n, x, y, curpoints = 0, swap_bytes;
	float	 xscale, yscale;


	swap_bytes = isSwapped();

	xscale = (float) dev_width / (float) SGK_MAXNDC;
	yscale = (float) dev_height / (float) SGK_MAXNDC;

	/* Output device initialization.  */
	textout (out, ps_init);

	/* Define the Postscript plotting procedures. */
	if (fourbyte)
	    textout (out, ps_4byte);
	else
	    textout (out, ps_7byte);

	/* Initialize pen width.  */
	fwrite (penencode (dev_penbase, DEV_SETLW), SZ_PENCMD, 1, out);

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
		    xy_flush (out);
		    fprintf (out, DEV_FRAME);
		    break;

		case SGK_MOVE:
		    x = dev_left + sgip->x * xscale;
		    y = dev_bottom + sgip->y * yscale;
		    xy_point (out, x, y, XY_MOVE);
		    break;

		case SGK_DRAW:
		    x = dev_left + sgip->x * xscale;
		    y = dev_bottom + sgip->y * yscale;
		    xy_point (out, x, y, XY_DRAW);

		    /* Limit number of points passed to Postscript between
		     * 'stroke' commands to draw the buffered points.
		     */
		    curpoints = curpoints + 1;
		    if (curpoints > MAX_POINTS) {
			xy_flush (out);
			xy_point (out, x, y, XY_MOVE);
			curpoints = 0;
		    }
		    break;

		case SGK_SETLW: {
		    /* Set pen width.
		     */
		    int x = max (0, sgip->x - 1);

		    xy_flush (out);
		    curpoints = 0;
		    fwrite (penencode (
			max (1, dev_penbase + x * dev_penslope), DEV_SETLW),
			SZ_PENCMD, 1, out);
		    }
		    break;
		
		default:
		    fprintf (stderr, "sgi2uapl: unrecognized sgi opcode %d\n",
			sgip->opcode);
		    break;
		}
	    }
	}

	/* Terminate plotting and exit.
	 */
	xy_flush (out);

	/* Add the NOAO logo and timestamp at the bottom of the page and
	 * output the page.
	 */
	if (!omit_logo) {
	    fprintf (out, "/Times-Roman findfont 24 scalefont setfont\n");

	    /* fprintf (out, "[-1 0 0 -1 2350 3180] setmatrix\n"); */
	    fprintf (out, "initmatrix\n");
	    fprintf (out, "-1 72 mul 300 div 1 72 mul 300 div scale\n");
	    fprintf (out, "-2409 88 translate\n");

	    fprintf (out, "%d %d moveto\n", 1600, 3150);
	    fprintf (out, "[1 0 0 -1 0 0] concat\n");
	    fprintf (out, "(%s) show\n", make_label());
	}

	fprintf (out, "showpage\n");
}


/* XY_POINT -- Output a move or draw instruction, using either the 4 byte or
 * 7 byte encoding scheme.
 */
static void
xy_point (
    FILE *out,			/* output file */
    register int x, 		/* coords to move to */
    register int y, 		/* coords to move to */
    int	flag			/* move or draw? */
)
{
	static char o[] = "m XXXX";
	register char	*op = o;
	register int	n;

	if (fourbyte) {
	    if (npts == 0) {
		fputs ("plot\n", out);
		penmode = XY_MOVE;
	    }

	    if (flag != penmode)
		*op++ = ((penmode = flag) == XY_MOVE) ? CH_MOVE : CH_DRAW;

	    *op++ = ((n = ((x >> 6) & 077)) == 077) ? n : (0100 | n);
	    *op++ = ((n = (x & 077)) == 077) ? n : (0100 | n);
	    *op++ = ((n = ((y >> 6) & 077)) == 077) ? n : (0100 | n);
	    *op++ = ((n = (y & 077)) == 077) ? n : (0100 | n);

	    fwrite (o, op-o, 1, out);
	    if (!(++npts % 15))
		fputc ('\n', out);

	} else {
	    o[0] = (flag == XY_MOVE) ? 'm' : 'd';
	    o[2] = ((n = ((x >> 6) & 077)) == 077) ? n : (0100 | n);
	    o[3] = ((n = (x & 077)) == 077) ? n : (0100 | n);
	    o[4] = ((n = ((y >> 6) & 077)) == 077) ? n : (0100 | n);
	    o[5] = ((n = (y & 077)) == 077) ? n : (0100 | n);

	    fwrite (o, 6, 1, out);
	    if (!(++npts % 10))
		fputc ('\n', out);
	    else
		fputc (' ', out);
	}
}


/* XY_FLUSH -- Terminate the current drawing sequence, if any, and issue the
 * stroke command to Postscript to draw the buffered points.
 */
static void
xy_flush (FILE *out)
{
	if (npts > 0) {
	    if (fourbyte)
		fputs ("$\n", out);
	    else if (npts % 10)
		fputc ('\n', out);
	    fputs ("stroke\n", out);
	    npts = 0;
	}
}


/* PENENCODE -- Encode base, slope into a character string formatted for the
 * device set-pen command.
 */
static char *
penencode (
    int	  val,			/* device line width 		*/
    char *code 			/* device set-linewidth command */
)
{
	static char	obuf[SZ_PENCMD+1];
	register int	digit, n;
	register char	*op, *ip;

	for (op = &obuf[SZ_PENVAL-1], digit = SZ_PENVAL, n=val;  --digit >= 0;
	    n = n / 10)
	    *op-- = n % 10 + '0';
	obuf[SZ_PENVAL] = ' ';
	for (op = &obuf[SZ_PENVAL+1], ip = code, n = SZ_PENCMD;  --n >= 0;  )
	    *op++ = *ip++;

	return (obuf);
}


/* TEXTOUT -- Output lines of text to a file.
 */
static void
textout (
    FILE  *out,			/* output file */
    char  *text[] 		/* array of lines of text */
)
{
	register char **lp;

	for (lp=text;  *lp;  lp++) {
	    fputs (*lp, out);
	    fputc ('\n', out);
	}
}


/* MAKE_LABEL -- Generate the label for the output printer page.
 */
static char *
make_label (void)
{
	static	char buf[128];
	char	hostname[32];
	char	username[32];
	struct	passwd *pw;
	time_t	clock;

	gethostname (hostname, 32);
	clock = time(0);
	pw = getpwuid (getuid());
	strcpy (username, pw->pw_name);
	endpwent();

	sprintf (buf, "NOAO/IRAF  %s@%s  %s",
	    username, hostname, asctime(localtime(&clock)));

	return (buf);
}
