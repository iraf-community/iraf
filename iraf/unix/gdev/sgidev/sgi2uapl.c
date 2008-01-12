/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <pwd.h>

#ifdef SOLARIS
#include <sys/systeminfo.h>
#endif

#define import_spp
#define import_error
#include <iraf.h>

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

#define OSOK		0		/* normal successful completion	   */
#define LEN_MCBUF	1024		/* number of SGK instrs in buffer  */
#define SGK_FRAME	1		/* new frame instruction	   */
#define SGK_MOVE	2		/* move pen			   */
#define SGK_DRAW	3		/* draw pen			   */
#define SGK_SETLW	4		/* set line width		   */
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

#define MAX_POINTS	512	/* maximum points on stack before a STROKE    */
#define DEF_LEFT	30	/* origin in device pixels in x		      */
#define DEF_WIDTH	3180	/* width in x (300d/i, 11" paper)	      */
#define DEF_BOTTOM	60	/* origin in device pixels in y		      */
#define DEF_HEIGHT	2415	/* height in y (300d/i, 8.5" paper)	      */
#define DEF_PENBASE	3	/* base pen width (LW 1->2)		      */
#define DEF_PENSLOPE	4	/* pen width slope (LW 2->4, 3->6 etc.)	      */
#define SZ_PENCMD	16	/* total no. of chars in penwidth instruction */
#define SZ_PENVAL	2	/* no. of chars in penwidth value	      */
#define SZ_VCODE	7	/* no. of chars in MOVE or DRAW opcode	      */
#define SZ_VECT		17	/* total no. chars in a MOVE or DRAW inst.    */
#define SZ_COORD	4	/* no. of chars in device coordinate          */
#define XY_MOVE		0
#define XY_DRAW		1

struct sgi_inst {
	short	opcode;
	short	x;
	short	y;
};

/* Commands to setup Postscript environment.
 */
static const char *ps_init[] = {
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
static const char *ps_7byte[] = {
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
#define CH_MOVE		'>'
#define CH_DRAW		'+'
#define CH_EXITPLOT	'$'

static const char *ps_4byte[] = {
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

static void translate ( FILE *, FILE * );
static void xy_point ( FILE *, int, int, int );
static void xy_flush ( FILE * );
static const char *penencode ( int, const char * );
static void textout ( FILE *, const char *[] );
static const char *make_label( void );
static int swapped( void );
static void bswap2 ( const char *, char *, int );
static int get_iarg ( char, char *const [], int, int );

/* MAIN -- Main entry point for SGI2UAPL.  Optional arguments are device
 * window parameters and name of input file.
 */
int main ( int argc, char *argv[] )
{
	char penparam[SZ_PENCMD];
	const char *infile;
	const char *argp;
	int argno, np;
	FILE *in;

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
		    if (argp[2] == '\0')
			if (argv[argno+1] == NULL) {
			    fprintf (stderr, "missing arg to switch `%s';",
				argp);
			    fprintf (stderr, " reset to %d.%d\n", dev_penbase,
				dev_penslope);
			} else {
			    strncpy (penparam, argv[++argno], SZ_PENCMD);
			    penparam[SZ_PENCMD-1] = '\0';
			}
		    else {
			strncpy (penparam, argv[argno]+2, SZ_PENCMD);
			penparam[SZ_PENCMD-1] = '\0';
		    }
		    
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

	return 0;
}


/* TRANSLATE -- Interpret input SGI metacode instructions into device 
 * instructions and write to stdout.
 */
static void translate ( FILE *in, FILE *out )
{
	struct sgi_inst *sgip;
	struct sgi_inst inbuf[LEN_MCBUF], *buftop;
	int n, x, y, curpoints = 0, swap_bytes;
	float xscale, yscale;

	swap_bytes = swapped();

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
		bswap2 ((const char *)inbuf, (char *)inbuf, sizeof(*sgip) * n);

	    buftop = inbuf + n;

	    for (sgip = inbuf;  sgip < buftop;  sgip++) {
		switch (sgip->opcode) {
		case SGK_FRAME:
		    xy_flush (out);
		    fprintf (out, "%s", DEV_FRAME);
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
/* out  : output file */
/* x, y : coords to move to */
/* flag : move or draw? */
static void xy_point ( FILE *out, int x, int y, int flag )
{
	static char o[] = "m XXXX";
	char *op = o;
	int n;

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
static void xy_flush ( FILE *out )
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


#if 0
/* XYENCODE -- Encode x, y into a character string formatted for the device.
 */
/* x, y : must be positive	*/
/* code : draw or move		*/
static const char *xyencode ( int x, int y, const char *code )
{
	static char obuf[SZ_VECT+1];
	const char *ip;
	char *op;
	int digit, n, i;

	i = 0 + SZ_COORD - 1;
	for ( op = &obuf[i++], digit=SZ_COORD, n=x ; digit > 0 ; n = n / 10, digit-- )
	    *op-- = n % 10 + '0';
	obuf[i] = ' ';
	i = i + SZ_COORD;
	for ( op = &obuf[i++], digit=SZ_COORD, n=y ; digit > 0 ; n = n / 10, digit-- )
	    *op-- = n % 10 + '0';

	obuf[i++] = ' ';
	for ( op = &obuf[i++], ip = code, n = SZ_VCODE ; n > 0 ; i++, n-- )
	    *op++ = *ip++;

	return (obuf);
}
#endif


/* PENENCODE -- Encode base, slope into a character string formatted for the
 * device set-pen command.
 */
/* val  : device line width		*/
/* code : device set-linewidth command	*/
static const char *penencode ( int val, const char *code )
{
	static char obuf[SZ_PENCMD+1];
	const char *ip;
	char *op;
	int digit, n;

	for ( op = &obuf[SZ_PENVAL-1], digit = SZ_PENVAL, n=val ; digit > 0 ;
	    n = n / 10, digit-- )
	    *op-- = n % 10 + '0';
	obuf[SZ_PENVAL] = ' ';
	for ( op = &obuf[SZ_PENVAL+1], ip = code, n = SZ_PENCMD ; n > 0 ; n-- )
	    *op++ = *ip++;

	return (obuf);
}


/* TEXTOUT -- Output lines of text to a file.
 */
/* out  : output file			*/
/* text : array of lines of text	*/
static void textout ( FILE *out, const char *text[] )
{
	const char **lp;

	for (lp=text;  *lp;  lp++) {
	    fputs (*lp, out);
	    fputc ('\n', out);
	}
}


/* MAKE_LABEL -- Generate the label for the output printer page.
 */
static const char *make_label( void )
{
	static	char buf[128];
	char	hostname[32];
	char	username[32];
	struct	passwd *pw;
	long	clock;

#ifdef SOLARIS
	sysinfo (SI_HOSTNAME, hostname, 32);
#else
	gethostname (hostname, 32);
#endif

	clock = time(0);
	pw = getpwuid (getuid());
	strncpy (username, pw->pw_name, 32);
	username[32-1] = '\0';
	endpwent();

	snprintf (buf, 128, "NOAO/IRAF  %s@%s  %s",
	    username, hostname, asctime(localtime(&clock)));

	return (buf);
}


/* SWAPPED -- Test whether we are running on a byte-swapped machine.
 */
static int swapped( void )
{
	union {
	    short   tswap;
	    char    b[2];
	} test;

	test.tswap = 1;
	return (test.b[0]);
}	


/* BSWAP2 -- Move bytes from array "a" to array "b", swapping successive
 * pairs of bytes.  The two arrays may be the same but may not be offset
 * and overlapping.
 */
/* a      : input array			*/
/* b      : output array		*/
/* nbytes : number of bytes to swap	*/
static void bswap2 ( const char *a, char *b, int nbytes )
{
	const char *ip;
	char *op, *otop;
	unsigned int temp;

	ip = a;
	op = b;
	otop = op + (nbytes & ~1);

	/* Swap successive pairs of bytes.
	 */
	while (op < otop) {
	    temp  = *ip++;
	    *op++ = *ip++;
	    *op++ = temp;
	}

	/* If there is an odd byte left, move it to the output array.
	 */
	if (nbytes & 1)
	    *op = *ip;
}


/* GET_IARG -- Get an integer argument, whether appended directly to flag
 * or separated by a whitespace character; if error, report and assign default.
 */
static int get_iarg ( char argp, char *const argv[], int argno, int def_val )
{
	int	temp_val;

	if (argp == '\0') {
	    if (argv[argno+1] == NULL) {
		fprintf (stderr, "missing arg to switch `%c';", argp);
		fprintf (stderr, " reset to %d\n", def_val);
		temp_val = def_val;
	    } else
		temp_val = atoi (argv[++argno]);
	} else
	    temp_val = atoi (argv[argno]+2);

	return (temp_val);
}
