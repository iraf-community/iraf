/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define import_spp
#define import_error
#include <iraf.h>

/*
 * SGI2UQMS.C -- Read IRAF SGI metacode from standard input, translate into
 * the QMS Vector Graphics Mode (Talaris Lasergrafix, QUIC Command Mode) and
 * write to standard output.
 *
 * Usage
 *	sgi2uqms.e [-params] [sgi_metacode] [| lpr -Pqms]
 *
 *	-l N   left edge; x plot origin in device pixels      def DEF_LEFT
 *	-b N   bottom edge; y plot origin in device pixels    def DEF_BOTTOM
 *	-w N   width of plot, device pixels starting from l   def DEF_WIDTH
 *	-h N   height of plot, device pixels starting from b  def DEF_HEIGHT
 *	-p O.S pen width `origin' and `slope'		      def DEF_PENBASE
 *								 .DEF_PENSLOPE
 *
 * Numeric values may be appended directly to their flags or separated by a
 * space; the input file name and the switches may occur in any order.
 */

#define OSOK		0		/* normal successful completion	   */
#define LEN_MCBUF	1024		/* number of SGK instrs in buffer  */
#define SGK_FRAME	1		/* new frame instruction	   */
#define SGK_MOVE	2		/* move pen			   */
#define SGK_DRAW	3		/* draw pen			   */
#define SGK_SETLW	4		/* set line width		   */
#define GKI_MAXNDC	32767		/* SGK units			   */

/* Device opcodes and parameters.  The default edge and width parameters (DEF_) 
 * are given in QMS pixels, at 300 dots/inch.  The QMS y-origin is at the
 * top of the page, so in GRAPHCAP, YF must be set, and Landscape Orientation
 * used in the QMS.  Thus the maximum page `width' is 11*300 pixels, `height'
 * is 8.5*300 pixels.
 */
#define DEV_INIT	"\012^PY^-\012^ISYNTAX00010^IOL^F^IGV" /* Enter graph.*/
#define DEV_END		"^IGE^ISYNTAX00000^IOP^O\012^PN^-\012" /* Exit graph. */
#define DEV_FRAME	"^,"	/* QMS Vector Graphics Mode form feed instr   */
#define DEV_MOVE	"^U"	/* QMS (VGM) pen-up instruction		      */
#define DEV_DRAW	"^D"	/* QMS (VGM) pen-down instruction	      */
#define DEV_SETLW	"^PW"	/* QMS (VGM) Set Line Width (follow w/ nn)    */

#define DEF_LEFT	30	/* origin in device pixels in x		      */
#define DEF_WIDTH	3180	/* width in x (300d/i, 11" paper)	      */
#define DEF_BOTTOM	60	/* origin in device pixels in y		      */
#define DEF_HEIGHT	2415	/* height in y (300d/i, 8.5" paper)	      */
#define DEF_PENBASE	3	/* base pen width (LW 1->2)		      */
#define DEF_PENSLOPE	4	/* pen width slope (LW 2->4, 3->6 etc.)	      */
#define SZ_PENCMD	5	/* total no. of chars in penwidth instruction */
#define SZ_PENVAL	2	/* no. of chars in penwidth value	      */
#define SZ_VECT		11	/* total no. chars in a MOVE or DRAW inst.    */
#define SZ_COORD	4	/* no. of chars in device coordinate          */

struct sgi_inst {
	short	opcode;
	short	x;
	short	y;
};

static int dev_left;
static int dev_bottom;
static int dev_width;
static int dev_height;
static int dev_penbase  = DEF_PENBASE;
static int dev_penslope = DEF_PENSLOPE;

static int get_iarg ( char, char *const [], int, int );
static void translate ( FILE *, FILE * );
static void bswap2 ( const char *, char *, int );
static const char *xyencode ( int, int, int );
static const char *penencode ( const char *, int );
static int swapped( void );

/* MAIN -- Main entry point for SGI2UQMS.  Optional arguments are device
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
	    fprintf (stderr, "Fatal error (sgi2uqms):  Cannot open `%s'\n",
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


/* GET_IARG -- Get an integer argument, whether appended directly to flag
 * or separated by a whitespace character; if error, report and assign default.
 */

static int get_iarg ( char argp, char *const argv[], int argno, int def_val )
{
	int	temp_val;

	if (argp == '\0')
	    if (argv[argno+1] == NULL) {
		fprintf (stderr, "missing arg to switch `%c';", argp);
		fprintf (stderr, " reset to %d\n", def_val);
		temp_val = def_val;
	    } else
		temp_val = atoi (argv[++argno]);
	else
	    temp_val = atoi (argv[argno]+2);

	return (temp_val);
}


/* TRANSLATE -- Interpret input SGI metacode instructions into device 
 * instructions and write to stdout.
 */
static void translate ( FILE *in, FILE *out )
{
	int n, x, y, swap_bytes;
	float xscale, yscale;
	struct sgi_inst *sgip;
	struct sgi_inst inbuf[LEN_MCBUF], *buftop;

	swap_bytes = swapped();

	xscale = (float) dev_width / (float) GKI_MAXNDC;
	yscale = (float) dev_height / (float) GKI_MAXNDC;

	/* Output device initialization.
	 */
	fwrite (DEV_INIT, strlen(DEV_INIT), 1, out);

	/* Initialize pen width.
	 */
	fwrite (penencode (DEV_SETLW, dev_penbase), SZ_PENCMD, 1, out);

	/* Process the metacode:
	 */
	while ((n = fread ((char *)inbuf, sizeof(*sgip), LEN_MCBUF, in)) > 0) {

	    if (swap_bytes)
		bswap2 ((char *)inbuf, (char *)inbuf, sizeof(*sgip) * n);

	    buftop = inbuf + n;

	    for (sgip = inbuf;  sgip < buftop;  sgip++) {
		switch (sgip->opcode) {
		case SGK_FRAME:
		    fwrite (DEV_FRAME, strlen(DEV_FRAME), 1, out);
		    break;

		case SGK_MOVE:
		    x = dev_left + sgip->x * xscale;
		    y = dev_bottom + sgip->y * yscale;
		    fwrite (xyencode ('U', x, y), SZ_VECT, 1, out);
		    break;

		case SGK_DRAW:
		    x = dev_left + sgip->x * xscale;
		    y = dev_bottom + sgip->y * yscale;
		    fwrite (xyencode ('D', x, y), SZ_VECT, 1, out);
		    break;

		case SGK_SETLW:
		    /* Set pen width.
		     */
		    fwrite (penencode (DEV_SETLW, dev_penbase +
			((sgip->x) - 1) * dev_penslope), SZ_PENCMD, 1, out);
		    break;
		
		default:
		    fprintf (stderr, "sgi2uqms: unrecognized sgi opcode %d\n",
			sgip->opcode);
		    break;
		}
	    }
	}

	/* Terminate plotting and exit.
	 */
	fwrite (DEV_END, strlen(DEV_END), 1, out);
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


/* XYENCODE -- Encode x, y into a character string formatted for the device.
 */
/* opcode : draw or move	*/
/* x, y   : must be positive	*/
static const char *xyencode ( int opcode, int x, int y )
{
	static char obuf[] = "^X0000:0000";
	char *op;
	int digit, n, i;

	obuf[1] = opcode;
	i = SZ_VECT - 1 - SZ_COORD - 1;
	for ( op = &obuf[i], digit = SZ_COORD, n=x ; digit > 0 ; n = n / 10, digit-- )
	    *op-- = n % 10 + '0';
	i = SZ_VECT - 1;
	for ( op = &obuf[i], digit = SZ_COORD, n=y ; digit > 0 ; n = n / 10, digit-- )
	    *op-- = n % 10 + '0';

	return (obuf);
}


/* PENENCODE -- Encode base, slope into a character string formatted for the
 * device set-pen command.
 */
/* opcode : device set-linewidth command	*/
/* val    : device line width			*/
static const char *penencode ( const char *opcode, int val )
{
	static char obuf[SZ_PENCMD+1];
	char *op;
	int digit, n;

	strncpy (obuf, opcode, SZ_PENCMD+1);
	obuf[SZ_PENCMD+1-1] = '\0';
	for ( op = &obuf[SZ_PENCMD-1], digit = SZ_PENVAL, n=val ; digit > 0 ;
	    n = n / 10, digit-- )
	    *op-- = n % 10 + '0';

	return (obuf);
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
