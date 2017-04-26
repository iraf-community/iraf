#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "sgiUtil.h"


/*
 * SGI2UHPGL.C -- Read IRAF SGI metacode from standard input, translate into
 * HP graphics language call for HP 7550A plotter
 *
 * Usage
 *	sgi2uhpgl.e [sgi_metacode] [| lpr -Papple]
 *
 */

#define	OSOK		0		/* normal successful completion	   */
#define	LEN_MCBUF	1024		/* number of SGK instrs in buffer  */
#define	SGK_FRAME	1		/* new frame instruction	   */
#define	SGK_MOVE	2		/* move pen	  		   */
#define	SGK_DRAW	3		/* draw pen			   */
#define	SGK_SETLW	4		/* set line width		   */
#define GKI_MAXNDC	32767		/* SGK units			   */

/* Device opcodes and parameters.  
 */
#define DEV_INIT	"IN;DF;SP1;"				/* initialize */
#define DEV_END		"SP0;PG:"				/* terminate  */
#define DEV_FRAME	"PG;"					/* newframe   */
#define DEV_MOVE 	"PU"					/* move       */
#define DEV_DRAW 	"PD"					/* draw       */
#define	PRES		1016		/* plotter resolution per inch */
#define XLEN_INCHES	10.0		/* width of plot (x) in inches */
#define YLEN_INCHES	8.0		/* height of plot (y) in inches */
#define XSCALE		PRES * XLEN_INCHES / GKI_MAXNDC
#define YSCALE		PRES * YLEN_INCHES / GKI_MAXNDC


#define	SZ_COORD	4	/* no. of chars in device coordinate          */

struct sgi_inst {
	short	opcode;
	short	x;
	short	y;
};

static void translate (FILE *in, FILE *out);


/* MAIN -- Main entry point for SGI2UHPGL.  Optional arguments are device
 * window parameters and name of input file.
 */
int
main (int argc, char *argv[])
{
	FILE	*in;
	char	*infile;


	infile = "stdin";

	/* Process the command line.
	 */
	infile = argv[1];

	if (strcmp (infile, "stdin") == 0)
	    in = stdin;
	else
	    in = fopen (infile, "r");

	if (in == NULL) {
	    fprintf (stderr, "Fatal error (sgi2uhpp):  Cannot open `%s'\n",
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
	int	 n, swap_bytes;
	float	x, y;


	swap_bytes = isSwapped();

	/* Output device initialization.
	 */
	fprintf (out, "%s\n", DEV_INIT);

	/* Initialize pen width. Not implemented.
	 */

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
		    fprintf (out, "%s\n", DEV_FRAME);
		    break;

		case SGK_MOVE:
		    x = sgip->x * XSCALE;
		    y = sgip->y * YSCALE;
		    fprintf (out,
			"%s%06.0f%s%06.0f%s\n", DEV_MOVE, x, ",", y, ";");
		    break;

		case SGK_DRAW:
		    x = sgip->x * XSCALE;
		    y = sgip->y * YSCALE;
		    fprintf (out,
			"%s%06.0f%s%06.0f%s\n", DEV_DRAW, x, ",", y, ";");
		    break;

		case SGK_SETLW:
		    /* Set pen width.
		     */
		    break;
		default:
		    fprintf (stderr, "sgi2uhpp: unrecognized sgi opcode %d\n",
			sgip->opcode);
		    break;
		}
	    }
	}

	/* Terminate plotting and exit.
	 */
	fwrite (DEV_END, strlen(DEV_END), 1, out);
	fprintf (out, "\n");
}
