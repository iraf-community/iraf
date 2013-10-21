#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "sgiUtil.h"


/*
** SGI2SVG.C -- Read IRAF SGI metacode from standard input, translate into
** Scalable Vector Graphics (SVG) format.
**
** Usage
**	sgi2svg.e [-params] [sgi_metacode] [| lpr -Papple]
**
**      -fg <N>   FG color specified as RGB triplet (e.g. 'F00' is red)
**      -bg <N>   BG color specified as RGB triplet (e.g. '0F0' is green)
**      -fill <N> fill color specified as RGB triplet (e.g. '00F' is blue)
**      -w <N>    width of plot, device pixels starting from l
**      -h <N>    height of plot, device pixels starting from b
**      -p <N>    pen width
**
**  Option values must be separated from their flags by a space; the input 
**  file name and the switches may occur in any order.
**
**  Sample Graphcap:
**
**  g-svg|UNIX generic interface to SVG file generator:\
**          :DD=usvg,tmp$sgk,!{ sgidispatch sgi2svg -w $(PX) -h $(PY) \
**          -bg FFF -fg 000 -fill FFF $F > sgi$$.svg ; }&:PX#640:PY#480:\
**          :kf=bin$x_sgikern.e:tn=sgikern:tc=sgi_apl:
*/

#define	OSOK		0		/* normal successful completion	   */
#define	LEN_MCBUF	1024		/* number of SGK instrs in buffer  */
#define	SGK_FRAME	1		/* new frame instruction	   */
#define	SGK_MOVE	2		/* move pen	  		   */
#define	SGK_DRAW	3		/* draw pen			   */
#define	SGK_SETLW	4		/* set line width		   */
#define GKI_MAXNDC	32767.		/* SGK units			   */


/* Device opcodes and parameters.  
 */
#define DEF_LEFT        0               /* origin in device pixels in x    */
#define DEF_BOTTOM      0               /* origin in device pixels in y    */
#define DEF_WIDTH       640             /* width in x (240d/i, 11" paper)  */
#define DEF_HEIGHT      480             /* height in y (240d/i, 8.5" paper)*/
#define DEF_PENWIDTH    1               /* origin in device pixels in x    */


/* Commands to setup SVG environment.
*/
static char *svg_prolog[] = {
    "<?xml version=\"1.0\" standalone=\"no\"?>\n",
    "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n",
    "    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n",
    NULL
};

/* Opcode instruction.
*/
struct sgi_inst {
    short	opcode;
    short	x, y;
};


int	pen_width   = DEF_PENWIDTH;	/* initial line width		*/
int	width 	    = DEF_WIDTH;	/* margins			*/
int	height 	    = DEF_HEIGHT;
char 	*bg 	    = "#FFF";		/* BG/FG colors			*/
char	*fg 	    = "#000";
char	*fill 	    = "#FFF";		/* vector fill color		*/
	

static void translate (FILE *in, FILE *out);

	

/*  MAIN -- Main entry point for SGI2UHPGL.  Optional arguments are device
**  window parameters and name of input file.
*/
int
main (int argc, char *argv[])
{
	FILE	*in;
	char	*infile = "stdin", *argp;
	int	argno;
        register char **lp;


	/* Process the command line.
	*/
        for (argno=1;  (argp = argv[argno]) != NULL;  argno++) {
            if (argp[0] == '-') {
                /* A window-control or pen width switch.
                 */
                switch (argp[1]) {
                case 'f':
		    if (argp[2] == 'g')
		        fg = argv[++argno];
		    else
		        fill = argv[++argno];
                    break;
                case 'b':
		    bg = argv[++argno];
                    break;
                case 'w': width     = atoi (argv[++argno]); break;
                case 'h': height    = atoi (argv[++argno]); break;
                case 'p': pen_width = atoi (argv[++argno]); break;
                default:
                    break;
                }

            } else {
                /* Input sgi-metacode file specification.
                 */
                infile = argp;
            }
        }


	/* Open the input file. */
	if (strcmp (infile, "stdin") == 0)
	    in = stdin;
	else
	    in = fopen (infile, "r");
	if (in == NULL) {
	    fprintf (stderr, "Fatal error (sgi2svg):  Cannot open `%s'\n",
		infile);
	    fflush (stderr);
	    exit (OSOK+1);
	}


	/*  Output the standard prolog.
	 */
        for (lp=svg_prolog;  *lp;  lp++)  	
            fputs (*lp, stdout);

	fprintf (stdout,"<svg width=\"%dpx\" height=\"%dpx\" version=\"1.1\"\n",
	    width, height);
	fprintf (stdout,"    xmlns=\"http://www.w3.org/2000/svg\">");
	fprintf (stdout,
    	    "<g id=\"g0\" fill=\"#%s\" stroke=\"#%s\" stroke-width=\"1\">\n",
	    bg, fg);
	fprintf (stdout,
    	    "<rect x=\"1\" y=\"1\" width=\"%d\" height=\"%d\"\n",
	    width, height);
	fprintf (stdout,
    	    "    fill=\"#%s\" stroke=\"#%s\" stroke-width=\"1\"/>\n", fill, bg);



	/*  Process the metacode.
	*/
	translate (in, stdout); 		


	/*  Clean up.
	*/
	fprintf (stdout, "</g></svg>");		/* output the end of file     */
	if (in != stdin)
	    fclose (in);

	return (0);
}


/* TRANSLATE -- Interpret input SGI metacode instructions into device 
** instructions and write to stdout.
*/
static void
translate (FILE *in, FILE *out)
{
	register struct sgi_inst *sgip;
	struct	 sgi_inst inbuf[LEN_MCBUF], *buftop;
	int	 n, in_stroke=0, swap_bytes = isSwapped();
	int 	 x, y, gnum=1;


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
		    fprintf (out, "\n");
		    break;

		case SGK_MOVE:
		    x = (int) (((float)sgip->x / GKI_MAXNDC) * width);
		    y = (int) (((float)sgip->y / GKI_MAXNDC) * height);
		    y = height - y + 1;
		    if (in_stroke) 			/* end current stroke */
			fprintf (out, "\"/>");

		    /* Begin a new output stroke.
		    */
		    fprintf (out, "<polyline points=\"%d,%d", x, y);
		    in_stroke = 1;
		    break;

		case SGK_DRAW:
		    x = (int) (((float)sgip->x / GKI_MAXNDC) * width);
		    y = (int) (((float)sgip->y / GKI_MAXNDC) * height);
		    y = height - y + 1;
		    fprintf (out, " %d,%d", x, y);
		    in_stroke = 1;
		    break;

		case SGK_SETLW:
		    /* Set pen width.
		     */
		    pen_width = sgip->x + 1;
		    if (in_stroke) 			/* end current stroke */
			fprintf (out, "\"/></g>");
		    fprintf (out, "<g id=\"g%d\" stroke-width=\"%d\">",
			gnum++, pen_width);
		    in_stroke = 0;
		    break;

		default:
		    fprintf (stderr, "sgi2svg: unrecognized sgi opcode %d\n",
			sgip->opcode);
		    break;
		}
	    }
	}

	/* Terminate plotting and exit.
	 */
	fprintf (out, " \"/></g>\n");
}
