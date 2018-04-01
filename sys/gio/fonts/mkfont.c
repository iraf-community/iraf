#include <stdio.h>

#define	DEBUG		0
#define	MASK		0x3F
#define	SCALE		1.0

#define max(a,b)		(a > b ? a : b)
#define min(a,b)		(a < b ? a : b)
/*
#define	XCOORD()		((*dp - 'R') + 9)
#define	YCOORD()		(('R' - *dp) + 14)
#define	XCOORD()	(max(0,min(20,((int)(((*dp - 'R') + 9)*SCALE+0.5)))))
#define	XCOORD()	(max(0,min(20,((int)(((*dp - 'R') - minx)*SCALE+0.5)))))
#define	YCOORD()	((int)((('R' - *dp) + 13) * SCALE + 0.5))
#define	XCOORD()	((int)(((*dp - 'R') - minx) * SCALE + 0.5))
#define	YCOORD()	(max(0,min(32,(((int)(('R' - *dp) + 13)*SCALE+0.5)))))
*/
#define	XCOORD()	(max(0,((int)(((*dp - 'R') - minx - 2) * SCALE + 0.5))))
#define	YCOORD()	(max(0,min(35,(((int)(('R' - *dp) + 16)*SCALE+0.5)))))
#define ENCODE(pen,x,y) ((int)(((pen<<12)|((x&MASK)<<6))|(y&MASK)))

int 	chridx[200];		/* Character index table	*/
int 	chrwid[200];		/* Character width table	*/
int 	chrtab[5000];		/* Character stroke table	*/


struct hershey_tab {
    int   num;                  /* hershey number       */
    int   length;               /* length               */
    char  *code;                /* stroke data string   */
} htab[] = {
#include "hershey.dat"
};

int	encode();


int 
main (int argc, char *argv[])
{
	register int i=0;
	int	minx, maxx, charnum=0, idx=0, hnum, hindex, hlength;
	short	x, y, pen, xspace, yspace;
	char 	ch, *dp, *data;

	/* Read all the hershey numbers from standard input and build up a
	 * table of stroke data.
	 */
	ch = 32;
	while (scanf ("%d", &hnum) != EOF) {

	    chridx[charnum] = idx + 1;

	    /* Get the index for the given number. */
	    for (hindex=0; hnum != htab[hindex].num; hindex++)
		;

	    hlength = htab[hindex].length;
	    dp = data = htab[hindex].code;

	    if (DEBUG)
		printf ("'%c' %4d: index=%4d len=%3d dlen=%3d %s\n",
		    ch, hnum, hindex, hlength, strlen(data),
		    (strlen(data) % 2) ? "ERROR" : "");

	    /* Now decode the stroke data into X-Y pairs, first pair is for
	     * proportional spacing.
	     */
	    minx = (*dp - 'R'); dp++;
	    maxx = (*dp - 'R'); dp++;
	    chrwid[charnum++] = min (32, maxx - minx + 5);

	    if (DEBUG) printf("\twidth (%02d) (%d,%d)\n", maxx-minx,minx,maxx);

	    /* Next pair is the initial move.  The Y coords are flipped
	     * for what we need so fix that every place we get a Yval.
	     */
	    pen = 0;
	    x = XCOORD(); dp++;
x = (ch == '1' ? x-3: x);
	    y = YCOORD(); dp++;
	    chrtab[idx++] = ENCODE(pen, x, y);

	    if (DEBUG) printf ("\tmove (%3d,%3d) '%s'\n", x, y, dp);

	    /* The remainder of the codes are move/draw strokes.
	     */
	    for (i=0; i < (hlength-2); i++) {
		if (*dp == ' ') {
		    pen = 0;
		    x = XCOORD(); dp++;	/* skip pen-up coords */
x = (ch == '1' ? x-3: x);
		    y = YCOORD(); dp++;
		    i++;
		} else
		    pen = 1;
		x = XCOORD(); dp++;
x = (ch == '1' ? x-3: x);
		y = YCOORD(); dp++;

		chrtab[idx++] = ENCODE(pen, x, y);

	        if (DEBUG) 
		    printf("\t%s (%3d,%3d) => %6d\n",
			pen?"draw":"move", x, y, ENCODE(pen,x,y));
	    }
	    chrtab[idx++] = ENCODE(0, 0, 0);
	    ch++;
	}

	print_prologue (charnum, idx);
	print_index (chridx, charnum);
	printf ("\n\n# Width data.\n\n");
	print_widths (chrwid, charnum);
	printf ("\n\n# Stroke data.\n\n");
	print_strokes (chrtab, idx);
}


int 
print_index (int *idxtab, int N)
{
	register int i, j, start=1, end=5;

	for (i=0; i < N; ) {
	    printf ("data    (chridx(i), i=%03d,%03d) /", start, min(N,end));
	    for (j=0; j < 5 && i < N; j++)
		printf ("%5d%c", idxtab[i++], (j<4 && i<N ? ',' : '/'));
	    printf ("\n");
	    start = end + 1;
	    end += 5;
	}
}


int 
print_widths (int *wtab, int N)
{
	register int i, j, start=1, end=5;

	for (i=0; i < N; ) {
	    printf ("data    (chrwid(i), i=%03d,%03d) /", start, min(N,end));
	    for (j=0; j < 5 && i < N; j++)
		printf ("%5d%c", wtab[i++], (j<4 && i<N ? ',' : '/'));
	    printf ("\n");
	    start = end + 1;
	    end += 5;
	}
}


int 
print_strokes (int *strtab, int N)
{
	register int i, j, start=1, end=5;

	for (i=0; i < N; ) {
	    printf ("data    (chrtab(i), i=%04d,%04d) /", start, min(N,end));
	    for (j=0; j < 5 && i < N; j++)
		printf ("%6d%c", strtab[i++], (j<4 && i<N ? ',' : '/'));
	    printf ("\n");
	    start = end + 1;
	    end += 5;
	}
}


int 
print_prologue (int nidx, int nchar)
{

printf ("# CHRTAB -- Table of strokes for the printable ASCII characters.  Each\n");
printf ("# character is encoded as a series of strokes.  Each stroke is ex-\n");
printf ("# pressed by a single integer containing the following bitfields:\n");
printf ("#\n");
printf ("#       2                   1\n");
printf ("#       0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1\n");
printf ("#                 | | | |         | |         |\n");
printf ("#                 | | | +---------+ +---------+\n");
printf ("#                 | | |      |           |\n");
printf ("#                 | | |      X           Y\n");
printf ("#                 | | |\n");
printf ("#                 | | +-- pen up/down\n");
printf ("#                 | +---- begin paint (not used at present)\n");
printf ("#                 +------ end paint (not used at present)\n");
printf ("#\n");
printf ("#----------------------------------------------------------------------------\n");
printf ("\n");
printf ("# Define the database.\n");
printf ("\n");
printf ("short   chridx[%d]\t# character index in chrtab\n", nidx+1);
printf ("short   chrwid[%d]\t# character width table\n", nidx+1);
printf ("short   chrtab[%d]\t# stroke data to draw the characters\n", nchar+1);
printf ("\n");
printf ("# Index into CHRTAB of each printable character (starting with SP)\n");
printf ("\n");
}
