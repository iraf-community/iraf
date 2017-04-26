/* 
 * TIFFIO.C -- Routines to save a simple TIFF 6.0 format file.  Loading is
 * not supported due to the complexity of the format.
 *
 *  	  writeTIFF (fa, data, w, h, ncolors, gray, r, g, b)
 */

#include <stdio.h>
#ifdef ULTRIX
#include <sys/types.h>
#endif
#include <unistd.h>


/* TIFF Tag description
 */
typedef struct {
        short   tag;
        short   type;
        int     count;
        int     offset;
} TiffTag;

typedef struct {
        unsigned short  red[256];
        unsigned short  green[256];
        unsigned short  blue[256];
} Colors;

static char    *h_order = "MM\0*";
static char    *l_order = "II*\0";

/* Tag Definitions */
#define IMAGEWIDTH      256
#define IMAGELENGTH     257
#define BITSPERSAMPLE   258
#define COMPRESSION     259
#define PHOTOMETRIC     262
#define STRIPOFFSETS    273
#define ROWSPERSTRIP    278
#define STRIPBYTECOUNT  279
#define XRESOLUTION     282
#define YRESOLUTION     283
#define RESOLUTIONUNIT  296
#define COLORMAP        320

#define MAXCOLORS	256

/* MONO returns total intensity of r,g,b components */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)  /*.33R+ .5G+ .17B*/



/* writeTIFF -- Write a TIFF 6.0 image.
*/

writeTIFF (fa, data, w, h, ncolors, gray, r, g, b)
FILE 	*fa;				/* output file descriptor	*/
unsigned char *data;			/* pixel data			*/
int w, h;				/* image dimensions		*/
int ncolors;				/* number of colors		*/
int gray;				/* grayscale?			*/
unsigned char *r, *g, *b;		/* colormap			*/
{
	int	i,size,offset;
	short   dirs = 12;
	TiffTag *tt;
	unsigned char	*dta, r1, b1, g1;
	unsigned short	*rr, *gg, *bb;
	int	bitspersample=8, compression=1, photo=3, resolution=2;
	int 	width = w, height = h;

	tt = (TiffTag *) calloc (dirs, sizeof(TiffTag));
	size = w * h;
	dta = (unsigned char *) calloc (size+(size%4), sizeof(unsigned char));
	rr = (unsigned short *) calloc (MAXCOLORS, sizeof(unsigned short));
	gg = (unsigned short *) calloc (MAXCOLORS, sizeof(unsigned short));
	bb = (unsigned short *) calloc (MAXCOLORS, sizeof(unsigned short));
	memcpy(dta, data, size * sizeof(unsigned char));

	if (is_swapped()) {
	    fwrite (l_order, 4, 1, fa);
	} else {
	    fwrite (h_order, 4, 1, fa);
	    width 	    = w << 16;
	    height 	    = h << 16;
	    bitspersample <<= 16;
	    compression   <<= 16;
	    photo         <<= 16;
	    resolution    <<= 16;
	}

	offset = 8;
	fwrite (&offset, 4, 1, fa);
	fwrite (&dirs, 2, 1, fa); 		/* number of IFD's */

	/* Create TIFF IFD's.
	 */
	create_TIFFtag (tt+0, IMAGEWIDTH, 3, 1, (width));
	create_TIFFtag (tt+1, IMAGELENGTH, 3, 1, (height));
	create_TIFFtag (tt+2, BITSPERSAMPLE, 3, 1, (bitspersample));
	create_TIFFtag (tt+3, COMPRESSION, 3, 1, (compression));
	create_TIFFtag (tt+4, PHOTOMETRIC, 3, 1, (photo));
	create_TIFFtag (tt+5, STRIPOFFSETS, 4, 1, 10+(dirs*12)+4);
	create_TIFFtag (tt+6, ROWSPERSTRIP, 3, 1, (width));
	create_TIFFtag (tt+7, STRIPBYTECOUNT, 4, 1, size);
	create_TIFFtag (tt+8, XRESOLUTION, 5, 1,
	    (tt+5)->offset+(2*768)+(size+(size%4)));
	create_TIFFtag (tt+9, YRESOLUTION, 5, 1,
	    (tt+5)->offset+(2*768)+(size+(size%4))+8);
	create_TIFFtag (tt+10, RESOLUTIONUNIT, 3, 1, (resolution));
	create_TIFFtag (tt+11, COLORMAP, 3, 768,
	    (tt+5)->offset+(size+(size%4)));

	/* Write header info and tags.
	 */
	for (i = 0 ; i < dirs ; i++) {
	    fwrite ((tt+i), 12, 1, fa);
	}

	/* Write a 4-byte NULL -- this tells the TIFF reader that there
	 * are no more IFD's.
	 */
	i = 0;
	fwrite (&i, 4, 1, fa);

	/* Write data.
	 */
	fwrite (dta, size+(size%4), 1, fa);

	/* Create unsigned short colormap.
	 */
	for (i = 0 ; i < ncolors ; i++) {
	    if (gray) {
		r1 = (unsigned char) MONO (*(r+i),*(g+i),*(b+i));
		g1 = (unsigned char) MONO (*(r+i),*(g+i),*(b+i));
		b1 = (unsigned char) MONO (*(r+i),*(g+i),*(b+i));
		*(r+i) = r1;
		*(g+i) = g1;
		*(b+i) = b1;
	    }
	    *(rr+i) = (unsigned short) *(r+i) | ((unsigned short) *(r+i) << 8);
	    *(gg+i) = (unsigned short) *(g+i) | ((unsigned short) *(g+i) << 8);
	    *(bb+i) = (unsigned short) *(b+i) | ((unsigned short) *(b+i) << 8);
	}

	/* Fill in the rest of the colortable with 0xffff's.
	 */
	for (i = ncolors ; i < MAXCOLORS ; i++) {
	    *(rr+i) = (unsigned short) 0xffff;
	    *(gg+i) = (unsigned short) 0xffff;
	    *(bb+i) = (unsigned short) 0xffff;
	}

	/* Write out colormap.
	 */
	fwrite (rr, MAXCOLORS*sizeof(unsigned short), 1, fa);
	fwrite (gg, MAXCOLORS*sizeof(unsigned short), 1, fa);
	fwrite (bb, MAXCOLORS*sizeof(unsigned short), 1, fa);

	/* Write the XResolution.
	 */
	i = 1;
	fwrite (&i, sizeof(int), 1, fa);
	fwrite (&i, sizeof(int), 1, fa);

	/* Write the YResolution.
	 */
	i = 1;
	fwrite (&i, sizeof(int), 1, fa);
	fwrite (&i, sizeof(int), 1, fa);

	/* Free memory.
	 */
	free (tt);
	free (dta);
	free (rr);
	free (gg);
	free (bb);
}


/* create_TIFFtag - Create a TIFF IFD (Image File Directory).
*/

create_TIFFtag (tag, desig, type, count, offset)
TiffTag *tag;				/* tag to create		*/
short desig;				/* tag definition		*/
short type;				/* scalar type of data items	*/
int count;				/* no. of items in tag data	*/
int offset;				/* byte offset to data items	*/
{
	tag->tag = desig;
	tag->type = type;
	tag->count = count;
	tag->offset = offset;
}
