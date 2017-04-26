#include <stdio.h>

/* QUANT.C -- Quantize the colors in a pixmap down to a specified number.
** This code is largely stolen from PBMPlus by Jef Poskanzer.
**
** ORIGINAL COPYRIGHT NOTICE:
**
** Copyright (C) 1989, 1991 by Jef Poskanzer.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
*/

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#undef abs
#define abs(a) ((a) >= 0 ? (a) : -(a))
#undef odd
#define odd(n) ((n) & 1)

#ifdef HPUX 
#define random()	rand()
#define srandom(x)	srand(x)
#endif

typedef unsigned char byte;
typedef unsigned char pixval;

#define PPM_MAXMAXVAL 255
typedef struct { pixval r, g, b; } pixel;

#define PPM_GETR(p) ((p).r)
#define PPM_GETG(p) ((p).g)
#define PPM_GETB(p) ((p).b)

#define PPM_ASSIGN(p,red,grn,blu) \
  { (p).r = (red); (p).g = (grn); (p).b = (blu); }

#define PPM_EQUAL(p,q) ( (p).r == (q).r && (p).g == (q).g && (p).b == (q).b )

/* Color scaling macro -- to make writing ppmtowhatever easier. */
#define PPM_DEPTH(newp,p,oldmaxval,newmaxval) \
    PPM_ASSIGN( (newp), \
                (int) PPM_GETR(p) * (newmaxval) / (oldmaxval), \
                (int) PPM_GETG(p) * (newmaxval) / (oldmaxval), \
                (int) PPM_GETB(p) * (newmaxval) / (oldmaxval) )

/* Luminance macro, using only integer ops.  Returns an int (*256)  JHB */
#define PPM_LUMIN(p) \
  ( 77 * PPM_GETR(p) + 150 * PPM_GETG(p) + 29 * PPM_GETB(p) )


/* Color histogram stuff. */
typedef struct colorhist_item *colorhist_vector;
struct colorhist_item {
	pixel color;
	int	value;
};

typedef struct colorhist_list_item *colorhist_list;
struct colorhist_list_item {
	struct colorhist_item ch;
	colorhist_list next;
};


/* Color hash table stuff. */

typedef colorhist_list	*colorhash_table;

#define MAXCOLORS 	32767
#define FS_SCALE 	1024
#define HASH_SIZE 20023

#define LARGE_LUM
#define REP_AVERAGE_PIXELS

#define hashPixel(p)  ((((int) PPM_GETR(p) * 33023 +    \
                            (int) PPM_GETG(p) * 30013 +    \
                            (int) PPM_GETB(p) * 27011) & 0x7fffffff)   \
                          % HASH_SIZE)


typedef struct box *box_vector;
struct box {
	int	ind;
	int	colors;
	int	sum;
};

static colorhist_vector medianCut (),  colorHashToColorHist ();
static colorhist_vector computeColorHist ();
static colorhash_table  computeColorHash (), colorHistToColorHash ();
static colorhash_table  allocColorHash ();
static int 		redCompare(), greenCompare(), blueCompare();
static int 		lookupColor (), addToColorHash (), sumCompare();
static void 		freeColorHash (), addToColorHist(),freeColorHist();



/* PPMQUANT --  Quantize an image with a given colormap to a new number of
 * colors, modifies the image in place.  Floyd-Steinberg dithering isn't
 * current enabled because of speed but can be turned on later.  Modified
 * from the original code in the PBMplus package by Jef Poskanzer.
 */

int
ppmquant (image, r, g, b, nx, ny, ncolors, newcolors)
byte 	*image;				/* image pixels (i.e. cmap indices) */
byte	*r, *g, *b;			/* colormap			    */
int  	nx, ny;				/* image dimensions                 */
int	ncolors;			/* current number of colors         */
int	newcolors;			/* requested number of colors       */
{
	register pixel *pP;
	register long	sr, sg, sb, err;
	register int	i, col, limitcol, index = 0;
	byte	 *pix = image;
	pixel    **pixels;
	pixval   maxval = 255, newmaxval;
	int	 rows, cols, row;
	int	 colors;
	int	 floyd = 0, usehash;
	colorhist_vector chv, colormap;
	colorhash_table cht;
	long	*thisrerr, *thisgerr, *thisberr;
	long	*nextrerr, *nextgerr, *nextberr, *temperr;
	int	fs_direction;

  	/* Reformat into 2-dimensional array of pixel structures */
	rows = ny;
	cols = nx;
  	pixels = (pixel **) malloc (rows * sizeof(pixel *));
  	if (!pixels) 
	    return;

  	for (row=0; row<rows; row++) {
    	    pixels[row] = (pixel *) malloc(cols * sizeof(pixel));
    	    if (!pixels[row])
		return;

    	    for (col=0, pP=pixels[row]; col<cols; col++, pP++) {
      	        pP->r = (pixval) r[*pix];
      	        pP->g = (pixval) g[*pix];
      	        pP->b = (pixval) b[*pix];
		pix++;
    	    }
  	}


	/* Step 2: attempt to make a histogram of the colors, unclustered.
	** If at first we don't succeed, lower maxval to increase color
	** coherence and try again.  This will eventually terminate, with
	** maxval at worst 15, since 32^3 is approximately MAXCOLORS.
	*/
	for  (; ;)  {
	    chv = computeColorHist(pixels, cols, rows, MAXCOLORS, &colors) ;
	    if  (chv != (colorhist_vector) 0) 
	        break;
	    newmaxval = maxval / 2;
	    for  (row = 0; row < rows; ++row) 
	        for  (col = 0, pP = pixels[row]; col < cols; ++col, ++pP) 
	            PPM_DEPTH (*pP, *pP, maxval, newmaxval) ;
	    maxval = newmaxval;
	}

	/* Step 3: apply median-cut to histogram, making the new colormap.
	*/
	colormap = medianCut (chv, colors, rows * cols, maxval, newcolors) ;
	freeColorHist (chv) ;

	/* Step 4: map the colors in the image to their closest match in the
    	** new colormap, and write 'em out.
    	*/
	cht = allocColorHash ();
	usehash = 1;
	pix = image;
	if  (floyd)  {
	    /* Initialize Floyd-Steinberg error vectors. */
	    thisrerr = (long *) calloc (cols + 2, sizeof(long) );
	    nextrerr = (long *) calloc (cols + 2, sizeof(long) );
	    thisgerr = (long *) calloc (cols + 2, sizeof(long) );
	    nextgerr = (long *) calloc (cols + 2, sizeof(long) );
	    thisberr = (long *) calloc (cols + 2, sizeof(long) );
	    nextberr = (long *) calloc (cols + 2, sizeof(long) );
	    srandom ((int)  (time (0)  ^ getpid ()) ) ;
	    for  (col = 0; col < cols + 2; ++col)  {
	    	thisrerr[col] = random () % (FS_SCALE * 2)  - FS_SCALE;
	    	thisgerr[col] = random () % (FS_SCALE * 2)  - FS_SCALE;
	    	thisberr[col] = random () % (FS_SCALE * 2)  - FS_SCALE;
	    	/* (random errors in [-1 .. 1]) */
	    }
	    fs_direction = 1;
	}
	for  (row = 0; row < rows; ++row)  {
	    if  (floyd) 
	    	for  (col = 0; col < cols + 2; ++col) 
	    	    nextrerr[col] = nextgerr[col] = nextberr[col] = 0;
	    if  ( (!floyd)  || fs_direction)  {
	    	col = 0;
	    	limitcol = cols;
	    	pP = pixels[row];
	    } else {
	    	col = cols - 1;
	    	limitcol = -1;
	    	pP = &(pixels[row][col]);
	    }
	    do {
	    	if  (floyd)  {
	    	    /* Use Floyd-Steinberg errors to adjust actual color. */
	    	    sr = PPM_GETR(*pP) + thisrerr[col + 1] / FS_SCALE;
	    	    sg = PPM_GETG(*pP) + thisgerr[col + 1] / FS_SCALE;
	    	    sb = PPM_GETB(*pP) + thisberr[col + 1] / FS_SCALE;
	    	    if  (sr < 0) 
	    	    	sr = 0;
	    	    else if  (sr > maxval) 
	    	    	sr = maxval;
	    	    if  (sg < 0) 
	    	    	sg = 0;
	    	    else if  (sg > maxval) 
	    	    	sg = maxval;
	    	    if  (sb < 0) 
	    	    	sb = 0;
	    	    else if  (sb > maxval) 
	    	    	sb = maxval;
	    	    PPM_ASSIGN (*pP, sr, sg, sb) ;
	    	}

	    	/* Check hash table to see if we have already matched this
		 * color. 
		 */
	    	index = lookupColor (cht, pP) ;
	    	if  (index == -1) { /* No; search colormap for closest match. */
	    	    register int	i, r1, g1, b1, r2, g2, b2;
	    	    register long	dist, newdist;
	    	    r1 = PPM_GETR (*pP) ;
	    	    g1 = PPM_GETG (*pP) ;
	    	    b1 = PPM_GETB (*pP) ;
	    	    dist = 2000000000;
	    	    for  (i = 0; i < newcolors; ++i)  {
	    	    	r2 = PPM_GETR (colormap[i].color) ;
	    	    	g2 = PPM_GETG (colormap[i].color) ;
	    	    	b2 = PPM_GETB (colormap[i].color) ;
	    	    	newdist =  (r1 - r2)  *  (r1 - r2)  + 
	    	    	     (g1 - g2)  *  (g1 - g2)  + 
	    	    	     (b1 - b2)  *  (b1 - b2) ;
	    	    	if  (newdist < dist)  {
	    	    	    index = i;
	    	    	    dist = newdist;
	    	    	}
	    	    }
	    	    if  (usehash)  {
	    	    	if  (addToColorHash (cht, pP, index)  < 0)  {
	    	    	    usehash = 0;
	    	    	}
	    	    }
	    	}

	    	if  (floyd)  {
	    	    /* Propagate Floyd-Steinberg error terms. */
	    	    if  (fs_direction)  {
	    	    	err =  (sr - (long) PPM_GETR (colormap[index].color)) * FS_SCALE;
	    	    	thisrerr[col + 2] +=  (err * 7)  / 16;
	    	    	nextrerr[col    ] +=  (err * 3)  / 16;
	    	    	nextrerr[col + 1] +=  (err * 5)  / 16;
	    	    	nextrerr[col + 2] +=  (err    )  / 16;
	    	    	err =  (sg - (long) PPM_GETG (colormap[index].color)) * FS_SCALE;
	    	    	thisgerr[col + 2] +=  (err * 7)  / 16;
	    	    	nextgerr[col    ] +=  (err * 3)  / 16;
	    	    	nextgerr[col + 1] +=  (err * 5)  / 16;
	    	    	nextgerr[col + 2] +=  (err    )  / 16;
	    	    	err =  (sb - (long) PPM_GETB (colormap[index].color)) * FS_SCALE;
	    	    	thisberr[col + 2] +=  (err * 7)  / 16;
	    	    	nextberr[col    ] +=  (err * 3)  / 16;
	    	    	nextberr[col + 1] +=  (err * 5)  / 16;
	    	    	nextberr[col + 2] +=  (err    )  / 16;
	    	    } else {

	    	    	err =  (sr - (long) PPM_GETR (colormap[index].color)) * FS_SCALE;
	    	    	thisrerr[col    ] +=  (err * 7)  / 16;
	    	    	nextrerr[col + 2] +=  (err * 3)  / 16;
	    	    	nextrerr[col + 1] +=  (err * 5)  / 16;
	    	    	nextrerr[col    ] +=  (err    )  / 16;
	    	    	err =  (sg - (long) PPM_GETG (colormap[index].color)) * FS_SCALE;
	    	    	thisgerr[col    ] +=  (err * 7)  / 16;
	    	    	nextgerr[col + 2] +=  (err * 3)  / 16;
	    	    	nextgerr[col + 1] +=  (err * 5)  / 16;
	    	    	nextgerr[col    ] +=  (err    )  / 16;
	    	    	err =  (sb - (long) PPM_GETB (colormap[index].color)) * FS_SCALE;
	    	    	thisberr[col    ] +=  (err * 7)  / 16;
	    	    	nextberr[col + 2] +=  (err * 3)  / 16;
	    	    	nextberr[col + 1] +=  (err * 5)  / 16;
	    	    	nextberr[col    ] +=  (err    )  / 16;
	    	    }
	    	}

	    	*pP = colormap[index].color;
		*pix++ = index;			  /* save the new image index */

	    	if  ( (!floyd)  || fs_direction)  {
	    	    ++col;
	    	    ++pP;
	    	} else {
	    	    --col;
	    	    --pP;
	    	}
	    } while  (col != limitcol) ;

	    if  (floyd)  {
	    	temperr = thisrerr;
	    	thisrerr = nextrerr;
	    	nextrerr = temperr;
	    	temperr = thisgerr;
	    	thisgerr = nextgerr;
	    	nextgerr = temperr;
	    	temperr = thisberr;
	    	thisberr = nextberr;
	    	nextberr = temperr;
	    	fs_direction = !fs_direction;
	    }

	}

  	/* Rescale and load the new colormap. */
  	for (i=0; i<newcolors; i++) {
    	    PPM_DEPTH(colormap[i].color, colormap[i].color, maxval, 255);
    	    r[i] = PPM_GETR (colormap[i].color);
    	    g[i] = PPM_GETG (colormap[i].color);
    	    b[i] = PPM_GETB (colormap[i].color);
  	}

  	/* Free the pixels array. */
  	for (i=0; i<rows; i++) free (pixels[i]);
  	free (pixels);

  	/* Free cht and colormap. */
  	freeColorHist (colormap);
  	freeColorHash (cht);

	return (0) ;
}


/*
** Here is the fun part, the median-cut colormap generator.  This is based
** on Paul Heckbert's paper "Color Image Quantization for Frame Buffer
** Display", SIGGRAPH '82 Proceedings, page 297.
*/

static colorhist_vector
medianCut (chv, colors, sum, maxval, newcolors) 
colorhist_vector chv;
int	colors, sum, newcolors;
pixval maxval;
{
	colorhist_vector colormap;
	box_vector bv;
	register int	bi, i;
	int	boxes;

	bv = (box_vector) malloc (sizeof(struct box)  * newcolors) ;
	colormap = 
	    (colorhist_vector) malloc (sizeof(struct colorhist_item)  * newcolors) ;
	if  (bv == (box_vector) 0 || colormap == (colorhist_vector) 0) 
	    perror ("out of memory") ;
	for  (i = 0; i < newcolors; ++i) 
	    PPM_ASSIGN (colormap[i].color, 0, 0, 0) ;

	/*
    	** Set up the initial box.
    	*/
	bv[0].ind = 0;
	bv[0].colors = colors;
	bv[0].sum = sum;
	boxes = 1;

	/*
    	** Main loop: split boxes until we have enough.
    	*/
	while  (boxes < newcolors)  {
	    register int	indx, clrs;
	    int	sm;
	    register int	minr, maxr, ming, maxg, minb, maxb, v;
	    int	halfsum, lowersum;

	    /*
	    ** Find the first splittable box.
	    */
	    for  (bi = 0; bi < boxes; ++bi) 
	    	if  (bv[bi].colors >= 2) 
	    	    break;
	    if  (bi == boxes) 
	    	break;	/* ran out of colors! */
	    indx = bv[bi].ind;
	    clrs = bv[bi].colors;
	    sm = bv[bi].sum;

	    /*
	    ** Go through the box finding the minimum and maximum of each
	    ** component - the boundaries of the box.
	    */
	    minr = maxr = PPM_GETR (chv[indx].color) ;
	    ming = maxg = PPM_GETG (chv[indx].color) ;
	    minb = maxb = PPM_GETB (chv[indx].color) ;
	    for  (i = 1; i < clrs; ++i)  {
	    	v = PPM_GETR (chv[indx + i].color) ;
	    	if  (v < minr) 
	    	    minr = v;
	    	if  (v > maxr) 
	    	    maxr = v;
	    	v = PPM_GETG (chv[indx + i].color) ;
	    	if  (v < ming) 
	    	    ming = v;
	    	if  (v > maxg) 
	    	    maxg = v;
	    	v = PPM_GETB (chv[indx + i].color) ;
	    	if  (v < minb) 
	    	    minb = v;
	    	if  (v > maxb) 
	    	    maxb = v;
	    }

	    /*
	    ** Find the largest dimension, and sort by that component.  I have
	    ** included two methods for determining the "largest" dimension;
	    ** first by simply comparing the range in RGB space, and second
	    ** by transforming into luminosities before the comparison.  You
	    ** can switch which method is used by switching the commenting on
	    ** the LARGE_ defines at the beginning of this source file.
	    */
	     {
      		/* LARGE_LUM version */

	    	pixel p;
	    	float	rl, gl, bl;

	    	PPM_ASSIGN(p, maxr - minr, 0, 0);
	    	rl = PPM_LUMIN(p);
	    	PPM_ASSIGN(p, 0, maxg - ming, 0);
	    	gl = PPM_LUMIN(p);
	    	PPM_ASSIGN(p, 0, 0, maxb - minb);
	    	bl = PPM_LUMIN(p);

	    	if  (rl >= gl && rl >= bl) 
	    	    qsort(
	    	        (char *)&(chv[indx]),clrs,sizeof(struct colorhist_item),
	    	        redCompare) ;
	    	else if  (gl >= bl) 
	    	    qsort(
	    	        (char *)&(chv[indx]),clrs,sizeof(struct colorhist_item),
	    	        greenCompare) ;
	    	else
	    	    qsort(
	    	        (char *)&(chv[indx]),clrs,sizeof(struct colorhist_item),
	    	        blueCompare) ;
	    }

	    /*
	    ** Now find the median based on the counts, so that about half the
	    ** pixels (not colors, pixels) are in each subdivision.
	    */
	    lowersum = chv[indx].value;
	    halfsum = sm / 2;
	    for  (i = 1; i < clrs - 1; ++i)  {
	    	if  (lowersum >= halfsum) 
	    	    break;
	    	lowersum += chv[indx + i].value;
	    }

	    /*
	    ** Split the box, and sort to bring the biggest boxes to the top.
	    */
	    bv[bi].colors = i;
	    bv[bi].sum = lowersum;
	    bv[boxes].ind = indx + i;
	    bv[boxes].colors = clrs - i;
	    bv[boxes].sum = sm - lowersum;
	    ++boxes;
	    qsort ((char *) bv, boxes, sizeof(struct box) , sumCompare) ;
	}

	/*
        ** Ok, we've got enough boxes.  Now choose a representative color for
        ** each box.  There are a number of possible ways to make this choice.
        ** One would be to choose the center of the box; this ignores any
        ** structure within the boxes.  Another method would be to average all
	** the colors in the box - this is the method specified in Heckbert's
	** paper.  A third method is to average all the pixels in the box. You
	** can switch which method is used by switching the commenting on the
	** REP_ defines at the beginning of this source file.
        */
	for  (bi = 0; bi < boxes; ++bi)  {
    	    /* REP_AVERAGE_PIXELS version */

	    register int	indx = bv[bi].ind;
	    register int	clrs = bv[bi].colors;
	    register long	r = 0, g = 0, b = 0, sum = 0;

	    for  (i = 0; i < clrs; ++i)  {
	    	r += PPM_GETR (chv[indx + i].color)  * chv[indx + i].value;
	    	g += PPM_GETG (chv[indx + i].color)  * chv[indx + i].value;
	    	b += PPM_GETB (chv[indx + i].color)  * chv[indx + i].value;
	    	sum += chv[indx + i].value;
	    }
	    r = r / sum;
	    if  (r > maxval) 
	    	r = maxval;	/* avoid math errors */
	    g = g / sum;
	    if  (g > maxval) 
	    	g = maxval;
	    b = b / sum;
	    if  (b > maxval) 
	    	b = maxval;
	    PPM_ASSIGN (colormap[bi].color, r, g, b) ;
	}

	/* All done.  */
	return colormap;
}


static int	
redCompare (ch1, ch2) 
colorhist_vector ch1, ch2;
{
	return (int) PPM_GETR (ch1->color)  - (int) PPM_GETR (ch2->color) ;
}


static int	
greenCompare (ch1, ch2) 
colorhist_vector ch1, ch2;
{
	return (int) PPM_GETG (ch1->color)  - (int) PPM_GETG (ch2->color) ;
}


static int	
blueCompare (ch1, ch2) 
colorhist_vector ch1, ch2;
{
	return (int) PPM_GETB (ch1->color)  - (int) PPM_GETB (ch2->color) ;
}


static int	
sumCompare (b1, b2) 
box_vector b1, b2;
{
	return b2->sum - b1->sum;
}


static colorhist_vector
computeColorHist (pixels, cols, rows, maxcolors, colorsP) 
pixel**pixels;
int	cols, rows, maxcolors;
int*colorsP;
{
	colorhash_table cht;
	colorhist_vector chv;

	cht = computeColorHash (pixels, cols, rows, maxcolors, colorsP) ;
	if  (cht == (colorhash_table) 0) 
	    return (colorhist_vector) 0;
	chv = colorHashToColorHist (cht, maxcolors) ;
	freeColorHash (cht) ;
	return chv;
}


static void
addToColorHist (chv, colorsP, maxcolors, colorP, value, position) 
colorhist_vector chv;
pixel*colorP;
int*colorsP;
int	maxcolors, value, position;
{
	int	i, j;

	/* Search colorhist for the color. */
	for  (i = 0; i < *colorsP; ++i) 
	    if  (PPM_EQUAL (chv[i].color, *colorP) )  {
	    	/* Found it - move to new slot. */
	    	if  (position > i)  {
	    	    for  (j = i; j < position; ++j) 
	    	    	chv[j] = chv[j + 1];
	    	} else if  (position < i)  {
	    	    for  (j = i; j > position; --j) 
	    	    	chv[j] = chv[j - 1];
	    	}
	    	chv[position].color = *colorP;
	    	chv[position].value = value;
	    	return;
	    }
	if  (*colorsP < maxcolors)  {
	    /* Didn't find it, but there's room to add it; so do so. */
	    for  (i = *colorsP; i > position; --i) 
	    	chv[i] = chv[i - 1];
	    chv[position].color = *colorP;
	    chv[position].value = value;
	    ++(*colorsP);
	}
}


static colorhash_table
computeColorHash (pixels, cols, rows, maxcolors, colorsP) 
pixel**pixels;
int	cols, rows, maxcolors;
int*colorsP;
{
	colorhash_table cht;
	register pixel*pP;
	colorhist_list chl;
	int	col, row, hash;

	cht = allocColorHash ();
	*colorsP = 0;

	/* Go through the entire image, building a hash table of colors. */
	for  (row = 0; row < rows; ++row) 
	    for  (col = 0, pP = pixels[row]; col < cols; ++col, ++pP)  {
	    	hash = hashPixel (*pP) ;
	    	for  (chl=cht[hash]; chl != (colorhist_list) 0; chl=chl->next) 
	    	    if  (PPM_EQUAL (chl->ch.color, *pP) ) 
	    	    	break;
	    	if  (chl != (colorhist_list) 0) 
	    	    ++(chl->ch.value);
	    	else
	    	 {
	    	    if  (++(*colorsP) > maxcolors)  {
	    	    	freeColorHash (cht) ;
	    	    	return (colorhash_table) 0;
	    	    }
	    	    chl = (colorhist_list) 
			malloc (sizeof(struct colorhist_list_item));
	    	    if  (chl == 0) 
	    	    	perror ("out of memory computing hash table") ;
	    	    chl->ch.color = *pP;
	    	    chl->ch.value = 1;
	    	    chl->next = cht[hash];
	    	    cht[hash] = chl;
	    	}
	    }

	return cht;
}


static colorhash_table
allocColorHash ()
{
	colorhash_table cht;
	int	i;

	cht = (colorhash_table) malloc (HASH_SIZE * sizeof(colorhist_list)) ;
	if  (cht == 0) 
	    perror ("out of memory allocating hash table") ;

	for  (i = 0; i < HASH_SIZE; ++i) 
	    cht[i] = (colorhist_list) 0;

	return cht;
}


static int
addToColorHash (cht, colorP, value) 
colorhash_table cht;
pixel*colorP;
int	value;
{
	register int	hash;
	register colorhist_list chl;

	chl = (colorhist_list) malloc (sizeof(struct colorhist_list_item) ) ;
	if  (chl == 0) 
	    return - 1;
	hash = hashPixel (*colorP) ;
	chl->ch.color = *colorP;
	chl->ch.value = value;
	chl->next = cht[hash];
	cht[hash] = chl;
	return 0;
}


static colorhist_vector
colorHashToColorHist (cht, maxcolors) 
colorhash_table cht;
int	maxcolors;
{
	colorhist_vector chv;
	colorhist_list chl;
	int	i, j;

	/* Now collate the hash table into a simple colorhist array. */
	chv = (colorhist_vector) malloc (maxcolors * sizeof(struct colorhist_item) ) ;
	/* (Leave room for expansion by caller.) */
	if  (chv == (colorhist_vector) 0) 
	    perror ("out of memory generating histogram") ;

	/* Loop through the hash table. */
	j = 0;
	for  (i = 0; i < HASH_SIZE; ++i) 
	    for  (chl = cht[i]; chl != (colorhist_list) 0; chl = chl->next)  {
	    	/* Add the new entry. */
	    	chv[j] = chl->ch;
	    	++j;
	    }

	/* All done. */
	return chv;
}


static colorhash_table
colorHistToColorHash (chv, colors) 
colorhist_vector chv;
int	colors;
{
	colorhash_table cht;
	int	i, hash;
	pixel color;
	colorhist_list chl;

	cht = allocColorHash ();

	for  (i = 0; i < colors; ++i)  {
	    color = chv[i].color;
	    hash = hashPixel (color) ;
	    for  (chl = cht[hash]; chl != (colorhist_list) 0; chl = chl->next) 
/*
	    	if  (PPM_EQUAL (chl->ch.color, color) ) 
	    	    pm_error(
	    	        "same color found twice - %d %d %d", PPM_GETR(color),
	    	        PPM_GETG(color), PPM_GETB(color)) ;
*/
	    chl = (colorhist_list) malloc (sizeof(struct colorhist_list_item) ) ;
	    if  (chl == (colorhist_list) 0) 
	    	perror ("out of memory") ;
	    chl->ch.color = color;
	    chl->ch.value = i;
	    chl->next = cht[hash];
	    cht[hash] = chl;
	}

	return cht;
}


static int
lookupColor (cht, colorP) 
colorhash_table cht;
pixel*colorP;
{
	int	hash;
	colorhist_list chl;

	hash = hashPixel (*colorP) ;
	for  (chl = cht[hash]; chl != (colorhist_list) 0; chl = chl->next) 
	    if  (PPM_EQUAL (chl->ch.color, *colorP) ) 
	    	return chl->ch.value;

	return - 1;
}


static void
freeColorHist (chv) 
colorhist_vector chv;
{
	free ((char *) chv) ;
}


static void
freeColorHash (cht) 
colorhash_table cht;
{
	int	i;
	colorhist_list chl, chlnext;

	for  (i = 0; i < HASH_SIZE; ++i) 
	    for  (chl = cht[i]; chl != (colorhist_list) 0; chl = chlnext)  {
	    	chlnext = chl->next;
	    	free ((char *) chl) ;
	    }
	free ((char *) cht) ;
}
