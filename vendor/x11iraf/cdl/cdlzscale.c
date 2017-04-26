#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"


/*
 * ZSCALE -- Compute the optimal Z1, Z2 (range of greyscale values to be
 * displayed) of an image.  For efficiency a statistical subsample of an image
 * is used.  The pixel sample evenly subsamples the image in x and y.  The
 * entire image is used if the number of pixels in the image is smaller than
 * the desired sample.
 *
 * The sample is accumulated in a buffer and sorted by greyscale value.
 * The median value is the central value of the sorted array.  The slope of a
 * straight line fitted to the sorted sample is a measure of the standard
 * deviation of the sample about the median value.  Our algorithm is to sort
 * the sample and perform an iterative fit of a straight line to the sample,
 * using pixel rejection to omit gross deviants near the endpoints.  The fitted
 * straight line is the transfer function used to map image Z into display Z.
 * If more than half the pixels are rejected the full range is used.  The slope
 * of the fitted line is divided by the user-supplied contrast factor and the
 * final Z1 and Z2 are computed, taking the origin of the fitted line at the
 * median value.
 */

#define	MIN_NPIXELS	5	     /* smallest permissible sample 	     */
#define	MAX_REJECT	0.5	     /* max frac. of pixels to be rejected   */
#define	GOOD_PIXEL	0	     /* use pixel in fit                     */
#define	BAD_PIXEL	1	     /* ignore pixel in all computations     */
#define	REJECT_PIXEL	2	     /* reject pixel after a bit             */
#define	KREJ		2.5	     /* k-sigma pixel rejection factor       */
#define	MAX_ITERATIONS	5	     /* maximum number of fitline iterations */

#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b)) 
#undef mod
#define mod(a,b) ((a) % (b))
#undef nint
#define nint(a) ((int)(a + 0.5))
#undef abs
#define abs(a) ((a) >= 0 ? (a) : -(a))


extern	int	cdl_debug;

#ifdef ANSI_FUNC

void cdl_zscale(unsigned char *im, int nx, int ny, int bitpix, float *z1, float *z2, float contrast, int opt_size, int len_stdline);
int sampleImage(unsigned char *im, int bitpix, float **sample, int nx, int ny, int optimal_size, int len_stdline);

static void subSample(float *a, float *b, int npix, int step);
int fitLine(float *data, int npix, float *zstart, float *zslope, float krej, int ngrow, int maxiter);
static void flattenData(float *data, float *flat, float *x, int npix, double
z0, double dz);
int computeSigma(float *a, char *badpix, int npix, double *mean, double *sigma);
int rejectPixels(float *data, float *flat, float *normx, char *badpix, int npix, double *sumxsqr, double *sumxz, double *sumx, double *sumz, double threshold, int ngrow);
int floatCompare(float *i, float *j);

#else

int		rejectPixels(), computeSigma();
int		sampleImage(), fitLine(), floatCompare();

static void	flattenData();
static void 	subSample();

#endif

/* Compatibility hacks. */
#ifdef AUX

#ifdef ANSI_FUNC
void * memmove (void *a, const void *b, size_t n)
#else
void *memmove(a,b,n) void *a; const void *b; size_t n;
#endif

 { bcopy(b,a,n); }

#else

#if defined(sun) && !defined(SYSV)

#ifdef ANSI_FUNC
void * memmove (void *a, void *b, int n)
#else
void *memmove(a,b,n) void *a, *b; int n;
#endif

 { bcopy(b,a,n); }
#endif
#endif



/* CDL_ZSCALE -- Sample the image and compute optimal Z1 and Z2 values.
 */

#ifdef ANSI_FUNC

void 
cdl_zscale (
    unsigned char *im,		/* image data to be sampled		*/
    int nx,
    int ny,			/* image dimensions			*/
    int bitpix,			/* bits per pixel			*/
    float *z1,
    float *z2,			/* output min and max greyscale values	*/
    float contrast,		/* adj. to slope of transfer function	*/
    int opt_size,		/* desired number of pixels in sample	*/
    int len_stdline		/* optimal number of pixels per line	*/
)
#else

void
cdl_zscale (im, nx, ny, bitpix, z1, z2, contrast, opt_size, len_stdline)

unsigned char    *im;		/* image data to be sampled		*/
int	nx, ny;			/* image dimensions			*/
int	bitpix;			/* bits per pixel			*/
float	*z1, *z2;		/* output min and max greyscale values	*/
float	contrast;		/* adj. to slope of transfer function	*/
int	opt_size;		/* desired number of pixels in sample	*/
int	len_stdline;		/* optimal number of pixels per line	*/
#endif
{
	register int  npix, minpix, ngoodpix, center_pixel, ngrow;
	float	zmin, zmax, median;
	float	zstart, zslope;
	float 	*sample = NULL, *left = NULL;


	if (cdl_debug)
	    printf ("[cdl_zscale] %dx%d-%d  cont=%g optsz=%d len=%d\n",
		nx, ny, bitpix, contrast, opt_size, len_stdline);

	/* Subsample the image. */
	npix = sampleImage((unsigned char *)im, bitpix, &sample, nx, ny,
	    opt_size, len_stdline);

	/* Sort the sample, compute the minimum, maximum, and median pixel
	 * values.
	 */
	qsort (sample, npix, sizeof (float), floatCompare);
	zmin = *sample;
	zmax = *(sample+npix-1);

	/* The median value is the average of the two central values if there 
	 * are an even number of pixels in the sample.
	 */
	center_pixel = max (1, (npix + 1) / 2);
	left = &(sample[center_pixel - 1]);
	if (mod (npix, 2) == 1 || center_pixel >= npix)
	    median = *left;
	else
	    median = (*left + *(left+1)) / 2;

	/* Fit a line to the sorted sample vector.  If more than half of the
	 * pixels in the sample are rejected give up and return the full range.
	 * If the user-supplied contrast factor is not 1.0 adjust the scale
	 * accordingly and compute Z1 and Z2, the y intercepts at indices 1 and
	 * npix.
	 */
	minpix = max (MIN_NPIXELS, (int) (npix * MAX_REJECT));
	ngrow = max (1, nint (npix * .01));
	ngoodpix = fitLine (sample, npix, &zstart, &zslope,
	    KREJ, ngrow, MAX_ITERATIONS);

	if (ngoodpix < minpix) {
	    *z1 = zmin;
	    *z2 = zmax;
	} else {
	    if (contrast > 0)
		zslope = zslope / contrast;
	    *z1 = max (zmin, median - (center_pixel - 1) * zslope);
	    *z2 = min (zmax, median + (npix - center_pixel) * zslope);
	}

	if (cdl_debug) {
	    printf("[cdl_zscale] zmin=%g zmax=%g left=%g median=%g\n",
		zmin, zmax, *left, median);
	    printf("[cdl_zscale] minpix=%d ngrow=%d ngoodpix=%d\n",
		minpix, ngrow, ngoodpix);
	    printf("[cdl_zscale] zslope=%g center_pix=%d z1=%g z2=%g\n",
		zslope, center_pixel, *z1, *z2);
	}

	/* Clean up. */
	free ((float *)sample);
}


/* sampleImage -- Extract an evenly gridded subsample of the pixels from
 * a two-dimensional image into a one-dimensional vector.
 */

#ifdef ANSI_FUNC

int 
sampleImage (
    unsigned char *im,		/* image to be sampled			*/
    int bitpix,			/* bits per pixel in image		*/
    float **sample,		/* output vector containing the sample	*/
    int nx,
    int ny,			/* image dimensions			*/
    int optimal_size,		/* desired number of pixels in sample	*/
    int len_stdline		/* optimal number of pixels per line	*/
)
#else

int 
sampleImage (im, bitpix, sample, nx, ny, optimal_size, len_stdline)

unsigned char	*im;		/* image to be sampled			*/
int	bitpix;			/* bits per pixel in image		*/
float	**sample;		/* output vector containing the sample	*/
int	nx, ny;			/* image dimensions			*/
int	optimal_size;		/* desired number of pixels in sample	*/
int	len_stdline;		/* optimal number of pixels per line	*/
#endif
{
	register int i;
	int ncols, nlines, col_step, line_step, maxpix, line;
	int opt_npix_per_line, npix_per_line, npix = 0;
	int opt_nlines_in_sample, min_nlines_in_sample, max_nlines_in_sample;
	float	*op = NULL, *row = NULL;
        int     *ipix = NULL;
        float   *fpix = NULL;
        double  *dpix = NULL;
        short   *spix = NULL;
        char    *bpix = NULL;


	ncols  = nx;
	nlines = ny;

	/* Compute the number of pixels each line will contribute to the sample,
	 * and the subsampling step size for a line.  The sampling grid must
	 * span the whole line on a uniform grid.
	 */
	opt_npix_per_line = max (1, min (ncols, len_stdline));
	col_step = max (2, (ncols + opt_npix_per_line-1) / opt_npix_per_line);
	npix_per_line = max (1, (ncols + col_step-1) / col_step);
	if (cdl_debug)
	    printf ("[sampleImage] opt_npix/line=%d col_step=%d n/line=%d\n",
		opt_npix_per_line, col_step, npix_per_line);

	/* Compute the number of lines to sample and the spacing between lines.
	 * We must ensure that the image is adequately sampled despite its
	 * size, hence there is a lower limit on the number of lines in the
	 * sample.  We also want to minimize the number of lines accessed when
	 * accessing a large image, because each disk seek and read is ex-
	 * pensive. The number of lines extracted will be roughly the sample
 	 * size divided by len_stdline, possibly more if the lines are very
 	 * short.
	 */
	min_nlines_in_sample = max (1, optimal_size / len_stdline);
	opt_nlines_in_sample = max(min_nlines_in_sample, min(nlines,
	    (optimal_size + npix_per_line-1) / npix_per_line));
	line_step = max (2, nlines / (opt_nlines_in_sample));
	max_nlines_in_sample = (nlines + line_step-1) / line_step;
	if (cdl_debug)
	    printf ("[sampleImage] nl_in_samp=%d/%d opt_nl/samp=%d lstep=%d\n",
		min_nlines_in_sample, opt_nlines_in_sample, line_step,
		max_nlines_in_sample);

	/* Allocate space for the output vector.  Buffer must be freed by our
	 * caller.
	 */
	maxpix = npix_per_line * max_nlines_in_sample;
	*sample = (float *) malloc (maxpix * sizeof (float));
	row = (float *) malloc (nx * sizeof (float));

	/* Extract the vector. */
	op = *sample;
	for (line = (line_step + 1)/2; line < nlines; line+=line_step) {
	    /* Load a row of float values from the image */
            switch (bitpix) {
            case 8:
		bpix = (char *) &im[(line-1) * nx * sizeof(char)];
                for (i=0; i < nx; i++)
		    row[i] = (float) bpix[i];
                break;
            case 16:
		spix = (short *) &im[(line-1) * nx * sizeof(short)];
                for (i=0; i < nx; i++)
		    row[i] = (float) spix[i];
                break;
            case 32:
		ipix = (int *) &im[(line-1) * nx * sizeof(int)];
                for (i=0; i < nx; i++)
		    row[i] = (float) ipix[i];
                break;
            case -32:
		fpix = (float *) &im[(line-1) * nx * sizeof(float)];
                for (i=0; i < nx; i++)
		    row[i] = (float) fpix[i];
                break;
            case -64:
		dpix = (double *) &im[(line-1) * nx * sizeof(double)];
                for (i=0; i < nx; i++)
		    row[i] = (float) dpix[i];
                break;
            }

	    subSample (row, op, npix_per_line, col_step);
	    op += npix_per_line;
	    npix += npix_per_line;
	    if (npix > maxpix)
		break;
	}

	free ((float *)row);
	return (npix);
}


/* subSample -- Subsample an image line.  Extract the first pixel and
 * every "step"th pixel thereafter for a total of npix pixels.
 */

#ifdef ANSI_FUNC

static void 
subSample (float *a, float *b, int npix, int step)
#else

static void
subSample (a, b, npix, step)
float	*a;
float	*b;
int	npix, step;
#endif
{
	register int ip, i;

	if (step <= 1)
	    memmove (b, a, npix);
	else {
	    ip = 0;
	    for (i=0; i < npix; i++) {
		b[i] = a[ip];
		ip += step;
	    }
	}
}


/* fitLine -- Fit a straight line to a data array of type real.  This is
 * an iterative fitting algorithm, wherein points further than ksigma from the
 * current fit are excluded from the next fit.  Convergence occurs when the
 * next iteration does not decrease the number of pixels in the fit, or when
 * there are no pixels left.  The number of pixels left after pixel rejection
 * is returned as the function value.
 */

#ifdef ANSI_FUNC

int 
fitLine (
    float *data,		/* data to be fitted	  		  */
    int npix,			/* number of pixels before rejection	  */
    float *zstart,		/* Z-value of pixel data[1]	(output)  */
    float *zslope,		/* dz/pixel			(output)  */
    float krej,			/* k-sigma pixel rejection factor	  */
    int ngrow,			/* number of pixels of growing		  */
    int maxiter			/* max iterations			  */
)
#else

int 
fitLine (data, npix, zstart, zslope, krej, ngrow, maxiter)

float	*data;			/* data to be fitted	  		  */
int	npix;			/* number of pixels before rejection	  */
float	*zstart;		/* Z-value of pixel data[1]	(output)  */
float	*zslope;		/* dz/pixel			(output)  */
float	krej;			/* k-sigma pixel rejection factor	  */
int	ngrow;			/* number of pixels of growing		  */
int	maxiter;		/* max iterations			  */
#endif
{
	int	i, ngoodpix, last_ngoodpix, minpix, niter;
	double	xscale, z0, dz, o_dz, x, z, mean, sigma, threshold;
	double	sumxsqr, sumxz, sumz, sumx, rowrat;
	float 	*flat, *normx;
	char	*badpix;

	if (npix <= 0)
	    return (0);
	else if (npix == 1) {
	    *zstart = data[1];
	    *zslope = 0.0;
	    return (1);
	} else
	    xscale = 2.0 / (npix - 1);

	/* Allocate a buffer for data minus fitted curve, another for the
	 * normalized X values, and another to flag rejected pixels.
	 */
	flat = (float *) malloc (npix * sizeof (float));
	normx = (float *) malloc (npix * sizeof (float));
	badpix = (char *) calloc (npix, sizeof(char));

	/* Compute normalized X vector.  The data X values [1:npix] are
	 * normalized to the range [-1:1].  This diagonalizes the lsq matrix
	 * and reduces its condition number.
	 */
	for (i=0; i<npix; i++)
	    normx[i] = i * xscale - 1.0;

	/* Fit a line with no pixel rejection.  Accumulate the elements of the
	 * matrix and data vector.  The matrix M is diagonal with
	 * M[1,1] = sum x**2 and M[2,2] = ngoodpix.  The data vector is
	 * DV[1] = sum (data[i] * x[i]) and DV[2] = sum (data[i]).
	 */
	sumxsqr = 0;
	sumxz = 0;
	sumx = 0;
	sumz = 0;

	for (i=0; i<npix; i++) {
	    x = normx[i];
	    z = data[i];
	    sumxsqr = sumxsqr + (x * x);
	    sumxz   = sumxz + z * x;
	    sumz    = sumz + z;
	}

	/* Solve for the coefficients of the fitted line. */
	z0 = sumz / npix;
	dz = o_dz = sumxz / sumxsqr;

	/* Iterate, fitting a new line in each iteration.  Compute the flattened
	 * data vector and the sigma of the flat vector.  Compute the lower and
	 * upper k-sigma pixel rejection thresholds.  Run down the flat array
	 * and detect pixels to be rejected from the fit.  Reject pixels from
	 * the fit by subtracting their contributions from the matrix sums and
	 * marking the pixel as rejected.
	 */
	ngoodpix = npix;
	minpix = max (MIN_NPIXELS, (int) (npix * MAX_REJECT));

	for (niter=0;  niter < maxiter;  niter++) {
	    last_ngoodpix = ngoodpix;

	    /* Subtract the fitted line from the data array. */
	    flattenData (data, flat, normx, npix, z0, dz);

	    /* Compute the k-sigma rejection threshold.  In principle this
	     * could be more efficiently computed using the matrix sums
	     * accumulated when the line was fitted, but there are problems with
	     * numerical stability with that approach.
	     */
	    ngoodpix = computeSigma (flat, badpix, npix, &mean, &sigma);
	    threshold = sigma * krej;

	    /* Detect and reject pixels further than ksigma from the fitted
	     * line.
	     */
	    ngoodpix = rejectPixels (data, flat, normx,
		badpix, npix, &sumxsqr, &sumxz, &sumx, &sumz, threshold,
		ngrow);

	    /* Solve for the coefficients of the fitted line.  Note that after
	     * pixel rejection the sum of the X values need no longer be zero.
	     */
	    if (ngoodpix > 0) {
		rowrat = sumx / sumxsqr;
		z0 = (sumz - rowrat * sumxz) / (ngoodpix - rowrat * sumx);
		dz = (sumxz - z0 * sumx) / sumxsqr;
	    }

	    if (ngoodpix >= last_ngoodpix || ngoodpix < minpix)
		break;
	}

	/* Transform the line coefficients back to the X range [1:npix]. */
	*zstart = z0 - dz;
	*zslope = dz * xscale;
        if (abs(*zslope) < 0.001)
            *zslope = o_dz * xscale;

	free ((float *)flat);
	free ((float *)normx);
	free ((char *)badpix);
	return (ngoodpix);
}


/* flattenData -- Compute and subtract the fitted line from the data array,
 * returned the flattened data in FLAT.
 */

#ifdef ANSI_FUNC

static void 
flattenData (
    float *data,		/* raw data array			*/
    float *flat,		/* flattened data  (output)		*/
    float *x,			/* x value of each pixel		*/
    int npix,			/* number of pixels			*/
    double z0,
    double dz			/* z-intercept, dz/dx of fitted line	*/
)
#else

static void
flattenData (data, flat, x, npix, z0, dz)
float	*data;			/* raw data array			*/
float	*flat;			/* flattened data  (output)		*/
float	*x;			/* x value of each pixel		*/
int	npix;			/* number of pixels			*/
double	z0, dz;			/* z-intercept, dz/dx of fitted line	*/
#endif
{
	register int i;

	for (i=0; i < npix; i++) 
	    flat[i] = data[i] - (x[i] * dz + z0);
}


/* computeSigma -- Compute the root mean square deviation from the
 * mean of a flattened array.  Ignore rejected pixels.
 */

#ifdef ANSI_FUNC

int 
computeSigma (
    float *a,			/* flattened data array			*/
    char *badpix,		/* bad pixel flags (!= 0 if bad pixel)	*/
    int npix,
    double *mean,
    double *sigma		/* (output)				*/
)
#else

int 
computeSigma (a, badpix, npix, mean, sigma)

float	*a;			/* flattened data array			*/
char	*badpix;		/* bad pixel flags (!= 0 if bad pixel)	*/
int	npix;
double	*mean, *sigma;		/* (output)				*/
#endif
{
	float	pixval;
	int	i, ngoodpix = 0;
	double	sum = 0.0, sumsq = 0.0, temp;

	/* Accumulate sum and sum of squares. */
	for (i=0; i < npix; i++)
	    if (badpix[i] == GOOD_PIXEL) {
		pixval = a[i];
		ngoodpix = ngoodpix + 1;
		sum = sum + pixval;
		sumsq = sumsq + pixval * pixval;
	    }

	/* Compute mean and sigma. */
	switch (ngoodpix) {
	case 0:
	    *mean = INDEF;
	    *sigma = INDEF;
	    break;
	case 1:
	    *mean = sum;
	    *sigma = INDEF;
	    break;
	default:
	    *mean = sum / (double) ngoodpix;
	    temp = sumsq / (double) (ngoodpix-1) -
			(sum*sum) / (double) (ngoodpix*(ngoodpix - 1));
	    if (temp < 0)		/* possible with roundoff error */
		*sigma = 0.0;
	    else
		*sigma = sqrt (temp);
	}

	return (ngoodpix);
}


/* rejectPixels -- Detect and reject pixels more than "threshold" greyscale
 * units from the fitted line.  The residuals about the fitted line are given
 * by the "flat" array, while the raw data is in "data".  Each time a pixel
 * is rejected subtract its contributions from the matrix sums and flag the
 * pixel as rejected.  When a pixel is rejected reject its neighbors out to
 * a specified radius as well.  This speeds up convergence considerably and
 * produces a more stringent rejection criteria which takes advantage of the
 * fact that bad pixels tend to be clumped.  The number of pixels left in the
 * fit is returned as the function value.
 */

#ifdef ANSI_FUNC

int 
rejectPixels (
    float *data,		/* raw data array			*/
    float *flat,		/* flattened data array			*/
    float *normx,		/* normalized x values of pixels	*/
    char  *badpix,		/* bad pixel flags (!= 0 if bad pixel)	*/
    int   npix,
    double *sumxsqr,
    double *sumxz,
    double *sumx,
    double *sumz,/* matrix sums				*/
    double threshold,		/* threshold for pixel rejection	*/
    int ngrow			/* number of pixels of growing		*/
)
#else

int 
rejectPixels (data, flat, normx, badpix, npix,
			 sumxsqr, sumxz, sumx, sumz, threshold, ngrow)

float	*data;				/* raw data array		  */
float	*flat;				/* flattened data array		  */
float	*normx;				/* normalized x values of pixels  */
char	*badpix;			/* bad pixel flags (!= 0 if bad)  */
int	npix;
double	*sumxsqr,*sumxz,*sumx,*sumz;	/* matrix sums			  */
double	threshold;			/* threshold for pixel rejection  */
int	ngrow;				/* number of pixels of growing	  */
#endif
{
	int	ngoodpix, i, j;
	float	residual, lcut, hcut;
	double	x, z;

	ngoodpix = npix;
	lcut = -threshold;
	hcut = threshold;

	for (i=0; i < npix; i++) {
	    if (badpix[i] == BAD_PIXEL)
		ngoodpix = ngoodpix - 1;
	    else {
		residual = flat[i];
		if (residual < lcut || residual > hcut) {
		    /* Reject the pixel and its neighbors out to the growing
		     * radius.  We must be careful how we do this to avoid
		     * directional effects.  Do not turn off thresholding on
		     * pixels in the forward direction; mark them for rejection
		     * but do not reject until they have been thresholded.
		     * If this is not done growing will not be symmetric.
		     */
		    for (j=max(0,i-ngrow); j < min(npix,i+ngrow); j++) {
			if (badpix[j] != BAD_PIXEL) {
			    if (j <= i) {
				x = (double) normx[j];
				z = (double) data[j];
				*sumxsqr = *sumxsqr - (x * x);
				*sumxz = *sumxz - z * x;
				*sumx = *sumx - x;
				*sumz = *sumz - z;
				badpix[j] = BAD_PIXEL;
				ngoodpix = ngoodpix - 1;
			    } else
				badpix[j] = REJECT_PIXEL;
			}
		    }
		}
	    }
	}

	return (ngoodpix);
}


#ifdef ANSI_FUNC

int 
floatCompare (float *i, float *j)
#else

int floatCompare (i,j)
float 	*i, *j;
#endif
{
        return ((*i <= *j) ? -1 : 1);
}
