include	<imhdr.h>
include <math.h>

define	OPTIONS		"|make|replace|add|multiply|"
define	MAKE		1	# Create a new image
define	REPLACE		2	# Replace pixels
define	ADD		3	# Add to pixels
define	MULTIPLY	4	# Multiply pixels

# T_MKIMAGE -- Make or edit an image with simple values.
# An image may be created of a specified size, dimensionality, and pixel
# datatype.  The image may also be edited to replace, add, or multiply
# by specified values.  The values may be a combination of a sloped plane
# (repeated for dimensions greater than 2) and Gaussian noise.
# The editing may be confined to sections of the image by use of image
# sections in the input image.  This task is a simple tool for
# specialized uses in test applications.
#
# The sloped plane is defined such that:
#
#    pix[i,j] = value + slope * ((ncols + nlines) / 2 - 1) + slope * (i + j)
#
# The interpretation of value is that it is the mean of the plane.
#
# The Gaussian noise is only approximately random for purposes of speed!

procedure t_mkimage ()

char	image[SZ_FNAME]			# Image to edit
char	option[7]			# Edit option
real	value				# Edit value
real	slope				# Slope
real	sigma				# Gaussian noise sigma
long	seed				# Random number seed

int	i, op, ncols, nlines
long	vin[IM_MAXDIM], vout[IM_MAXDIM]
pointer	sp, rannums, im, buf, bufin, bufout

int	clgwrd(), clgeti(), clscan(), nscan() imgnlr(), impnlr()
char	clgetc()
real	clgetr()
long	clgetl()
pointer	immap()

data	seed/1/

begin
	call smark (sp)
	call clgstr ("image", image, SZ_FNAME)
	op = clgwrd ("option", option, 7, OPTIONS)
	value = clgetr ("value")
	slope = clgetr ("slope")
	sigma = clgetr ("sigma")
	if (clgetl ("seed") > 0)
	    seed = clgetl ("seed")

	call amovkl (long (1), vin, IM_MAXDIM)
	call amovkl (long (1), vout, IM_MAXDIM)
	switch (op) {
	case MAKE:
	    im = immap (image, NEW_IMAGE, 0)
	    IM_NDIM(im) = clgeti ("ndim")
	    i = clscan ("dims")
	    do i = 1, IM_NDIM(im)
		call gargi (IM_LEN(im, i))
	    if (nscan() != IM_NDIM(im))
		call error (0, "Bad dimension string")
	    switch (clgetc ("pixtype")) {
	    case 's':
	        IM_PIXTYPE(im) = TY_SHORT
	    case 'i':
	        IM_PIXTYPE(im) = TY_INT
	    case 'l':
	        IM_PIXTYPE(im) = TY_LONG
	    case 'r':
	        IM_PIXTYPE(im) = TY_REAL
	    case 'd':
	        IM_PIXTYPE(im) = TY_DOUBLE
	    default:
		call error (0, "Bad pixel type")
	    }

	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    call salloc (rannums, 2 * ncols, TY_REAL)
	    call mksigma (sigma, seed, Memr[rannums], 2*ncols)

	    while (impnlr (im, bufout, vout) != EOF)
		call mkline (value, slope, sigma, seed, Memr[rannums],
		    Memr[bufout], vout[2] - 1, ncols, nlines)
	case REPLACE:
	    im = immap (image, READ_WRITE, 0)

	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    call salloc (rannums, 2 * ncols, TY_REAL)
	    call mksigma (sigma, seed, Memr[rannums], 2*ncols)

	    while (impnlr (im, bufout, vout) != EOF)
		call mkline (value, slope, sigma, seed, Memr[rannums],
		    Memr[bufout], vout[2] - 1, ncols, nlines)
	case ADD:
	    im = immap (image, READ_WRITE, 0)

	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    call salloc (buf, ncols, TY_REAL)
	    call salloc (rannums, 2 * ncols, TY_REAL)
	    call mksigma (sigma, seed, Memr[rannums], 2*ncols)

	    while (imgnlr (im, bufin, vin) != EOF) {
		i = impnlr (im, bufout, vout)
		call mkline (value, slope, sigma, seed, Memr[rannums],
		    Memr[buf], vout[2] - 1, ncols, nlines)
		call aaddr (Memr[bufin], Memr[buf], Memr[bufout], ncols)
	    }
	case MULTIPLY:
	    im = immap (image, READ_WRITE, 0)

	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    call salloc (buf, ncols, TY_REAL)
	    call salloc (rannums, 2 * ncols, TY_REAL)
	    call mksigma (sigma, seed, Memr[rannums], 2*ncols)

	    while (imgnlr (im, bufin, vin) != EOF) {
		i = impnlr (im, bufout, vout)
		call mkline (value, slope, sigma, seed, Memr[rannums],
		    Memr[buf], vout[2] - 1, ncols, nlines)
		call amulr (Memr[bufin], Memr[buf], Memr[bufout], ncols)
	    }
	}

	call imunmap (im)
	call sfree (sp)
end


# MKLINE -- Make a line of data.  A slope of zero is a special case.
# The Gaussian random numbers are taken from the sequence of stored
# values with starting point chosen randomly in the interval 1 to ncols.
# This is not very random but is much more efficient.

procedure mkline (value, slope, sigma, seed, rannums, data, line, ncols, nlines)

real	value		# Mean value
real	slope		# Slope in mean
real	sigma		# Sigma about mean
long	seed		# Random number seed
real	rannums[ARB]	# Random numbers
real	data[ncols]	# Data for line
int	line		# Line number
int	ncols		# Number of columns
int	nlines		# Number of lines

int	i
real	a, urand()

begin
	if (slope == 0.)
	    call amovkr (value, data, ncols)
	else {
	    a = value + slope * (line - (ncols + nlines) / 2. - 1)
	    do i = 1, ncols
	        data[i] = a + slope * i
	}
	if (sigma > 0.) {
	    i = (ncols - 1) * urand (seed) + 1
	    call aaddr (rannums[i], data, data, ncols)
	}
end


# MKSIGMA -- A sequence of random numbers of the specified sigma and
# starting seed is generated.
#
# Copyright(c) 2017 Anastasia Galkin
# Reference: G. E. P. Box and Mervin E. Muller, A Note on the Generation of
#            Random Normal Deviates, The Annals of Mathematical Statistics
#            (1958), Vol. 29, No. 2 pp. 610–611

procedure mksigma (sigma, seed, rannums, nnums)

real	sigma		# Sigma for random numbers
long	seed		# Seed for random numbers
real	rannums[nnums]	# Random numbers
int	nnums		# Number of random numbers

int	i
real	v1, v2, u1, u2, urand(), sqrt()

begin
	if (sigma > 0.) {
	    for (i=1; i<=nnums; i=i+1) {
	        u1 = 1. - urand (seed)
 	        u2 = urand (seed)
		v1 = sqrt(-2 * log(u1)) * cos(2*PI*u2)
		rannums[i] = v1 * sigma
		if (i == nnums)
		    break
		v2 = sqrt(-2 * log(u1)) * sin(2*PI*u2)
		i = i + 1
		rannums[i] = v2 * sigma
	    }
	}
end
