include <imhdr.h>
include <mach.h>
include "../lib/daophotdef.h"

# DP_NXYCHECK -- Check that the center of the group is defined. -1 is
# returned if the center of the group is undefined, 0, is returned if the
# group is entirely off the image, 1 is returned if the group is at least
# partially on the input image.

int procedure dp_nxycheck (im, x, y, group_size, radius, stat)

pointer	im			# pointer to the input image
real	x[ARB]			# array of x values
real	y[ARB]			# array of y values
int	group_size		# the size of the group
real	radius			# the fitting radius
int	stat			# the return status

int	i, nxy
real	xmin, xmax, ymin, ymax, xx, yy

begin
	# The center of the group is assumed to be undefined.
	stat = -1

	nxy = 0
	xmin = MAX_REAL
	xmax = -MAX_REAL
	ymin = MAX_REAL
	ymax = -MAX_REAL

	# Compute the minimum and maximum x and y values.
	do i = 1, group_size {
	    xx = x[i]
	    yy = y[i]
	    if (IS_INDEFR(xx) || IS_INDEFR(yy))
		next
	    nxy = nxy + 1
	    if (xx < xmin)
		xmin = xx
	    if (xx > xmax)
		xmax = xx
	    if (yy < ymin)
		ymin = yy
	    if (yy > ymax)
		ymax = yy
	}

	# The group center is undefined.
	if (nxy <= 0)
	    return (stat)

	# The center of the group is assumed to be off the image.
	stat = 0

	# Test the min and max values. 
	if ((int (xmin - radius) + 1) > IM_LEN(im,1))
	    return (stat)
	if (int (xmax + radius) < 1)
	    return (stat)
	if ((int (ymin - radius) + 1) > IM_LEN(im,2))
	    return (stat)
	if (int (ymax + radius) < 1)
	    return (stat)
	
	# The center of the group is on the image.
	stat = 1

	return (stat)
end


# DP_NMSKY -- Compute the mean sky value for the group of stars.

real procedure dp_nmsky (sky, group_size, msky)

real	sky[ARB]		# the array of sky values
int	group_size		# the size of the group of stars
real	msky			# the mean sky value

int	i, nsky
real	sky_sum

begin
	sky_sum = 0.0
	nsky = 0

	do i = 1, group_size {
	    if (IS_INDEFR(sky[i]))
		next
	    sky_sum = sky_sum + sky[i]
	    nsky = nsky + 1
	}

	if (nsky <= 0)
	    msky = INDEFR
	else
	    msky = sky_sum / nsky

	return (msky)
end


define	MIN_REL_BRIGHT		1.0E-04		# minimum relative brightness
define	INIT_REL_BRIGHT		0.01		# initial relative brightness

# DP_NMAGINIT --  Initialize various arrays before fitting the group.

procedure dp_nmaginit (dao, mag, magerr, group_size)

pointer	dao			# pointer to the daophot strucuture
real	mag[ARB]		# the magnitude array
real	magerr[ARB]		# the magnitude error array
int	group_size		# size of the group

int	i
pointer	psffit

begin
	psffit = DP_PSFFIT (dao)

	do i = 1, group_size {
	    if (IS_INDEFR(mag[i]))
		mag[i] = INIT_REL_BRIGHT
	    else {
	        mag[i] = DAO_RELBRIGHT (psffit, mag[i])
	        if (mag[i] <= MIN_REL_BRIGHT) 
		    mag[i] = INIT_REL_BRIGHT
	    }
	    magerr[i] = 0.0
	}
end
