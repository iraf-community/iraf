include <mach.h>
include <imhdr.h>
include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_PSUBRAST -- Fetch the prospective PSF star data and check it for bad
# pixel values.

pointer procedure dp_psubrast (dao, im, lowbad, highbad, x1, x2, y1, y2,
	saturated)

pointer	dao		# pointer to the daophot structure
pointer	im		# pointer to the input image
real	lowbad, highbad	# minimum and maximum good data values
int	x1, x2		# output x limits of the extracted subraster
int	y1, y2		# output y limits of the extracted subraster
int	saturated	# is the star saturated ?

pointer	psf, buf
real	psfrad, fitrad
int	dp_chksr()
pointer	dp_subrast()

begin
	# Initialize.
	psf = DP_PSF(dao)
	buf = NULL
	psfrad = DP_PSFRAD(dao)
	fitrad = DP_FITRAD(dao)

	# Get the data.
 	buf = dp_subrast (im, DP_CUR_PSFX(psf), DP_CUR_PSFY(psf), psfrad,
	    x1, x2, y1, y2)
	if (buf == NULL)
	    return (NULL)

	# Check for bad pixels in subraster, compute the min and max.
	if (dp_chksr (DP_CUR_PSFX(psf), DP_CUR_PSFY(psf), Memr[buf],
	    x2 - x1 + 1, y2 - y1 + 1, x1, y1, psfrad, fitrad,
	    lowbad, highbad, saturated, DP_CUR_PSFMIN(psf),
	        DP_CUR_PSFMAX(psf), DP_CUR_PSFGMAX(psf)) == ERR) {
	    call mfree (buf, TY_REAL)
	    return (NULL)
	}
	
	return (buf)
end


# DL_LSUBRAST -- Give a valid PSF star compute the limits of the data to
# be extracted around it.

int procedure dp_lsubrast (im, xcen, ycen, radius, x1, x2, y1, y2)

pointer	im			# input image descriptor
real	xcen, ycen		# center of subraster
real	radius			# radius of the box
int	x1, y1, x2, y2		# boundaries of subraster

begin
	# Calculate start position of extraction box.
	x1 = int (xcen - radius) - 2
	x2 = int (xcen + radius) + 3
	y1 = int (ycen - radius) - 2
	y2 = int (ycen + radius) + 3
	if (x1 > IM_LEN(im,1) || x2 < 1 || y1 > IM_LEN(im,2) || y2 < 1)
	    return (ERR)

	x1 = max (1, x1)
	x2 = min (IM_LEN(im,1), x2)
	y1 = max (1, y1)
	y2 = min (IM_LEN(im,2), y2)
	return (OK)
end


# DP_SUBRAST -- Given a valid PSF star extract the data around it.

pointer procedure dp_subrast (im, xcen, ycen, radius, x1, x2, y1, y2)

pointer	im			# input image descriptor
real	xcen, ycen		# center of subraster
real	radius			# radius of the box
int	x1, y1, x2, y2		# boundaries of subraster

int	j, ncols
pointer	buf, ptr, imbuf
pointer imgs2r()

begin
	# Calculate start position of extraction box.
	x1 = int (xcen - radius) - 2
	x2 = int (xcen + radius) + 3
	y1 = int (ycen - radius) - 2
	y2 = int (ycen + radius) + 3
	if (x1 > IM_LEN(im,1) || x2 < 1 || y1 > IM_LEN(im,2) || y2 < 1)
	    return (NULL)

	x1 = max (1, x1)
	x2 = min (IM_LEN(im,1), x2)
	y1 = max (1, y1)
	y2 = min (IM_LEN(im,2), y2)
	call malloc (buf, (x2 - x1 + 1) * (y2 - y1 + 1), TY_REAL)

	ptr = buf
	ncols = x2 - x1 + 1
	do j = y1, y2 {
	    imbuf = imgs2r (im, x1, x2, j, j)
	    call amovr (Memr[imbuf], Memr[ptr], ncols) 
	    ptr = ptr + ncols
	}

	return (buf)
end


# DP_CHKSR -- Check the input subraster for bad pixels.

int procedure dp_chksr (x, y, sr, xdim, ydim, x1, y1, psfrad, fitrad, lowbad,
        highbad, saturated, dmin, dmax, gmax)

real	x, y			# position of the star
real	sr[xdim,ydim]		# the data subraster
int	xdim, ydim		# the dimensions of the subraster
int	x1, y1			# the lower left hand coordinates of the array
real	psfrad			# the psf radius
real	fitrad			# the fitting radius
real	lowbad, highbad		# the good data limits
int	saturated		# is the star saturated
real	dmin, dmax		# output data limits
real	gmax			# maximum good data limit

int	i,j
real	pradsq, fradsq, dy2, r2

begin
	pradsq = psfrad * psfrad
	fradsq = fitrad * fitrad
	dmin = MAX_REAL
	dmax = -MAX_REAL
	gmax = -MAX_REAL
	saturated = NO

	# Loop through the pixels looking for bad values.
	do j = 1, ydim {
	    dy2 = (y - (y1 + j -1)) ** 2 
	    if (dy2 > pradsq)
		next
	    do i = 1, xdim {
		r2 = (x - (x1 + i - 1)) ** 2 + dy2
	        if (r2 > pradsq)
		    next
		if (sr[i,j] < dmin)
		    dmin = sr[i,j]
		if (sr[i,j] > dmax)
		    dmax = sr[i,j]
		if (sr[i,j] < lowbad || sr[i,j] > highbad) {
		    if (r2 <= fradsq) {
		        if (sr[i,j] < lowbad)
		    	    return (ERR)
			else if (saturated == NO)
			    saturated = YES
		    }
		} else {
		    if (sr[i,j] > gmax)
		        gmax = sr[i,j]
		}
	    }
	}

	return (OK)
end
