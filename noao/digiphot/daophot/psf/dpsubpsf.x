include <mach.h>
include	<imhdr.h>
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_SUBPSF -- Add the star at the given position to the PSF if it exists and
# passes the selection criteria.

int procedure dp_subpsf (dao, im, x, y, idnum, gd, mgd, showplots)

pointer	dao			# pointer to daophot structure
pointer	im			# pointer to image
real 	x, y			# position of proposed PSF star
int	idnum		        # id number of desired star
pointer	gd			# pointer to the graphics stream
pointer	mgd			# pointer to the metacode descriptor
bool 	showplots		# show plots?

real	tx, ty
pointer	srim
int	x1, x2, y1, y2, starnum, saturated
bool	star_ok

real	dp_statr()
pointer	dp_psubrast()
int	dp_locstar(), dp_idstar(), dp_pstati(), dp_issat()

begin
        # Convert coordinates for display.
	if (showplots)
            call dp_ltov (im, x, y, tx, ty, 1)
	else
            call dp_wout (dao, im, x, y, tx, ty, 1)

	# Check that the position of the star is within the image.
	if (idnum == 0 && (x < 1.0 || x > real (IM_LEN(im,1)) || y < 1.0 || y >
	    real (IM_LEN(im,2)))) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("Star at %g,%g is outside the image\n")
	            call pargr (tx)
	            call pargr (ty)
	    }
	    return (ERR)
	}

	# Find the star in the aperture photometry list
	if (idnum == 0)
	    starnum = dp_locstar (dao, im, x, y)
	else
	    starnum = dp_idstar (dao, im, idnum)
	if (starnum == 0) {
	    if (DP_VERBOSE(dao) == YES) {
		if (idnum > 0) {
	            call printf ("Star %d not found in the photometry file\n")
		        call pargi (idnum)
		} else {
	            call printf (
		        "Star at %g,%g  not found in the photometry file\n")
		        call pargr (tx)
		        call pargr (ty)
		}
	    }
	    return (ERR)
	} else if (starnum < 0 || starnum > dp_pstati (dao, PNUM)) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("Star %d not found in the PSF star list\n") 
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}

	# Check to see if the star is saturated.
	if (dp_issat (dao, starnum) == YES) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("Star %d is saturated\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}

	# Get the data subraster, check for saturation and bad pixels,
	# and compute the  min and max data values inside the subraster.
 	srim = dp_psubrast (dao, im, -MAX_REAL, MAX_REAL, x1, x2, y1, y2,
	    saturated)
	if (srim == NULL) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("Star %d error reading data subraster\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}

	# Now let us do the subtraction.
	call dp_spstar (dao, Memr[srim], (x2 - x1 + 1), (y2 - y1 + 1),
	    x1, y1, starnum, dp_statr (dao, PSFRAD) ** 2)

	# Now let's look at the extracted subraster.
	if (showplots) {
	    call dp_showpsf (dao, im, Memr[srim], (x2 - x1 + 1), (y2 - y1 + 1),
	        x1, y1, gd, star_ok)
	} else
	    star_ok = true

	if (star_ok) {
	    if (mgd != NULL)
	        call dp_plotpsf (dao, im, Memr[srim], (x2 - x1 + 1),
		    (y2 - y1 + 1), x1, y1, mgd)
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("PSF star %d saved by user\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    call mfree (srim, TY_REAL)
	    return (ERR)
	}

	# Delete the star from the list by moving it to the position
	# currently occupied by PNUM and moving everything else up.
	call dp_pfreorder (dao, dp_pstati (dao, CUR_PSF),
	    dp_pstati (dao, PNUM))

	# Decrement the list of psf stars.
	call dp_pseti (dao, PNUM, dp_pstati (dao, PNUM) - 1)

	# Print message.
	if (DP_VERBOSE(dao) == YES) {
	    call printf ("Star %d has been deleted from the PSF star list\n")
	        call pargi (dp_pstati (dao, CUR_PSFID))
	}

	call mfree (srim, TY_REAL)
	return (OK)
end


# DP_SPSTAR -- Subtract the fitted star from the data subraster.

procedure dp_spstar (dao, data, nx, ny, x1, y1, starno, psfradsq)

pointer	dao			# pointer to the daophot structure
real	data[nx,ARB]		# the data subraster
int	nx, ny			# the dimensions of the data subraster
int	x1, y1			# the coordinates of the ll pixel
int	starno			# the index of the star in question
real	psfradsq		# the psf radius squared

int	i, j
pointer	psf, psffit
real	xstar, ystar, scale, deltax, deltay, dx, dy, dysq, rsq, svdx, svdy
real	dp_usepsf()

begin
	# Define some daophot pointers
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Get the star position and scale factor.
	xstar = Memr[DP_PXCEN(psf)+starno-1]
	ystar = Memr[DP_PYCEN(psf)+starno-1]
	deltax = (xstar - 1.0) / DP_PSFX(psffit) - 1.0
	deltay = (ystar - 1.0) / DP_PSFY(psffit) - 1.0
	xstar = xstar - x1 + 1
	ystar = ystar - y1 + 1
	scale = Memr[DP_PH(psf)+starno-1] / Memr[DP_PH(psf)] 

	do j = 1, ny {
	    dy = real (j) - ystar
	    dysq = dy ** 2
	    do i = 1, nx {
		dx = real (i) - xstar
		rsq = dx ** 2 + dysq
		if (rsq >= psfradsq)
		    next
		data[i,j] = data[i,j] - scale *
		    dp_usepsf (DP_PSFUNCTION(psffit), dx, dy,
		    DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		    Memr[DP_POLDLUT(psf)], DP_PSFSIZE(psffit),
		    DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit),
		    deltax, deltay, svdx, svdy)
	    }
	}

	call alimr (data, nx * ny, DP_CUR_PSFMIN(psf), DP_CUR_PSFMAX(psf)) 
end
