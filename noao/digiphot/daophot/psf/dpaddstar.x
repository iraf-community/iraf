include <mach.h>
include	<imhdr.h>
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_ADDSTAR -- Add the star at the given position to the PSF if it exists and
# passes the selection criteria.

int procedure dp_addstar (dao, im, x, y, mag, idnum, gd, mgd, showplots)

pointer	dao			# pointer to daophot structure
pointer	im			# pointer to image
real 	x, y			# position of proposed PSF star
real	mag			# mag of proposed PSF star
int	idnum		        # id number of desired star
pointer	gd			# pointer to the graphics stream
pointer	mgd			# pointer to the metacode descriptor
bool 	showplots		# show plots?

real	tx, ty, logood, higood
pointer	srim
int	x1, x2, y1, y2, starnum, saturated
bool	star_ok

real	dp_statr(), dp_pstatr()
pointer	dp_psubrast()
int	dp_locstar(), dp_idstar(), dp_stati(), dp_pstati()

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
	} else if (starnum < 0) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
	        "Star %d is too near the edge of the image\n") 
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	} else if (starnum <= dp_pstati (dao, PNUM)) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("Star %d is already a PSF star\n") 
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}

	# Check for INDEF valued sky.
	if (IS_INDEFR (dp_pstatr (dao, CUR_PSFSKY))) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("Star %d has an undefined sky value\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}


	logood = dp_statr (dao, MINGDATA)
	if (IS_INDEFR(logood))
	    logood = -MAX_REAL
	higood = dp_statr (dao, MAXGDATA)
	if (IS_INDEFR(higood))
	    higood = MAX_REAL

	# Get the data subraster, check for saturation and bad pixels,
	# and compute the  min and max data values inside the subraster.
 	srim = dp_psubrast (dao, im, logood, higood, x1, x2, y1, y2, saturated)
	if (srim == NULL) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
	            "Star %d has low bad pixels inside fitrad\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}

	# Check for saturation.
	if (saturated == YES && dp_stati (dao, SATURATED) == NO) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
	            "Star %d has high bad pixels inside fitrad\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    call mfree (srim, TY_REAL)
	    return (ERR)
	}

	# Now let's look at the extracted subraster.
	if (showplots) {
	    call dp_showpsf (dao, im, Memr[srim], (x2 - x1 + 1), (y2 - y1 + 1),
	        x1, y1, gd, star_ok)
	} else if (saturated == YES) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
	        "Warning: Star %d contains high bad pixels inside fitrad\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    star_ok = true
	} else if (dp_pstatr (dao, CUR_PSFMIN) < logood || dp_pstatr (dao,
	    CUR_PSFMAX) > higood) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
	            "Warning: Star %d contains bad pixels outside fitrad\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    star_ok = true
	} else
	    star_ok = true

	# The star is rejected by the user.
	if (! star_ok) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("Star %d rejected by user\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    call mfree (srim, TY_REAL)
	    return (ERR)
	}

	# Save the plot in the metacode file.
	if (mgd != NULL)
	    call dp_plotpsf (dao, im, Memr[srim], (x2 - x1 + 1), (y2 - y1 + 1),
	        x1, y1, mgd)

	# Add the star to the PSF star list by swapping its position with the
	# position of the star currently in PNUM + 1.
	call dp_aplswap (dao, dp_pstati (dao, CUR_PSF), dp_pstati (dao,
	    PNUM) + 1)

	# Increment the number of psf stars.
	call dp_pseti (dao, PNUM, dp_pstati (dao, PNUM) + 1)

	# Reallocate the fitting array space.
	call dp_lmempsf (dao)

	# Enter the new initial values.
	call dp_xyhpsf (dao, dp_pstati (dao, PNUM), mag, saturated)

	# Print message.
	if (DP_VERBOSE(dao) == YES) {
	    call printf ("Star %d has been added to the PSF star list\n")
	        call pargi (dp_pstati (dao, CUR_PSFID))
	    call dp_ltov (im, dp_pstatr (dao, CUR_PSFX),
	        dp_pstatr(dao, CUR_PSFY), tx, ty, 1)
	    call printf (
	        "\tX: %7.2f Y: %7.2f  Mag: %7.3f  Dmin: %g  Dmax: %g\n")
		call pargr (tx)
		call pargr (ty)
	        call pargr (dp_pstatr (dao, CUR_PSFMAG))
	        call pargr (dp_pstatr (dao, CUR_PSFMIN))
	        call pargr (dp_pstatr (dao, CUR_PSFMAX))
	}

	call mfree (srim, TY_REAL)
	return (OK)
end
