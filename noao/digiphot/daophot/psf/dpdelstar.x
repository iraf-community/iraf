include	<imhdr.h>
include <mach.h>
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_DELSTAR -- Delete the star at the given position from the list of PSF
# stars if it exists and passes the selection criteria.

int procedure dp_delstar (dao, im, x, y, idnum, gd, showplots)

pointer	dao			# pointer to daophot structure
pointer	im			# pointer to image
real 	x, y			# position of proposed PSF star
int	idnum		        # id number of desired star
pointer	gd			# pointer to the graphics stream
bool 	showplots		# show plots?

real	tx, ty
pointer	srim
int	starnum, saturated, x1, x2, y1, y2
bool	star_ok

pointer	dp_psubrast()
int	dp_locstar(), dp_idstar(), dp_pstati()

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

	# Find the star in the aperture photometry list.
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
		        "Star at %g,%g not found in the photometry file\n")
		        call pargr (tx)
		        call pargr (ty)
		}
	    }
	    return (ERR)
	} else if (starnum < 0 || starnum > dp_pstati (dao, PNUM)) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ( "Star %d not found in the PSF star list\n") 
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}

	# Get the data subraster, check for bad pixels and compute the min
	# and max.
	if (showplots) {
 	    srim = dp_psubrast (dao, im, -MAX_REAL, MAX_REAL, x1, x2,
	        y1, y2, saturated)
	    if (srim == NULL) {
		if (DP_VERBOSE(dao) == YES) {
		    call printf ("Star %d error reading data subraster\n")
		        call pargi (dp_pstati (dao, CUR_PSFID))
		}
	        star_ok = false
	    } else {
	        call dp_showpsf (dao, im, Memr[srim], (x2 - x1 + 1),
		    (y2 - y1 + 1), x1, y1, gd, star_ok)
	        call mfree (srim, TY_REAL)
	    }
	} else
	    star_ok = true

	if (star_ok) {
	    if (DP_VERBOSE(dao) == YES) {
	        call printf ("PSF star %d saved by user\n")
		    call pargi (dp_pstati (dao, CUR_PSFID))
	    }
	    return (ERR)
	}

	# Delete the star from the list by moving it to the position
	# currently occupied by PNUM and moving everything else up.
	call dp_pfreorder (dao, dp_pstati (dao, CUR_PSF), dp_pstati (dao,
	    PNUM))

	# Decrement the number of psf stars.
	call dp_pseti (dao, PNUM, dp_pstati(dao, PNUM) - 1)

	# Print message.
	if (DP_VERBOSE(dao) == YES) {
	    call printf ("Star %d has been deleted from the PSF star list\n")
	        call pargi (dp_pstati (dao, CUR_PSFID))
	}

	return (OK)
end
