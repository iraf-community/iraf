include	<imhdr.h>
include	"../lib/daophot.h"
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_ADDPSF -- Add the star at the given position to the PSF if
# it exists and passes the selection criteria

int procedure dp_addpsf (dao, im, psfim, psfgr, x, y, idnum, gd, mgd, showplots)

pointer	dao			# pointer to daophot structure
pointer	im			# pointer to image
pointer	psfim			# pointer to the psfimage
pointer	psfgr			# pointer to the psfgroup
real 	x, y			# position of proposed PSF star
int	idnum		        # id number of desired star
pointer	gd			# pointer to the graphics stream
pointer	mgd			# pointer to the metacode descriptor
bool 	showplots		# show plots?

bool	star_ok
int	x1, x2, y1, y2, starnum, nfriends
pointer	psf, srim
real	oxstar, oystar, xstar, ystar, sky, rel_bright
real 	datamin, datamax, magnitude

bool	dp_ispsf()
int	dp_locstar(), dp_idstar(), dp_fitpeak(), dp_nstars()
pointer	dp_psubrast()
real	dp_cursky(), dp_curmag(), dp_genpsf()

begin
	# Check that the position of the star is within the image.
	if (idnum == 0 && (x < 1.0 || x > real (IM_LEN(im,1)) || y < 1.0 || y >
	    real (IM_LEN(im,2)))) {
	    call printf ("Cursor position %g,%g is outside the image.\n")
		call pargr (x)
		call pargr (y)
	    return (ERR)
	}

	# Pointer to APSELECT and PSF definition structures.
	psf = DP_PSF(dao)

	# Find the star in the aperture photometry list
	if (idnum == 0)
	    starnum = dp_locstar (dao, im, x, y)
	else
	    starnum = dp_idstar (dao, im, idnum)
	if (starnum == 0) {
	    call printf ("Star not found in the aperture photometry file.\n")
	    return (ERR)
	} else if (starnum < 0) {
	    call printf (
	        "Star is too near the edge of the image for a PSF star.\n") 
	    return (ERR)
	}

	# Check for INDEF valued sky.
	if (IS_INDEFR(dp_cursky (dao))) {
	    call printf ("Star %d has an INDEF valued sky.\n")
	        call pargi (DP_CUR_PSFID(psf))
	    return (ERR)
	}

	# Check for INDEF valued magnitude.
	if (IS_INDEFR(dp_curmag (dao))) {
	    call printf ("Star %d has an INDEF valued magnitude.\n")
	        call pargi (DP_CUR_PSFID(psf))
	    return (ERR)
	}

	# Check whether the star has already been included in the PSF.
	if (dp_ispsf (psfim, DP_CUR_PSFID(psf), DP_PSFNUMB(psf))) {
	    call printf ("Star %d is already a PSF star\n")
	        call pargi (DP_CUR_PSFID(psf))
	    return (ERR)
	}

	# Get the data subraster, check for bad pixels and compute the min
	# and max.
 	srim = dp_psubrast (dao, im, x1, x2, y1, y2, NO)
	if (srim == NULL) {
	    call printf (
	        "Subraster around star %d contains a bad pixel value.\n")
	        call pargi (DP_CUR_PSFID(psf))
	    return (ERR)
	}

	# Save the min and max values of the original subraster.
	datamin = DP_PSFMIN(psf)
	datamax = DP_PSFMAX(psf)

	# Now let's look at the extracted subraster
	if (showplots)
	    call dp_showpsf (dao, Memr[srim], (x2 - x1 + 1), (y2 - y1 + 1),
	        gd, star_ok)
	else
	    star_ok = true

	if (star_ok) {
	    call dp_plotpsf (dao, Memr[srim], (x2 - x1 + 1), (y2 - y1 + 1), mgd)
	} else {
	    call printf ("Star %d rejected: select another\n")
	        call pargi (DP_CUR_PSFID(psf))
	    call mfree (srim, TY_REAL)
	    return (ERR)
	}

	# Fit the star.
	if (dp_fitpeak (dao, Memr[srim], (x2 - x1 + 1), (y2- y1 - 1),
	    oxstar, oystar, xstar, ystar, rel_bright, sky) == ERR) {
	    call printf ("Error fitting star %d: select another\n")
	        call pargi (DP_CUR_PSFID(psf))
	    call mfree (srim, TY_REAL)
	    return (ERR)
	}

	# Subtract the fitted PSF and show user.
	call dp_subgauss (dao, Memr[srim], Memr[DP_PLOOKUP (psf)],
	    (x2 - x1 + 1), (y2 - y1 + 1), xstar, ystar, 1, 1, rel_bright, sky)
	if (showplots)
	    call dp_showpsf (dao, Memr[DP_PLOOKUP (psf)], (x2 - x1 + 1),
	        (y2 - y1 + 1), gd, star_ok)
	else
	    star_ok = true

	if (star_ok) {
	    call dp_plotpsf (dao, Memr[DP_PLOOKUP (psf)], (x2 - x1 + 1),
	        (y2 - y1 + 1), mgd)
	} else {
	    call printf ("Star %d rejected: select another\n")
	        call pargi (DP_CUR_PSFID(psf))
	    call mfree (srim, TY_REAL)
	    return (ERR)
	}

	# Add the star to the PSF.
	magnitude = dp_genpsf (dao, psfim, oxstar, oystar, xstar, ystar,
	    1, 1, rel_bright, NO)
	call printf ("Star %d has been added to the PSF\n")
	    call pargi (DP_CUR_PSFID(psf))
	call printf ("\tMagnitude: %7.3f  Datamin: %g  Datamax: %g\n")
	    call pargr (magnitude)
	    call pargr (datamin)
	    call pargr (datamax)

	# Now generate a list of neighbors.
	nfriends = dp_nstars (dao, psfgr, false)
	call printf ("\tNumber of neighbours: %d\n\n")
	    call pargi (nfriends)

	return (OK)
end
