include	<imhdr.h>
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_NEWPSF -- Use the star at the given position as the PSF if
# it exists and passes the selection criteria

int procedure dp_newpsf (dao, im, psfim, psfgr, x, y, idnum, gd, mgd,
	showplots)

pointer	dao			# pointer to daophot structure
pointer	im			# pointer to image
pointer	psfim			# pointer to psf image
pointer	psfgr			# pointer to group file
real 	x,y			# position of proposed PSF star
int	idnum			# id number of desired star
pointer	gd			# pointer to graphics stream
pointer	mgd			# pointer to metacode stream
bool	showplots		# show plots ?

bool	star_ok
int	x1, x2, y1, y2
int	starnum, nfriends
pointer	srim, psf
real	datamin, datamax, magnitude

int	dp_locstar(), dp_idstar(), dp_gaussfit(), dp_nstars()
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

	# Define some daophot pointers.
	psf = DP_PSF(dao)

	# Allocate memory for PSF fitting.
	call dp_mempsf (dao)

	# Find the star in the aperture photometry list.
	if (idnum == 0)
	    starnum = dp_locstar (dao, im, x, y)
	else
	    starnum = dp_idstar (dao, im, idnum)
	if (starnum == 0) {
	    call printf ("Star not found in the photometry file.\n")
	    return (ERR)
	} else if (starnum < 0) {
	    call printf ("Star is off or too close to the edge of the image.\n")
	    return (ERR)
	}

	# Check for INDEF valued sky.
	if (IS_INDEFR (dp_cursky (dao))) {
	    call printf ("Star %d has INDEF valued sky.\n")
	        call pargi (DP_CUR_PSFID(psf))
	    return (ERR)
	}

	# Check for INDEF values magnitude.
	if (IS_INDEFR (dp_curmag (dao))) {
	    call printf ("Star %d has INDEF valued magnitude.\n")
	        call pargi (DP_CUR_PSFID(psf))
	    return (ERR)
	}

	# Get the data raster, check for bad pixel values, and subtract sky.
	srim = dp_psubrast (dao, im, x1, x2, y1, y2, NO)
	if (srim == NULL) {
	    call printf ("Subraster around star %d contains a bad pixel.\n")
	        call pargi (DP_CUR_PSFID(psf))
	    return (ERR)
	}

	# Save the min and max values of the original data subraster.
	datamin = DP_PSFMIN(psf)
	datamax = DP_PSFMAX(psf)

	# Now let's look at the proposed PSF star.
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

	# Now fit a Gaussian to the data.
	if (dp_gaussfit (dao, Memr[srim], (x2 - x1 + 1),
	    (y2 - y1 + 1)) == ERR) {
	    call printf ("Error fitting Gaussian to star %d.\n")
	        call pargi (DP_CUR_PSFID(psf))
	    call mfree (srim, TY_REAL)
	    return (ERR)
	}
	
	# Generate a look-up table containing the departures of the
	# stellar profile from the best-fitting Gaussian.
	call dp_subgauss (dao, Memr[srim], Memr[DP_PLOOKUP (psf)],
	    (x2 - x1 + 1), (y2 - y1 + 1), DP_CUR_PSFX(psf),
	    DP_CUR_PSFY(psf), x1, y1, 1.0, 0.0)

	# Now let us examine the lookup table.
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

	# Generate the PSF.
	magnitude = dp_genpsf (dao, psfim, DP_CUR_PSFX(psf),
	    DP_CUR_PSFY(psf), DP_CUR_PSFX(psf), DP_CUR_PSFY(psf), x1, y1,
	    1.0, YES)
	call printf ("Star %d has been added to the PSF\n")
	    call pargi (DP_CUR_PSFID(psf))
	call printf ("\tMagnitude: %7.3f  Datamin: %g  Datamax: %g\n")
	    call pargr (magnitude)
	    call pargr (datamin)
	    call pargr (datamax)

	# Now generate a list of neighbors.
	nfriends = dp_nstars (dao, psfgr, true)
	call printf ("\tNumber of neighbours: %d\n\n")
	    call pargi (nfriends)

	return (OK)
end
