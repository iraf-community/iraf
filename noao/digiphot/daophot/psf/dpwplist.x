include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/psfdef.h"

define	NCOLUMN	5

# DP_WPLIST -- Write the list of psf stars to the output psf star list.

procedure dp_wplist (dao, opst)

pointer	dao			# pointer to the daophot structure
int	opst			# the output psf star list descriptor

pointer	apsel, psf, sp, ocolpoint
bool	itob()
int	dp_stati()

begin
	# Get some daophot pointers.
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)

	# Allocate some working memory.
	call smark (sp)
	call salloc (ocolpoint, NCOLUMN, TY_POINTER)

	# Initialize the output file.
	if (dp_stati (dao, TEXT) == YES) {
	    call dp_pxgrppars (dao, opst)
	    call dp_xpbanner (opst)
	} else {
	    call dp_tpdefcol (opst, Memi[ocolpoint])
	    call dp_ptgrppars (dao, opst)
	}

	# Write out the stars.
	call dp_wpstars (opst, Memi[ocolpoint], itob (dp_stati (dao, TEXT)),
	    Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	    Memr[DP_APMSKY(apsel)], DP_PNUM(psf))

	# Free memory.
	call sfree (sp)
end


# DP_CHINDEF -- Change all the INDEFS to a legal number which is a lower limit.

procedure dp_chindef (a, b, npix, lolimit)

real	a[ARB]			# the input array.
real	b[ARB]			# the output array.
int	npix			# number of points
real	lolimit			# the lower limit

int	i

begin
	do i = 1, npix {
	    if (IS_INDEFR(a[i]))
		b[i] = lolimit
	    else
		b[i] = a[i]
	}
end


# DP_PFSTARS -- Select the psf stars.

int procedure dp_pfstars (dao, im, ids, xcen, ycen, mag, sky, nstar, maxnpsf,
	lolimit, radius, mgd) 

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
int	ids[ARB]		# array of star ids
real	xcen[ARB]		# array of x coordinates
real	ycen[ARB]		# array of y coordinates
real	mag[ARB]		# array of magnitudes
real	sky[ARB]		# array of sky values
int	nstar			# the number of stars
int	maxnpsf			# the maximum number of psf stars
real	lolimit			# lower data limit
real	radius			# minimum separation
pointer	mgd			# pointer to the plot metacode file

bool	omit
int	istar, jstar, npsf
real	radsq, dy2, dr2
int	dp_addstar()

begin
	radsq = radius * radius

	if ((mag[1] > lolimit) && (dp_addstar (dao, im, xcen[1], ycen[1],
	    ids[1], NULL, mgd, false)) == OK) {
	    npsf = 1
	} else
	    npsf = 0

	# Loop over the candidate psf stars.
	do istar = 2, nstar {

	    # Test that the candidate psf stars are not saturated and
	    # are sufficiently far from the edge of the frame.

	    if (mag[istar] <= lolimit)
		next

	    # Text that there are no brighter stars with a distance squared
	    # of radsq.
	    omit = false
	    do jstar = 1, istar - 1 {
		dy2 = abs (ycen[jstar] - ycen[istar])
		if (dy2 >= radius)
		    next
		dr2 = (xcen[jstar] - xcen[istar]) ** 2 + dy2 ** 2
		if (dr2 >= radsq)
		    next
		omit = true
		break
	    }

	    if (omit)
		next

	    if (dp_addstar (dao, im, xcen[istar], ycen[istar], ids[istar],
	        NULL, mgd, false) == ERR)
		next
	    npsf = npsf + 1
	    if (npsf >= maxnpsf)
		break
	}

	return (npsf)
end
