include <mach.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/psfdef.h"

define	NCOLUMN	5

# DP_GPSTARS -- Select the psf stars.

procedure dp_gpstars (dao, im, tp_in, tp_out, text_file, maxnpsf, gd, mgd, id,
	mkstars, interactive, use_cmdfile)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
int	tp_in			# the input file descriptor
int	tp_out			# the output file descriptor
bool	text_file		# text or table file
int	maxnpsf			# the maximum number of psf stars
pointer	gd			# pointer to the graphics stream
pointer	mgd			# pointer to the plot metacode file
pointer	id			# pointer to the image display stream
bool	mkstars			# mark the accepted and deleted stars
bool	interactive		# interactive mode
bool	use_cmdfile		# cursor command file mode

int	ier, npsf
pointer	apsel, sp, ocolpoint, index
real	radius
int	dp_pfstars(), dp_ipfstars()

begin
	# Get some daophot pointers.
	apsel = DP_APSEL(dao)

	# Define the radius for determining proximity.
	radius = DP_PSFRAD(dao) + DP_FITRAD(dao) + 2.0

	# Allocate some working memory.
	call smark (sp)
	call salloc (ocolpoint, NCOLUMN, TY_POINTER)
	call salloc (index, DP_APNUM(apsel), TY_INT)

	# Initialize the output file.
	if (text_file) {
	    call seek (tp_in, BOF)
	    call dp_apheader (tp_in, tp_out)
	    call dp_xpselpars (tp_out, DP_INIMAGE(dao), maxnpsf, DP_SCALE(dao),
	        DP_SPSFRAD(dao), DP_SFITRAD(dao))
	    call dp_xpbanner (tp_out)
	} else {
	    call dp_tpdefcol (tp_out, Memi[ocolpoint])
	    call tbhcal (tp_in, tp_out)
	    call dp_tpselpars (tp_out, DP_INIMAGE(dao), maxnpsf, DP_SCALE(dao),
	        DP_SPSFRAD(dao), DP_SFITRAD(dao))
	}

	# Change all the INDEFS to a large negative number.
	call dp_chindef (Memr[DP_APMAG(apsel)], Memr[DP_APMAG(apsel)],
	    DP_APNUM(apsel), -MAX_REAL)

	# Sort the stars.
	call quick (Memr[DP_APMAG(apsel)], DP_APNUM(apsel), Memi[index], ier)
	call dp_irectify (Memi[DP_APID(apsel)], Memi[index], DP_APNUM(apsel)) 
	call dp_rectify (Memr[DP_APXCEN(apsel)], Memi[index], DP_APNUM(apsel)) 
	call dp_rectify (Memr[DP_APYCEN(apsel)], Memi[index], DP_APNUM(apsel)) 
	call dp_rectify (Memr[DP_APMSKY(apsel)], Memi[index], DP_APNUM(apsel)) 

	# Select the stars and write them to the output file.
	if (use_cmdfile)
	    npsf = dp_ipfstars (dao, im, maxnpsf, -MAX_REAL, radius, gd,
		mgd, id, mkstars, false, false)
	else if (interactive)
	    npsf = dp_ipfstars (dao, im, maxnpsf, -MAX_REAL, radius, gd,
		mgd, id, mkstars, true, true)
	else
	    npsf = dp_pfstars (dao, im, Memi[DP_APID(apsel)],
	        Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
		Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)],
		DP_APNUM(apsel), maxnpsf, -MAX_REAL, radius, mgd)

	if (DP_VERBOSE(dao) == YES) {
	    call printf ("\nTotal of %d PSF stars selected\n")
		call pargi (npsf)
	}

	# Write out the stars.
	call dp_wout (dao, im, Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
	    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)], npsf)
	call dp_wpstars (tp_out, Memi[ocolpoint], text_file,
	    Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	    Memr[DP_APMSKY(apsel)], npsf)

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
	# Initialize the list.
	call dp_pseti (dao, PNUM, 0)

	# Set the radius.
	radsq = radius * radius

	# Get the first star.
	if ((mag[1] > lolimit) && (dp_addstar (dao, im, xcen[1], ycen[1],
	    INDEFR, ids[1], NULL, mgd, false)) == OK) {
	    npsf = 1
	} else
	    npsf = 0

	# Loop over the candidate psf stars.
	do istar = 2, nstar {

	    # Test for the maximum number of psf stars.
	    if (npsf >= maxnpsf)
		break

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

	    if (dp_addstar (dao, im, xcen[istar], ycen[istar], INDEFR,
	        ids[istar], NULL, mgd, false) == ERR)
		next
	    npsf = npsf + 1
	}

	return (npsf)
end
