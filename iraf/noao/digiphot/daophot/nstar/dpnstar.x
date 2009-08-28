include	<imhdr.h>
include <tbset.h>
include <mach.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/nstardef.h"


# DP_NPHOT -- Read in the groups and fit all the stars in each group
# simultaneously.

procedure dp_nphot (dao, im, grp, nst, rej, ap_text)

pointer	dao			# pointer to the daophot structure
pointer	im			# input image descriptor
pointer	grp			# input group file descriptor
pointer	nst			# output photometry file descriptor
pointer	rej			# rejections file descriptor
bool	ap_text			# photometry text file

size_t	sz_val
bool	converge 
int	old_size, niter, cdimen, stat, i_val0, i_val1
long	in_record, nrow_in_table, out_record, rout_record
pointer	sp, indices, fields, icolpoint, ocolpoint, key, nstar, apsel
real	radius, mean_sky

int	dp_ggroup(), dp_nstarfit(), dp_nxycheck()
long	tbpstl()
real	dp_nmsky()

begin
	# Store the original value of the fitting radius.
	radius = DP_FITRAD(dao)
	DP_FITRAD(dao) = min (DP_FITRAD(dao), DP_PSFRAD(dao))
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)

	# Allocate working memory.
	call smark (sp)
	sz_val = NAPGROUP
	call salloc (icolpoint, sz_val, TY_POINTER)
	sz_val = NAPPAR
	call salloc (indices, sz_val, TY_INT)
	sz_val = SZ_LINE
	call salloc (fields, sz_val, TY_CHAR)
	sz_val = NST_NOUTCOL
	call salloc (ocolpoint, sz_val, TY_POINTER)

	# Get some daophot pointers.
	apsel = DP_APSEL(dao)
	nstar = DP_NSTAR (dao)

	# Allocate space for and define the output table.
	if (DP_TEXT(dao) == YES) { 
	    i_val0 = nst
	    call dp_xpnewnstar (dao, i_val0)
	    if (rej != NULL) {
		i_val0 = rej
	        call dp_xpnewnstar (dao, i_val0)
	    }
	} else {
	    call dp_tpnewnstar (dao, nst, Memp[ocolpoint])
	    if (rej != NULL)
	        call dp_tpnewnstar (dao, rej, Memp[ocolpoint])
	}

	# Set up the input file.
	call dp_gnindices (Memi[indices])
	if (ap_text) {
	    call pt_kyinit (key)
	    call dp_gnstpsf (Memi[indices], Memc[fields], NAPGROUP)
	    nrow_in_table = 0
	} else {
	    key = NULL
	    call dp_tnstinit (grp, Memp[icolpoint])
	    nrow_in_table = tbpstl (grp, TBL_NROWS)
	}

	# Allocate some memory for fitting.
	call dp_memapsel (dao, Memi[indices], NAPPAR, DP_MAXGROUP(dao) + 1)
	call dp_memnstar (dao, DP_MAXGROUP(dao) + 1)

	# Initialize the input/output files for reading/writing.
	in_record = 1
	out_record = 0
	rout_record = 0

	repeat {

	    # Read in the next group of stars.
	    old_size = dp_ggroup (dao, grp, key, Memc[fields], Memi[indices],
	        Memp[icolpoint], nrow_in_table, DP_MAXGROUP(dao), in_record,
		DP_NGNUM(nstar))
	    if (old_size <= 0)
		break
	    DP_NNUM(nstar) = old_size

	    # Convert coordinates if necessary.
	    sz_val = DP_NNUM(nstar)
	    call dp_win (dao, im, Memr[DP_APXCEN(apsel)],
	        Memr[DP_APYCEN(apsel)], Memr[DP_APXCEN(apsel)],
		Memr[DP_APYCEN(apsel)], sz_val)

	    # Print out the group number and number of stars.
	    if (DP_VERBOSE (dao) == YES) {
		call printf ("Group: %4d contains %2d stars\n")
		    call pargi (DP_NGNUM (nstar))
		    call pargi (DP_NNUM (nstar))
	    }

	    # If the group center is undefined or off image skip to next 
	    # group.
	    if (dp_nxycheck (im, Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
	        DP_NNUM(nstar), DP_FITRAD(dao), stat) <= 0) {

		if (DP_VERBOSE(dao) == YES) {
		    if (stat < 0)
			call printf (
			    "\tGroup %d center is undefined: skipping\n")
		    else if (stat == 0)
			call printf ("\tGroup %d is off the image: skipping\n")
		    call pargi (DP_NGNUM(nstar))
		}
		next

	    # If too many stars skip to the next group.
	    } else if (DP_NNUM (nstar) > DP_MAXGROUP(dao)) {

		if (DP_VERBOSE(dao) == YES) {
		    call printf ("\tGroup %d larger than %d stars: skipping\n")
			call pargi (DP_NGNUM(nstar))
			call pargi (DP_MAXGROUP(dao))
		}
		niter = 0
		sz_val = DP_NNUM(nstar)
		call realloc (DP_NIER(nstar), sz_val, TY_INT)
		call dp_nstindef (dao, DP_NNUM(nstar), NSTERR_BIGGROUP)

	    # If the mean sky value of group is undefined skip to next group.
	    } else if (IS_INDEFR (dp_nmsky (Memr[DP_APMSKY(apsel)],
	        DP_NNUM(nstar), mean_sky))) {

		if (DP_VERBOSE(dao) == YES) {
		    call printf (
		        "\tGroup %d sky value is undefined: skipping\n")
		        call pargi (DP_NGNUM(nstar))
		}
		niter = 0
		call dp_nstindef (dao, DP_NNUM(nstar), NSTERR_INDEFSKY)

	    # Do the fit.
	    } else {

		# Estimate values of INDEF magnitudes.
		call dp_nmaginit (dao, Memr[DP_APMAG(apsel)],
		    Memr[DP_APERR(apsel)], DP_NNUM(nstar))

		# Set up for the fit.
		if (DP_RECENTER(dao) == YES)
	            cdimen = 3 * DP_NNUM(nstar) + 1
		else
	            cdimen = DP_NNUM(nstar) + 1

	        # Iterate.
		for (niter = 1; niter <= DP_MAXITER(dao); niter = niter + 1) {
	            DP_NNUM(nstar) = dp_nstarfit (dao, im, DP_NNUM(nstar),
		        mean_sky, cdimen, niter, converge)
		    if (DP_NNUM(nstar) <= 0)
			break
		    if (converge)
			break
	        }

		# Solution did not converge.
		niter = min (niter, DP_MAXITER(dao))
	    }

	    # Now write out the results.
	    if (DP_TEXT(dao) == YES) {
		i_val0 = nst
		i_val1 = rej
	        call dp_xntwrite (dao, im, i_val0, i_val1, niter, old_size)
	    } else {
	        call dp_tntwrite (dao, im, nst, rej, niter, old_size,
		    out_record, rout_record, Memp[ocolpoint])
	    }
	} 

	if (ap_text)
	    call pt_kyfree (key)

	call sfree (sp)

	# Restore the original value of the fitting radius.
	DP_FITRAD(dao) = radius
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)
end


# DP_NSTINDEF -- Set magnitudes and fitting parameters of unfit stars to indef.

procedure dp_nstindef (dao, max_nstars, pier)

pointer	dao		# pointer to the daophot structure
int	max_nstars	# number of stars
int	pier		# the photometry error code

int	i
pointer	apsel, nstar

begin
	apsel = DP_APSEL(dao)
	nstar = DP_NSTAR(dao)
	do i = 1, max_nstars {
	    Memr[DP_APMAG(apsel)+i-1] = INDEFR
	    Memr[DP_APERR(apsel)+i-1] = INDEFR
	    Memr[DP_APCHI(apsel)+i-1] = INDEFR
	    Memr[DP_APSHARP(apsel)+i-1] = INDEFR
	    Memi[DP_NIER(nstar)+i-1] = pier
	}
end


# DP_GNINDICES -- Get the memory allocation fields.

procedure dp_gnindices (indices)

int	indices[ARB]		# index array

begin
	indices[1] = DP_PAPID
        indices[2] = DP_PAPXCEN
	indices[3] = DP_PAPYCEN
	indices[4] = DP_PAPMAG1
	indices[5] = DP_PAPSKY
	indices[6] = DP_PAPGROUP
	indices[7] = DP_PAPMERR1
	indices[8] = DP_PAPNITER
	indices[9] = DP_PAPSHARP
	indices[10] = DP_PAPCHI
end


define	GRP_CTRINDEF	-1
define	GRP_CTROFFIMAGE	 0
define	GRP_CTROK	 1

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
long	lint()

begin
	# Initialize.
	stat = GRP_CTRINDEF
	xmin = MAX_REAL
	xmax = -MAX_REAL
	ymin = MAX_REAL
	ymax = -MAX_REAL

	# Compute the minimum and maximum x and y values.
	nxy = 0
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
	if (nxy <= 0)
	    return (stat)

	# Test the min and max values. 
	stat = GRP_CTROFFIMAGE
	if ((lint (xmin - radius) + 1) > IM_LEN(im,1))
	    return (stat)
	if (lint (xmax + radius) < 1)
	    return (stat)
	if ((lint (ymin - radius) + 1) > IM_LEN(im,2))
	    return (stat)
	if (lint (ymax + radius) < 1)
	    return (stat)
	
	# The group is on the image.
	stat = GRP_CTROK
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

# DP_NMAGINIT --  Initialize the magnitude and magnitude error arrays before
# fitting the group.

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
