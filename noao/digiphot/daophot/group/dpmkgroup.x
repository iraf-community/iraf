include <mach.h>
include "../lib/daophotdef.h"
include	"../lib/apseldef.h"

define	EPS_OVERLAP		(1.0e-10)

# DP_MKGROUP -- Arrange the photometry results into natural groupings
# based on physical proximity and the signal-to-noise ratio.

procedure dp_mkgroup (dao, im, grp)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to input image
int	grp			# the output file descriptor

bool	overlap
int	i, maxgroup, curr_star, first_unknown, first_ingrp, nin_currgrp
int	curr_point, crit_point, ier
pointer	psffit, apsel, index, group_size, number
real	fradius, critovlap, bright_mag, read_noise, fitradsq, critsep
real	critsep_sq, xcurr, ycurr, dxcurr_frompsf, dycurr_frompsf, magcurr
real	skycurr, magcrit, skycrit, dxcrit_frompsf, dycrit_frompsf
real	deltax, deltay, radsq, rel_bright, radius, ratio, stand_err, dvdx, dvdy

bool 	dp_gchkstar()
real	dp_usepsf()

begin
	# Get the daophot pointers.
	psffit = DP_PSFFIT(dao)
	apsel = DP_APSEL(dao)

	# Return if the photometry file is empty.
	if (DP_APNUM(apsel) <= 0)
	    return

	# Store the original fitting radius.
	fradius = DP_FITRAD(dao)
	DP_FITRAD(dao) = min (DP_FITRAD(dao), DP_PSFRAD(dao))
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)

	# Get some working memory.
	call malloc (index, DP_APNUM(apsel), TY_INT)
	call malloc (group_size, DP_APNUM(apsel), TY_INT)
	call malloc (number, DP_APNUM(apsel), TY_INT)

	# Initialize the group information.
	call aclri (Memi[index], DP_APNUM(apsel))
	call aclri (Memi[group_size], DP_APNUM(apsel))
	call aclri (Memi[number], DP_APNUM(apsel))

	# Check for INDEF results and fix them up..
	call dp_gpfix (dao, im, bright_mag)

	# Sort the stars into order of increasing y value.  The star ID's, X,
	# MAG and SKY values are still in the order in which they were read
	# in. INDEX points to the proper value, i.e. Y (i) and X (index(i)).

	call quick (Memr[DP_APYCEN(apsel)], DP_APNUM(apsel), Memi[index], ier)

	# Bright_mag is the apparent magnitude of the brightest star
	# in the input file. If this is INDEF then set to the PSFMAG.

	if (! IS_INDEFR(bright_mag))
	    bright_mag = DAO_RELBRIGHT (psffit, bright_mag)
	else
	    bright_mag = 1.0

	# Smooth the PSF.
	if ((DP_NVLTABLE(psffit) + DP_NFEXTABLE(psffit)) > 0)
	    call dp_smpsf (dao)

	# Define some important constants in terms of the daophot parameters.

	critovlap = DP_CRITSNRATIO(dao) * DP_CRITSNRATIO(dao)
	read_noise = (DP_READNOISE (dao) / DP_PHOTADU(dao)) ** 2
	fitradsq = (DP_FITRAD(dao) + 1) * (DP_FITRAD(dao) + 1)

	## Note that the definition of firadsq has been changed in this
	## version of group. This has been done so that the grouping
	## algorithm defaults to the one used by ALLSTAR in the case that
	## the critoverlap parameter is very large.
	## fitradsq = (2.0 * DP_FITRAD(dao)) * (2.0 * DP_FITRAD(dao))

	# Define the critical separation.
	critsep = DP_PSFRAD(dao) + DP_FITRAD(dao) + 1.0
	critsep_sq = critsep * critsep

	# Now we search the list for stars lying within one critical
	# separation of each other. The stars are currently in a stack
	# DP_APNUM(apsel) stars long.  FIRST_INGRP points to the first star
	# in the current group and starts out, of course, with the
	# value CURR_STAR.  CURR_STAR will point to the position in the stack
	# occupied by the star which is currently the center of a circle 
	# of radius equal to the critical radius, within which we are
	# looking for other stars. CURR_STAR starts out with a value of
	# 1. FIRST_UNKNOWN points to the top position in the stack of the
	# stars which have not yet been assigned to groups. FIRST_UNKNOWN
	# starts out with the value 2.  Each time through, the program goes
	# down through the stack from FIRST_UNKNOWN to DP_APNUM(apsel) and
	# looks for stars within the critical distance from the star at stack
	# position CURR_STAR. When such a star is found, it changes places
	# in the stack with the star at FIRST_UNKNOWN and FIRST_UNKNOWN is
	# incremented by one.  When the search has gotten to the last
	# position in the stack (DP_APNUM(apsel)), the pointer CURR_STAR is
	# incremented by one, and the search proceeds again from the new
	# value of FIRST_UNKNOWN to DP_APNUM(apsel). If the pointer CURR_STAR
	# catches up with the pointer FIRST_UNKNOWN, that means that the
	# group currently being built up is complete. The number of stars in
	# the newly-created group (the first star of which is at stack
	# position FIRST_INGRP) is stored in array element GROUP_SIZE[FIRST_
	# INGRP]. Then a new group is started beginning with the star at the
	# current position CURR_STAR, ( = FIRST_UNKNOWN for the moment),
	# FIRST_UNKNOWN is incremented by 1, and the next group is built up
	# as before.

	# Initialize.
	maxgroup = 0
	curr_star = 1
	first_unknown = 1

	# Loop through the list of stars.
	while (curr_star <= DP_APNUM(apsel)) {

	    # Initialize for the next group.
	    first_ingrp = curr_star
	    nin_currgrp = 1
	    first_unknown = first_unknown + 1

	    # Begin defining a group.
	    while (curr_star < first_unknown) {

		# Get the parameters of the current star.
		curr_point = Memi[index+curr_star-1]
		xcurr = Memr[DP_APXCEN(apsel)+curr_point-1]
		ycurr = Memr[DP_APYCEN(apsel)+curr_star-1]
		call dp_wpsf (dao, im, xcurr, ycurr, dxcurr_frompsf,
		    dycurr_frompsf, 1)
		dxcurr_frompsf = (dxcurr_frompsf - 1.0) / DP_PSFX(psffit) - 1.0
		dycurr_frompsf = (dycurr_frompsf - 1.0) / DP_PSFY(psffit) - 1.0
		magcurr = Memr[DP_APMAG(apsel)+curr_point-1]
		skycurr = Memr[DP_APMSKY(apsel)+curr_point-1]
		if (IS_INDEFR(magcurr))
		    magcurr = bright_mag
		else if (abs (DAO_MAGCHECK(psffit, magcurr)) > (MAX_EXPONENTR -
		    1))
		    magcurr = bright_mag	    
		else
		    magcurr = DAO_RELBRIGHT (psffit, magcurr)

		# Go through the list of unassigned stars looking for stars
		# within one critical distance of the current star.

		do i = first_unknown, DP_APNUM(apsel) {

		    # Is this star within one critical separation of the
		    # current star ?

		    overlap = dp_gchkstar (Memr[DP_APXCEN(apsel)], 
		        Memr[DP_APYCEN(apsel)], Memi[index], i, xcurr, ycurr,
			critsep_sq, deltax, deltay, radsq)

		    # Break from the do loop if we are beyond the the critical
		    # separation.

		    if (deltay > critsep)
			break
		    if (! overlap)
			next

		    # Define the characteristics of the unknown star.

		    crit_point = Memi[index+i-1]
		    magcrit = Memr[DP_APMAG(apsel)+crit_point-1]
		    skycrit = Memr[DP_APMSKY(apsel)+crit_point-1]
		    call dp_wpsf (dao, im, Memr[DP_APXCEN(apsel)+crit_point-1],
		        Memr[DP_APYCEN(apsel)+i-1], dxcrit_frompsf,
		        dycrit_frompsf, 1)
		    dxcrit_frompsf = (dxcrit_frompsf - 1.0) / DP_PSFX(psffit) -
		        1.0
		    dycrit_frompsf = (dycrit_frompsf - 1.0) / DP_PSFY(psffit) -
		        1.0

		    # Check to see if stars overlap critically.

		    if ((critovlap > EPS_OVERLAP) && (radsq > fitradsq)) {

			overlap = false

			# Rel_bright is the brightness of the star currently
			# being tested relative to the star in the center.

			if (IS_INDEFR(magcrit))
			    rel_bright = bright_mag
			else if (abs (DAO_MAGCHECK (psffit, magcrit)) >
			    (MAX_EXPONENTR - 1))
			    rel_bright = bright_mag
			else
			    rel_bright = DAO_RELBRIGHT(psffit, magcrit)
							
			# Determine the point at which to evaluate the PSF.

			radius = sqrt (radsq)
			ratio = (radius - (DP_FITRAD(dao) + 1)) / radius
			deltax = ratio * deltax
			deltay = ratio * deltay

			# Determine which star is brighter.

			if (magcurr > rel_bright) {
			    rel_bright = magcurr *
			        dp_usepsf (DP_PSFUNCTION(psffit), deltax,
				deltay, DP_PSFHEIGHT(psffit),
				Memr[DP_PSFPARS(psffit)],
				Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
				DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit),
				dxcurr_frompsf, dycurr_frompsf, dvdx, dvdy)
			} else {
			    rel_bright = rel_bright *
			        dp_usepsf (DP_PSFUNCTION(psffit), -deltax,
			        -deltay, DP_PSFHEIGHT(psffit),
				Memr[DP_PSFPARS(psffit)],
				Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
				DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit),
				dxcrit_frompsf, dycrit_frompsf, dvdx, dvdy)
			}

			# Do the stars overlap ?

			if (! IS_INDEFR(skycurr) && ! IS_INDEFR(skycrit)) {
			    stand_err = read_noise + 0.5 * (skycurr +
			        skycrit) / DP_PHOTADU(dao)
			    if ((rel_bright * rel_bright) >= (stand_err *
			        critovlap))
			        overlap = true
			}
		    }   
				
		    # The two stars do overlap. Increment the pointers
		    # and move this star to the top of the stack.

		    if (overlap) {
			nin_currgrp = nin_currgrp + 1
			call dp_gswap2 (i, first_unknown, Memi[index], 
			    Memr[DP_APYCEN(apsel)])
			first_unknown = first_unknown + 1
		    }
		}

		curr_star = curr_star + 1
	    }

	    # Check for maximum group size.
	    if (nin_currgrp > maxgroup)
	        maxgroup = nin_currgrp

	    # Increment the number of groups versus size counter. Stars
	    # with undefined centers are always in a group with a membrship
	    # of 1 and are skipped.

	    if (! IS_INDEFR(xcurr) && ! IS_INDEFR(ycurr))
	        Memi[number+nin_currgrp-1] = Memi[number+nin_currgrp-1] + 1

	    # Define the group size.
	    Memi[group_size+first_ingrp-1] = nin_currgrp
	}		   

	# Write out all the groups to the output file.
	call dp_wrtgroup (dao, im, grp, Memi[number], Memi[index],
	    Memi[group_size], maxgroup)

	call mfree (number, TY_INT)
	call mfree (index, TY_INT)
	call mfree (group_size, TY_INT)

	# Restore the original fitting radius.
	DP_FITRAD(dao) = fradius
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)
end


# DP_GSWAP2 -- Interchange two stars in the photometry list and then shift
# the list.

procedure dp_gswap2 (star1, star2, index, y)

int	star1, star2		# the two star numbers to interchange
int	index[ARB]		# the index array
real	y[ARB]			# array of y positions

int	j, k, l, ihold
real	yhold

begin
	yhold = y[star1]
	ihold = index[star1]

	l = star1 + star2
	do j = star2, star1 - 1 {
	    k = l - j
	    y[k] = y[k-1]
	    index[k] = index [k-1]
	}

	y[star2] = yhold
	index[star2] = ihold
end


# DP_GCHKSTR -- Check to see if the unknown stars star is within one critical
# separation of the current star.

bool	procedure dp_gchkstar (x, y, index, i, xcurr, ycurr, crit, deltax,
	deltay, radsq)

real	x[ARB]			# array of x positions
real	y[ARB]			# array of y positions
int	index[ARB]		# index array from the quick sort
int	i			# position of test star in stack
real	xcurr			# x position of current star
real	ycurr			# y position of current star
real	crit 			# critical radius squared
real	deltax			# output difference in x
real	deltay			# output difference in y
real	radsq			# separation squared

begin
	# Initialize the deltas.
	deltax = MAX_REAL
	deltay = MAX_REAL

	# Check to see if star is within a critical radius. Reject the
	# star if any of the positions are INDEF.
	if (IS_INDEFR(xcurr) || IS_INDEFR(ycurr)) {
	    return (false)
	} else if (IS_INDEFR(y[i]) || IS_INDEFR(x[index[i]])) {
	    return (false)
	} else {
	    deltay = y[i] - ycurr
	    deltax = x[index[i]] -  xcurr
   	    radsq = deltax * deltax + deltay * deltay
	    if (radsq <= crit)
	        return (true)
	    else
 	        return (false)
	}
end


# DP_GPFIX -- Check for INDEF photometry and get estimate of magnitude.

procedure dp_gpfix (dao, im, bright_mag)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
real	bright_mag		# apparent magnitude of the brightest star

int	i, lowx, lowy, nxpix, nypix
pointer apsel, subim
real 	fitrad, fitradsq, mingdata, maxgdata, x, y, sky, mag

bool	fp_equalr()
pointer	dp_gsubrast()
real	dp_gaccum()

begin
	# Get pointers to the daophot structures.
	apsel = DP_APSEL (dao)
	
	# Initialize.
	fitrad = DP_FITRAD(dao)
	fitradsq = fitrad * fitrad
	if (IS_INDEFR (DP_MINGDATA(dao)))
	    mingdata = -MAX_REAL
	else
	    mingdata = DP_MINGDATA(dao)
	if (IS_INDEFR (DP_MAXGDATA(dao)))
	    maxgdata = MAX_REAL
	else
	    maxgdata = DP_MAXGDATA(dao)
	bright_mag = MAX_REAL

	# Get a magnitude estimate for stars with INDEF magnitudes by summing
	# all of the pixels within one fitting radius and scaling with respect
	# to the PSFMAG.

	do i = 1, DP_APNUM(apsel) {

	    # Check for undefined centers.
	    x = Memr[DP_APXCEN(apsel)+i-1]
	    y = Memr[DP_APYCEN(apsel)+i-1]
	    if (IS_INDEFR(x) || IS_INDEFR(y))
		next

	    # Check for an undefined sky.
	    sky = Memr[DP_APMSKY(apsel)+i-1]
	    if (IS_INDEFR(sky)) 
		next

	    # Correct the magnitudes.
	    mag = Memr[DP_APMAG(apsel)+i-1]
	    if (IS_INDEFR(mag)) {

		# Get subraster around star position. If the subraster
		# cannot be extracted leave the magnitude as INDEF.
		subim = dp_gsubrast (im, x, y, fitrad, lowx, lowy, nxpix,
		    nypix)
		if (subim == NULL) 
		    next

		# Estimate the value of the PSF.
		mag = dp_gaccum (dao, Memr[subim], nxpix, nypix, lowx, lowy,
		    x, y,  sky, fitradsq, mingdata, maxgdata)

		if (! IS_INDEFR(mag))
		    Memr[DP_APMAG(apsel)+i-1] = mag


	    } else if (mag < bright_mag)
		bright_mag = mag
	}	    

	if (fp_equalr (bright_mag, MAX_REAL))
	    bright_mag = INDEFR
end


# DP_GACCUM -- Accumulate the model counts.

real procedure dp_gaccum (dao, subim, nxpix, nypix, lowx, lowy, x, y, sky,
	fitradsq, mingdata, maxgdata)

pointer	dao			# pointer to the daophot structure
real	subim[nxpix,nypix]	# the input data subraster
int	nxpix, nypix		# the dimensions of the data subraster
int	lowx, lowy		# the lower left corner of the subraster
real	x, y			# the coordinates of the star
real	sky			# the sky value of the star
real	fitradsq		# the fitting radius of the star
real	mingdata, maxgdata	# the minimum and maximum good data values

int	k, j
pointer	psffit
real	mag, dxfrom_psf, dyfrom_psf, numer, denom, dx, dy, radsq, dvdx, dvdy
real	weight, value
real	dp_usepsf()

begin
	psffit = DP_PSFFIT(dao)

	# Compute the distance from the psf star.
	dxfrom_psf = (x - 1.0) / DP_PSFX(psffit) - 1.0
	dyfrom_psf = (y - 1.0) / DP_PSFY(psffit) - 1.0

	denom = 0.
	numer = 0.
	mag = INDEFR
	do k = 1, nypix {
	    do j = 1, nxpix {

		dx = real (lowx + j - 1) - x
		dy = real (lowy + k - 1) - y
		radsq = dx * dx + dy * dy
		if ((radsq >= fitradsq) || (subim[j,k] <  mingdata) ||
		    (subim[j,k] > maxgdata))
		    next

		value = dp_usepsf (DP_PSFUNCTION(psffit), dx, dy,
		    DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		    DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit),
		    dxfrom_psf, dyfrom_psf, dvdx, dvdy)
		radsq = radsq / fitradsq
		weight = 5. / (5. + radsq / (1.0 - radsq))
		numer = numer + weight * value * (subim[j,k] - sky)
		denom = denom + weight * value * value
	    }
	}

	if (denom > 0.0 && numer > 0.0)
	    mag = DP_PSFMAG(psffit) - 2.5 * log10 (numer / denom)

	return (mag)
end
