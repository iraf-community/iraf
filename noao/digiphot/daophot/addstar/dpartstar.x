include <imhdr.h>
include <tbset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"

define	ADD_NINCOLUMN		4

# DP_ARTSTAR -- Add artificial stars to the data frames.

procedure dp_artstar (dao, iim, oim, cl, ofd, nstar, minmag, maxmag, iseed,
	coo_text, simple, offset, cache)

pointer	dao				# pointer to DAOPHOT structure
pointer	iim				# the input image descriptor
pointer	oim				# image to add stars to
int	cl				# fd of input photometry file
int	ofd				# fd of output photometry file
int	nstar				# number of stars to be added
real	minmag, maxmag			# min. and max. magnitudes to add
int	iseed[ARB]			# the random number generator array
bool	coo_text			# coordinate text file ?
int	simple				# simple text file ?
int	offset				# id offset for output photometry file
int	cache				# cache the output pixels

real	xmin, ymin, xwide, ywide, mwide, x, y, dxfrom_psf, dyfrom_psf, mag
real	radius, psfradsq, rel_bright, sky, tx, ty
pointer	sp, colpoint, psffit, subim, subim_new, indices, fields, key
int	i, iid, id, lowx, lowy, nxpix, nypix, nrow

pointer	dp_gsubrast(), imps2r(), tbpsta()
int	dp_gcoords(), dp_apsel()

begin
	# Get some memory.
	call smark (sp)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (indices, ADD_NINCOLUMN, TY_INT)
	call salloc (colpoint, ADD_NINCOLUMN, TY_INT)

	# Initialize the input photometry file.
	key = NULL
	if (cl != NULL) {
	    if (! coo_text) {
		call dp_tadinit (cl, Memi[indices])
		nrow = tbpsta (cl, TBL_NROWS)
	    #} else if (coo_text && simple == NO) {
	    } else if (simple == NO) {
	        call pt_kyinit (key)
	        Memi[indices] = DP_PAPID
	        Memi[indices+1] = DP_PAPXCEN
	        Memi[indices+2] = DP_PAPYCEN
	        Memi[indices+3] = DP_PAPMAG1
	        call dp_gappsf (Memi[indices], Memc[fields], ADD_NINCOLUMN)
	    }
	}

	# Initialize the output table.
	if (DP_TEXT(dao) == YES)
	    call dp_xnaddstar (dao, ofd)
	else
	    call dp_tnaddstar (dao, ofd, Memi[colpoint])

	# Get some daophot pointers.
	psffit = DP_PSFFIT (dao)

	# Get the psf radius
	if (DP_PSFSIZE(psffit) == 0)
	    radius = DP_PSFRAD(dao)
	else
	    radius = min (DP_PSFRAD(dao), (real (DP_PSFSIZE(psffit) - 1) /
	        2.0 - 1.0) / 2.0)
	psfradsq = radius * radius

	# Get the x and y limits for the random number generator. The 
	# magnitude limits are input by the user.
	xmin = 1.0
	xwide = real(IM_LEN(oim,1)) - 1.0
	ymin = 1.0
	ywide = real (IM_LEN(oim,2)) - 1.0
	mwide = maxmag - minmag

	if (DP_VERBOSE (dao) == YES) {
	    call printf ("OUTPUT IMAGE: %s\n")
		call pargstr (IM_HDRFILE(oim))
	}

	# Add the stars.
	i = 1
	repeat {

	    # Get the coords and magnitudes of the star.
	    if (cl == NULL) {
	        call dp_mkcoords (x, y, mag, xmin, xwide, ymin, ywide, minmag,
		    mwide, iseed)
		id = i + offset
		if (i > nstar)
		    break
	    } else if (! coo_text) {
		if (i > nrow)
		    break
		call dp_tadread (cl, Memi[indices], iid, x, y, mag, i)
	        call dp_win (dao, iim, x, y, x, y, 1)
		if (iid == 0)
		    iid = i + offset
		else
		    id = iid
	    } else {
		if (simple == YES) {
	            if (dp_gcoords (cl, x, y, mag, iid) == EOF)
		        break
		    if (iid == 0)
			id = i + offset
		    else
			id = iid
		} else {
		    if (dp_apsel (key, cl, Memc[fields], Memi[indices], iid, x,
			y, sky, mag) == EOF) 
		        break
		    if (iid == 0)
			id = i + offset
		    else
			id = iid
		}
	        call dp_win (dao, iim, x, y, x, y, 1)
	    }

	    # Increment the counter
	    i = i + 1

	    # Compute the psf coordinates.
	    call dp_wpsf (dao, iim, x, y, dxfrom_psf, dyfrom_psf, 1)
	    dxfrom_psf = (dxfrom_psf - 1.0) / DP_PSFX(psffit) - 1.0
	    dyfrom_psf = (dyfrom_psf - 1.0) / DP_PSFY(psffit) - 1.0

	    # Compute output coordinates.
	    call dp_wout (dao, iim, x, y, tx, ty, 1)

	    # Read in the subraster and compute the relative x-y position.
	    subim = dp_gsubrast (oim, x, y, radius, lowx, lowy, nxpix, nypix)
	    if (subim == NULL) {
	       call printf (
	       "    Star: %d - X: %6.2f  Y: %6.2f  Mag: %7.3f off image\n")
		    call pargi (id)
		    call pargr (tx)
		    call pargr (ty)
		    call pargr (mag)
		next
	    } else if (DP_VERBOSE (dao) == YES) {
	       call printf (
	           "    Added Star: %d - X: %6.2f  Y: %6.2f  Mag: %7.3f\n")
		    call pargi (id)
		    call pargr (tx)
		    call pargr (ty)
		    call pargr (mag)
	    }

	    if (DP_TEXT(dao) == YES)
		call dp_xwadd (ofd, id, tx, ty, mag)
	    else
		call dp_twadd (ofd, Memi[colpoint], id, tx, ty, mag, id)

	    # Get the relative brightness
	    rel_bright = DAO_RELBRIGHT (psffit, mag)

	    # Get the output buffer.
	    subim_new = imps2r (oim, lowx, lowx + nxpix - 1, lowy,
	        lowy + nypix - 1)

	    # Evaluate the PSF for a single star.
	    x = x - lowx + 1.0
	    y = y - lowy + 1.0
	    call dp_artone (dao, Memr[subim], nxpix, nypix, x, y, rel_bright,
	        dxfrom_psf, dyfrom_psf, psfradsq, DP_PHOTADU(dao), iseed)

	    # Make sure the image buffer is flushed. Currently this is a
	    # very inefficient way to do the image i/o.
	    call amovr (Memr[subim], Memr[subim_new], nxpix * nypix)
	    if (cache == NO)
	        call imflush (oim)

	}

	# Release the text file structure.
	if (key != NULL)
	    call pt_kyfree (key)

	call sfree (sp)
end


# DP_ARTONE -- Add a single star to the image.

procedure dp_artone (dao, subin, nxpix, nypix, x, y, rel_bright, xfrom_psf,
	yfrom_psf, psfradsq, gain, iseed)

pointer	dao				# pointer to the daophot structure
real	subin[nxpix,nypix]		# input subraster
int	nxpix, nypix			# dimensions of subrasters
real	x, y				# input position
real	rel_bright			# relative brightness
real	xfrom_psf			# x distance from the psf
real	yfrom_psf			# y distance from the psf
real	psfradsq			# psf radius squared
real	gain				# gain
int	iseed[ARB]			# random number seed array

int	ix, iy
pointer	psffit
real	dx, dy, dxsq, dysq, radsq, dvdx, dvdy, value, err
real	dp_usepsf(), dp_nrml(), urand()

begin
	psffit = DP_PSFFIT(dao)

	do iy = 1, nypix {
	    dy = real (iy) - y
	    dysq = dy * dy
	    do ix = 1, nxpix {
		dx = real (ix) - x
		dxsq = dx * dx
		radsq = dxsq + dysq
		if (radsq < psfradsq) {
		    value = rel_bright * dp_usepsf (DP_PSFUNCTION(psffit),
		        dx, dy, DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
			Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
			DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit),
			xfrom_psf, yfrom_psf, dvdx, dvdy)
		    err = urand (iseed[mod(ix+iy,3)+1])
		    err = sqrt (max (0.0, value / gain)) * dp_nrml (err)
		    subin[ix,iy] = subin[ix,iy] + value + err
		}
	    }
	}
end


# DP_MKCOORDS -- Construct a list of coordinates using a random number
# generator.

procedure dp_mkcoords (x, y, mag, xmin, xwide, ymin, ywide, minmag, mwide,
	iseed)

real	x		# x array
real	y		# y array
real	mag		# magnitude array
real	xmin		# xmin value
real	xwide		# x coordinate range
real	ymin		# ymin value
real	ywide		# y coordinate range
real	minmag		# minimum magnitude
real	mwide		# the magnitude range
int	iseed[ARB]	# seed array for random number genrator

real	urand()

begin
	x = xmin + urand (iseed[1]) * xwide
	y = ymin + urand (iseed[2]) * ywide
	mag = minmag + urand (iseed[3]) * mwide
end


# DP_SEED3 -- Seed the random number generator.

procedure dp_seed3 (seed, iseed)

int	seed			# initial seed value
int	iseed[ARB]		# the seed array

int	idum
real	urand()

begin
	idum = seed
	iseed[1] = int (524288.* urand (idum)) + 1
	iseed[2] = int (524288.* urand (idum)) + 1
	iseed[3] = int (524288.* urand (idum)) + 1
end


# DP_NRML -- Convert a uniform probability distribution to a Gaussian
# distribution with mean zero and standard deviation of unity.

real procedure dp_nrml (random)

real 	random			# input random number 

real p, sign, t

begin
	p = random
	sign = -1.0
	if (p > 0.5) {
	    p = p - 0.5
	    sign = 1.0
	} else if (p <= 0.0)
	    return (-1.0e20)

	t = sqrt (log (1.0 / (p * p)))
	return (sign * (t - (2.30753 + 0.27061 * t) / 
	    (1.0 + t * (0.99229 + t * 0.04481))))
end
