include <imhdr.h>
include <tbset.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"
include "../lib/apsel.h"

define	NCOLUMN		4

# DP_ADDSTAR -- Add artificial stars to the data frames.

procedure dp_addstar (dao, im, cl, ofd, nstar, minmag, maxmag, coo_text,
	simple, offset) 

pointer	dao				# pointer to DAOPHOT structure
pointer	im				# image to add stars to
int	cl				# fd of input photometry file
int	ofd				# fd of output photometry file
int	nstar				# number of stars to be added
real	minmag, maxmag			# min. and max. magnitudes to add
bool	coo_text			# coordinate text file ?
int	simple				# simple text file ?
int	offset				# id offset for output photometry file

int	i, iid, idum, id, lowx, lowy, nxpix, nypix, nrow
pointer	sp, colpoint, psffit, subim, subim_new, indices, fields, key
real	xmin, xmax, ymin, ymax, x, y, xfrom_psf, yfrom_psf, mag
real	radius, fitrad, psfradsq, rel_bright, sky

int	dp_gcoords(), dp_apsel()
long	clktime()
pointer	dp_gsubrast(), imps2r(), tbpsta()
real	ran3()

begin
	# Get some memory.
	call smark (sp)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (indices, NCOLUMN, TY_INT)
	call salloc (colpoint, NCOLUMN, TY_INT)

	# Initialize the input photometry file.
	if (cl != NULL) {
	    if (! coo_text) {
		call dp_tadinit (cl, Memi[indices])
		nrow = tbpsta (cl, TBL_NROWS)
	    } else if (coo_text && simple == NO) {
	        call pt_kyinit (key)
	        Memi[indices] = DP_PAPID
	        Memi[indices+1] = DP_PAPXCEN
	        Memi[indices+2] = DP_PAPYCEN
	        Memi[indices+3] = DP_PAPMAG1
	        call dp_gappsf (Memi[indices], Memc[fields], NCOLUMN)
	    }
	}

	# Initialize the output table.
	if (DP_TEXT(dao) == YES)
	    call dp_xnaddstar (dao, ofd)
	else
	    call dp_tnaddstar (dao, ofd, Memi[colpoint])

	# Get some constants.
	psffit = DP_PSFFIT (dao)
	radius = DP_PSFRAD(dao)
	psfradsq = radius * radius
	fitrad = DP_FITRAD(dao)
	xmin = fitrad
	xmax = IM_LEN(im,1) - fitrad
	ymin = fitrad
	ymax = IM_LEN(im,2) - fitrad

	# Initialize the random number generator.
	idum = -clktime (0)
	if (ran3 (idum) < 0.0)
	    ;

	# Add the stars.
	i = 1
	repeat {

	    # Get the coords and magnitudes of the star.
	    if (cl == NULL) {
	        call dp_mkcoords (x, y, mag, xmin, xmax, ymin, ymax, minmag,
		    maxmag, idum)
		id = i + offset
		if (i > nstar)
		    break
	    } else if (! coo_text) {
		if (i > nrow)
		    break
		call dp_tadread (cl, Memi[indices], iid, x, y, mag, i)
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
	    }

	    xfrom_psf = x - DP_XPSF(psffit)
	    yfrom_psf = y - DP_YPSF(psffit)

	    if (DP_VERBOSE (dao) == YES) {
	       call printf ("Added Star: %d - X: %6.2f  Y: %6.2f  Mag: %7.3f\n")
		    call pargi (id)
		    call pargr (x)
		    call pargr (y)
		    call pargr (mag)
	    }

	    if (DP_TEXT(dao) == YES)
		call dp_xwadd (ofd, id, x, y, mag)
	    else
		call dp_twadd (ofd, Memi[colpoint], id, x, y, mag, id)

	    # Read in the subraster and compute the relative x-y position.
	    subim = dp_gsubrast (im, x, y, radius, lowx, lowy, nxpix, nypix)
	    x = x - lowx + 1.0
	    y = y - lowy + 1.0

	    # Get the relative brightness
	    rel_bright = DAO_RELBRIGHT (psffit, mag)

	    # Get the output buffer.
	    subim_new = imps2r (im, lowx, lowx + nxpix - 1, lowy,
	        lowy + nypix - 1)

	    # Evaluate the PSF for a single star.
	    call dp_addone (Memr[subim], Memr[subim_new], nxpix, nypix, x, y,
		rel_bright, xfrom_psf, yfrom_psf, DP_VARPSF(dao), psfradsq,
		psffit, DP_PHOT_ADC(dao), idum)

	    # Make sure the image buffer is flushed. Currently this is a
	    # very inefficient way to do the image i/o.
	    call imflush (im)

	    i = i + 1
	}

	if (coo_text && simple == NO)
	    call pt_kyfree (key)

	call sfree (sp)
end


# DP_ADDONE -- Add a single star to the image.

procedure dp_addone (subin, subout, nxpix, nypix, x, y, rel_bright,
        xfrom_psf, yfrom_psf, varpsf, psfradsq, psffit, gain, idum)

real	subin[nxpix,nypix]		# input subraster
real	subout[nxpix,nypix]		# ouput subraster
int	nxpix, nypix			# dimensions of subrasters
real	x, y				# input position
real	rel_bright			# relative brightness
real	xfrom_psf			# x distance from the psf
real	yfrom_psf			# y distance from the psf
int	varpsf				# variable psf
real	psfradsq			# psf radius squared
pointer	psffit				# psffit structure
real	gain				# gain
int	idum				# random number generator seed

int	ix, iy
real	dx, dy, dxsq, dysq, radsq, dvdx, dvdy, value, err
real	dp_evalpsf(), dp_nrml(), ran3()

begin
	do iy = 1, nypix {
	    dy = real (iy) - y
	    dysq = dy * dy
	    do ix = 1, nxpix {
		dx = real (ix) - x
		dxsq = dx * dx
		radsq = dxsq + dysq
		if (radsq <= psfradsq) {
		    value = rel_bright * dp_evalpsf (dx, dy, psffit,
			xfrom_psf, yfrom_psf, varpsf, dvdx, dvdy)
		    err = sqrt (max (0.0, value / gain)) * dp_nrml (ran3 (idum))
		    subout[ix,iy] = subin[ix,iy] + value + err
		} else
		    subout[ix,iy] = subin[ix,iy]
	    }
	}
end


# DP_MKCOORDS -- Construct a list of coordinates using a random number
# generator.

procedure dp_mkcoords (x, y, mag, xmin, xmax, ymin, ymax, minmag, maxmag,
	idum)

real	x		# x array
real	y		# y array
real	mag		# magnitude array
real	xmin		# xmin value
real	xmax		# xmax value
real	ymin		# ymin value
real	ymax		# ymax value
real	minmag		# minimum magnitude
real	maxmag		# maximum magnitude
long	idum		# seed for random number genrator

real	ran3()

begin
	x = xmin + ran3 (idum) * xmax
	y = ymin + ran3 (idum) * ymax
	mag = minmag + ran3 (idum) * (maxmag - minmag)
end


# DP_NRML -- Convert a uniform probability distribution to a Gaussian
# distribution with mean zero and standard deviation unity.

real procedure dp_nrml (random)

real 	random			# random number generator seed.

real p, sign, t

begin
	p = random
	sign = -1.0
	if (p > 0.5) {
	    p = p - 0.5
	    sign = 1.0
	}
	t = sqrt (log10 (1.0 / (p * p)))

	return (sign * (t - (2.030753 + 0.27061 * t) / 
		       (1.0 + t * (0.99229 + t * 0.04481))))
end
