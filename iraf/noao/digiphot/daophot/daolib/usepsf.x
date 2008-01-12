include "../lib/daophotdef.h"

# DP_USEPSF -- Evaluate the psf at a given point.

real procedure dp_usepsf (ipstyp, dx, dy, bright, par, psf, npsf, nexp,
	nfrac, deltax, deltay, dvdxc, dvdyc)

int	ipstyp			# analytic psf function type
real	dx, dy			# distance for center of function
real	bright			# the relative brightness of the object
real	par[ARB]		# current values of the parameters
real    psf[npsf,npsf,ARB]	# the psf lookup tables	
int	npsf			# size of the psf look-up table
int	nexp			# number pf look-up tables
int	nfrac			# fractional pixel expansion
real	deltax, deltay		# distance from center of look-up tables
real	dvdxc, dvdyc		# derivatives with respect to position

int	nterm, j, k, lx, ly
real	psfval, middle, junk[MAX_NEXPTERMS], xx, yy, corr, dfdx, dfdy
real	dp_profile(), bicubic()

begin
	nterm = nexp + nfrac
	psfval = bright * dp_profile (ipstyp, dx, dy, par, dvdxc, dvdyc,
	    junk, 0)
	dvdxc = bright * dvdxc
	dvdyc = bright * dvdyc
	if (nterm <= 0)
	    return (psfval) 

	# The PSF look-up tables are centered at (MIDDLE, MIDDLE).

	switch (nexp) {
	case 1:
	    junk[1] = 1.
	case 3:
	    junk[1] = 1.
	    junk[2] = deltax
	    junk[3] = deltay
	case 6:
	    junk[1] = 1.
	    junk[2] = deltax
	    junk[3] = deltay
	    junk[4] = 1.5 * deltax ** 2 - 0.5
	    junk[5] = deltax * deltay
	    junk[6] = 1.5 * deltay ** 2 - 0.5
	}

	if (nfrac >  0) {
	    j = nexp + 1
	    junk[j] = - 2. * (dx - real(nint(dx)))
	    j = j + 1
	    junk[j] = - 2. * (dy - real(nint(dy)))
	    j = j + 1
	    junk[j] = 1.5 * junk[j-2] ** 2 - 0.5
	    j = j + 1
	    junk[j] = junk[j-3] * junk[j-2]
	    j = j + 1
	    junk[j] = 1.5 * junk[j-3] ** 2 - 0.5
	} 

	# This point in the stellar profile lies between columns LX and LX+1,
	# and between rows LY and LY+1 in the look-up tables.

	middle = (npsf + 1) / 2
	xx = (2. * dx) + middle
	lx = int (xx)
	yy = (2. * dy) + middle
	ly = int (yy)

	do k = 1, nterm {
	    corr = bicubic (psf[lx-1,ly-1,k], npsf, xx - real(lx),
	        yy - real(ly), dfdx, dfdy)
	    psfval = psfval + junk[k] * corr
	    dvdxc = dvdxc - junk[k] * dfdx
	    dvdyc = dvdyc - junk[k] * dfdy
	}

	return (psfval) 
end
