include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/psfdef.h"

# DP_PFSWAP -- Swap two stars in the PSF star list.

procedure dp_pfswap (dao, star1, star2)

pointer	dao			# pointer to the daophot structure
int	star1			# index of first star to be swapped
int	star2			# index of second star to be swapped

pointer	apsel, psf

begin
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)
	call dp_10swap (star1, star2, Memi[DP_APID(apsel)],
	    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
	    Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)], Memr[DP_PXCEN(psf)],
	    Memr[DP_PYCEN(psf)], Memr[DP_PH(psf)], Memr[DP_PMAG(psf)],
	    Memi[DP_PSAT(psf)])
end


# DP_PFREORDER -- Move a star in the PSF star list to the end of the
# list.

procedure dp_pfreorder (dao, star, nstars)

pointer	dao			# pointer to the daophot structure
int	star			# index of star to be deleted
int	nstars			# index of second star to be swapped

pointer	apsel, psf

begin
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)
	call dp_10reorder (star, Memi[DP_APID(apsel)], Memr[DP_APXCEN(apsel)],
	    Memr[DP_APYCEN(apsel)], Memr[DP_APMAG(apsel)],
	    Memr[DP_APMSKY(apsel)], Memr[DP_PXCEN(psf)], Memr[DP_PYCEN(psf)],
	    Memr[DP_PH(psf)], Memr[DP_PMAG(psf)], Memi[DP_PSAT(psf)], nstars)
end


# DP_APLSWAP -- Swap two stars in the daophot photometry substructure.

procedure dp_aplswap (dao, star1, star2)

pointer	dao			# pointer to the daophot structure
int	star1			# index of first star to be swapped
int	star2			# index of second star to be swapped

pointer	apsel

begin
	apsel = DP_APSEL(dao)
	call dp_5swap (star1, star2, Memi[DP_APID(apsel)],
	    Memr[DP_APXCEN(apsel)], Memr[DP_APYCEN(apsel)],
	    Memr[DP_APMAG(apsel)], Memr[DP_APMSKY(apsel)])
end


# DP_XYHPSF -- Compute the initial x, y, and height of the current PSF star.

procedure dp_xyhpsf (dao, star, mag, saturated)

pointer	dao			# pointer to the daophot structure
int	star			# star for which x, y, h is to be computed
real	mag			# magnitude of proposed psf star
int	saturated		# is the star saturated

pointer	apsel, psf, psffit
real	dhdxc, dhdyc, junk
real	dp_profile()

begin
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	Memr[DP_PXCEN(psf)+star-1] = Memr[DP_APXCEN(apsel)+star-1]
	Memr[DP_PYCEN(psf)+star-1] = Memr[DP_APYCEN(apsel)+star-1]
	if (saturated == YES)
	    Memr[DP_PH(psf)+star-1] = INDEFR
	else
	    Memr[DP_PH(psf)+star-1] = (DP_CUR_PSFGMAX(psf) -
	        Memr[DP_APMSKY(apsel)+star-1]) /
		dp_profile (DP_PSFUNCTION(psffit),
	        0.0, 0.0, Memr[DP_PSFPARS(psffit)], dhdxc, dhdyc, junk, 0)
	Memr[DP_PMAG(psf)+star-1] = mag
	Memi[DP_PSAT(psf)+star-1] = saturated
end


# DP_LISTPSF -- List the PSF stars.

procedure dp_listpsf (dao, im)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor

real	x, y
pointer	apsel, psf
int	i

begin
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)

	call printf ("\nCurrent PSF star list\n")
	do i = 1, DP_PNUM(psf) {
	    call dp_ltov (im, Memr[DP_APXCEN(apsel)+i-1],
	        Memr[DP_APYCEN(apsel)+i-1], x, y, 1)
	    call printf (
	        "    Star: %4d  X: %7.2f Y: %7.2f  Mag: %7.2f  Sky: %10.1f\n")
		call pargi (Memi[DP_APID(apsel)+i-1])
		call pargr (x)
		call pargr (y)
		call pargr (Memr[DP_APMAG(apsel)+i-1])
		call pargr (Memr[DP_APMSKY(apsel)+i-1])
	}
	call printf ("\n")
end


# DP_LISTPARS -- List the analytic PSF parameters.

procedure dp_listpars (dao)

pointer	dao			# pointer to the daophot structure

pointer	psffit

begin
	psffit = DP_PSFFIT(dao)
	call printf ("\nAnalytic PSF fit \n")
	    call printf (
	        "    Function: %s  X: %g  Y: %g  Height: %g  Psfmag: %g\n")
		call pargstr (DP_FUNCTION(dao))
		call pargr (DP_PSFX(psffit))
		call pargr (DP_PSFY(psffit))
		call pargr (DP_PSFHEIGHT(psffit))
		call pargr (DP_PSFMAG(psffit))

	switch (DP_PSFUNCTION(psffit)) {
	case FCTN_GAUSS:
	    call printf ("    Par1: %g  Par2: %g\n") 
		call pargr (Memr[DP_PSFPARS(psffit)])
		call pargr (Memr[DP_PSFPARS(psffit)+1])
	case FCTN_MOFFAT15:
	    call printf (
	        "    Par1: %g  Par2: %g  XYterm: %g  Moffat: %g\n") 
		call pargr (Memr[DP_PSFPARS(psffit)])
		call pargr (Memr[DP_PSFPARS(psffit)+1])
		call pargr (Memr[DP_PSFPARS(psffit)+2])
		call pargr (1.5)
	case FCTN_PENNY1:
	    call printf ("    Par1: %g  Par2: %g  Par3: %g  Par4: %g\n") 
		call pargr (Memr[DP_PSFPARS(psffit)])
		call pargr (Memr[DP_PSFPARS(psffit)+1])
		call pargr (Memr[DP_PSFPARS(psffit)+2])
		call pargr (Memr[DP_PSFPARS(psffit)+3])
	case FCTN_MOFFAT25:
	    call printf (
	        "    Par1: %g  Par2: %g  XYterm: %g  Moffat: %g\n") 
		call pargr (Memr[DP_PSFPARS(psffit)])
		call pargr (Memr[DP_PSFPARS(psffit)+1])
		call pargr (Memr[DP_PSFPARS(psffit)+2])
		call pargr (2.5)
	case FCTN_PENNY2:
	    call printf (
	    "    Par1: %g  Par2: %g  Par3: %g  Par4: %g  Par5: %g\n") 
		call pargr (Memr[DP_PSFPARS(psffit)])
		call pargr (Memr[DP_PSFPARS(psffit)+1])
		call pargr (Memr[DP_PSFPARS(psffit)+2])
		call pargr (Memr[DP_PSFPARS(psffit)+3])
		call pargr (Memr[DP_PSFPARS(psffit)+4])
	case FCTN_LORENTZ:
	    call printf ("    Par1: %g  Par2: %g  Par3: %g\n") 
		call pargr (Memr[DP_PSFPARS(psffit)])
		call pargr (Memr[DP_PSFPARS(psffit)+1])
		call pargr (Memr[DP_PSFPARS(psffit)+2])
	}
end


# DP_PSHOW -- Print photometry for the given star

procedure dp_pshow (dao, im, istar)

pointer	dao		# pointer to the main daophot descriptor
pointer im		# the input image descriptor
int	istar		# star to be printed

real	x, y
pointer	apsel

begin
	apsel = DP_APSEL(dao)
	call dp_ltov (im, Memr[DP_APXCEN(apsel)+istar-1],
	    Memr[DP_APYCEN(apsel)+istar-1], x, y, 1)
	call printf (
	    "Star: %4d X: %7.2f Y: %7.2f  Mag: %7.2f  Sky: %10.1f\n")
	    call pargi (Memi[DP_APID(apsel)+istar-1])
	    call pargr (x)
	    call pargr (y)
	    call pargr (Memr[DP_APMAG(apsel)+istar-1])
	    call pargr (Memr[DP_APMSKY(apsel)+istar-1])
end


# DP_10REORDER -- Move a PSF star to the end of the list.

procedure dp_10reorder (star, id, x, y, mag, sky, xfit, yfit, hfit, pmag,
	sat, nstars)

int	star			# star to be moved to the end of the list
int	id[ARB]			# the ids of the stars
real	x[ARB]			# the x positions of the stars
real	y[ARB]			# the y positions of the stars
real	mag[ARB]		# the magnitudes of the stars
real	sky[ARB]		# the sky values of the stars
real	xfit[ARB]		# the current x fit array
real	yfit[ARB]		# the current y fit array
real	hfit[ARB]		# the current height of the stars
real	pmag[ARB]		# the psf star list magnitude
int	sat[ARB]		# are the star saturated
int	nstars			# number of stars in the list

int	i, ihold, sfhold
real	xhold, yhold, mhold, shold, xfhold, yfhold, hfhold, mfhold

begin
	ihold = id[star]
	xhold = x[star]
	yhold = y[star]
	mhold = mag[star]
	shold = sky[star]
	xfhold = xfit[star]
	yfhold = yfit[star]
	hfhold = hfit[star]
	mfhold = pmag[star]
	sfhold = sat[star]

	do i = star + 1, nstars {
	    id[i-1] = id[i]
	    x[i-1] = x[i]
	    y[i-1] = y[i]
	    mag[i-1] = mag[i]
	    sky[i-1] = sky[i]
	    xfit[i-1] = xfit[i]
	    yfit[i-1] = yfit[i]
	    hfit[i-1] = hfit[i]
	    pmag[i-1] = pmag[i]
	    sat[i-1] = sat[i]
	}

	id[nstars] = ihold
	x[nstars]  = xhold
	y[nstars]  = yhold
	mag[nstars] = mhold
	sky[nstars] = shold
	xfit[nstars] = xfhold
	yfit[nstars] = yfhold
	hfit[nstars] = hfhold
	pmag[nstars] = mfhold
	sat[nstars] = sfhold
end


# DP_5SWAP -- Exchange the position of two stars in the APPHOT photometry
# results.

procedure dp_5swap (star1, star2, id, x, y, mag, sky)

int	star1, star2		# the indices of the two stars to exchange
int	id[ARB]			# the ids of the stars
real	x[ARB]			# the x postions of the stars
real	y[ARB]			# the y positions of the stars
real	mag[ARB]		# the magnitudes of the stars
real	sky[ARB]		# the sky values of the stars

int	ihold
real	xhold, yhold, mhold, shold

begin
	ihold = id[star1]
	xhold = x[star1]
	yhold = y[star1]
	mhold = mag[star1]
	shold = sky[star1]

	id[star1] = id[star2]
	x[star1]  = x[star2]
	y[star1]  = y[star2]
	mag[star1] = mag[star2]
	sky[star1] = sky[star2]

	id[star2] = ihold
	x[star2]  = xhold
	y[star2]  = yhold
	mag[star2] = mhold
	sky[star2] = shold
end


# DP_10SWAP -- Exchange the position of two stars in the APPHOT photometry
# and PSF fitting results.

procedure dp_10swap (star1, star2, id, x, y, mag, sky, xfit, yfit, hfit, pmag,
	sat)

int	star1, star2		# the indices of the two stars to exchange
int	id[ARB]			# the ids of the stars
real	x[ARB]			# the x postions of the stars
real	y[ARB]			# the y positions of the stars
real	mag[ARB]		# the magnitudes of the stars
real	sky[ARB]		# the sky values of the stars
real	xfit[ARB]		# the current x fit array
real	yfit[ARB]		# the current y fit array
real	hfit[ARB]		# the current height of the stars
real	pmag[ARB]		# the psf star list magnitudes
int	sat[ARB]		# are the star saturated

int	ihold, sfhold
real	xhold, yhold, mhold, shold, xfhold, yfhold, hfhold, mfhold

begin
	ihold = id[star1]
	xhold = x[star1]
	yhold = y[star1]
	mhold = mag[star1]
	shold = sky[star1]
	xfhold = xfit[star1]
	yfhold = yfit[star1]
	hfhold = hfit[star1]
	mfhold = pmag[star1]
	sfhold = sat[star1]

	id[star1] = id[star2]
	x[star1]  = x[star2]
	y[star1]  = y[star2]
	mag[star1] = mag[star2]
	sky[star1] = sky[star2]
	xfit[star1] = xfit[star2]
	yfit[star1] = yfit[star2]
	hfit[star1] = hfit[star2]
	pmag[star1] = pmag[star2]
	sat[star1] = sat[star2]

	id[star2] = ihold
	x[star2]  = xhold
	y[star2]  = yhold
	mag[star2] = mhold
	sky[star2] = shold
	xfit[star2] = xfhold
	yfit[star2] = yfhold
	hfit[star2] = hfhold
	pmag[star2] = mfhold
	sat[star2] = sfhold
end


# DP_ISSAT -- Is the candidate PSF star saturated ?

int procedure dp_issat (dao, starno)

pointer	dao		# pointer to the daophot structure
int	starno		# the star index number

pointer	psf

begin
	psf = DP_PSF(dao)
	if (Memi[DP_PSAT(psf)+starno-1] == YES)
	    return (YES)
	else
	    return (NO)
end
