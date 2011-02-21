include <mach.h>
include <imhdr.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/psfdef.h"
include "../lib/peakdef.h"

# DP_FITPSF -- Compute the PSF function.

int procedure dp_fitpsf (dao, im, errmsg, maxch)

pointer	dao				# pointer to the daophot structure
pointer	im				# pointer to the input image
char	errmsg[ARB]			# string containing error message
int	maxch				# max characters in errmsg

int	nfunc, func
pointer	sp, flist, fstr, psf, psffit
int	dp_pstati(), dp_unsatstar(), dp_fitana(), dp_ifitana()
int	dp_fitlt(), dp_fctdecode(), dp_strwrd(), strdic()

begin
	# Get some daophot pointers.
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Test to see if there are any psf stars.
	if (dp_pstati (dao, PNUM) <= 0) {
	    call sprintf (errmsg, maxch, "The PSF star list is empty")
	    return (ERR)
	}

	# Test to see if there are any unsaturated stars. At the same time
	# make sure that the first star is unsaturated.
	if (dp_unsatstar (dao) <= 0) {
	    call sprintf (errmsg, maxch,
	        "There are no unsaturated PSF stars")
	    return (ERR)
	}

	# Determine the analytic function.
	call smark (sp)
	call salloc (flist, SZ_FNAME, TY_CHAR)
	call salloc (fstr, SZ_FNAME, TY_CHAR)
	call dp_stats (dao, FUNCLIST, Memc[flist], SZ_FNAME)
	nfunc = dp_fctdecode (Memc[flist], Memc[fstr], SZ_FNAME)
	func = dp_strwrd (1, Memc[fstr], SZ_FNAME, Memc[flist])
	func = strdic (Memc[fstr], Memc[fstr], SZ_LINE, FCTN_FTYPES)

	# Compute the analytic part of the PSF function.
	if (nfunc > 1 || func == FCTN_AUTO) {

	    # Loop over all the analytic functions.
	    if (func == FCTN_AUTO) {
		call strcpy (FCTN_FTYPES, Memc[flist], SZ_FNAME)
		nfunc = FCTN_NFTYPES
	    }

	    # Find the best fitting analytic function.
	    if (dp_ifitana (dao, im, Memc[flist], nfunc) == ERR) {
	        call sprintf (errmsg, maxch,
	            "Analytic function solution failed to converge")
		call sfree (sp)
	        return (ERR)
	    } else if (DP_VERBOSE(dao) == YES)
	        call dp_listpars (dao)

	} else {

	    # Save the analytic function for the new fit.
	    call strcpy (Memc[fstr], DP_FUNCTION(dao), SZ_LINE)

	    # Initialize the parameters.
	    call dp_reinit (dao)

	    # Fit the analytic part of the function.
	    if (dp_fitana (dao, im, Memr[DP_PXCEN(psf)], Memr[DP_PYCEN(psf)],
	        Memr[DP_PH(psf)], Memi[DP_PSAT(psf)], DP_PNUM(psf)) == ERR) {
	        call sprintf (errmsg, maxch,
	            "Analytic function solution failed to converge")
		call sfree (sp)
	        return (ERR)
	    } else if (DP_VERBOSE(dao) == YES) {
	        call printf ("\nFitting function %s    norm scatter: %g\n")
		    call pargstr (DP_FUNCTION(dao))
		    call pargr (DP_PSIGANA(psf))
	        call dp_listpars (dao)
	    }
	}
	call sfree (sp)

	# Compute the look-up table.
	if (dp_fitlt (dao, im) == ERR) {
	    call sprintf (errmsg, maxch,
	        "Too few stars to compute PSF lookup tables")
	    return (ERR)
	} else if (DP_VERBOSE(dao) == YES) {
	    call printf ("\nComputed %d lookup table(s)\n")
	        call pargi (DP_NVLTABLE(psffit)+DP_NFEXTABLE(psffit))
	}

	return (OK)
end


# DP_UNSATSTAR -- Make sure there is at least one unsaturated star.

int procedure dp_unsatstar (dao)

pointer	dao			# pointer to the daophot structure

int	i, first_unsat, nstar
pointer	psf

begin
	psf = DP_PSF(dao)
	first_unsat = 0

	nstar = 0
	do i = 1, DP_PNUM(psf) {
	    if (Memi[DP_PSAT(psf)+i-1] == YES)
		next
	    nstar = nstar + 1
	    if (first_unsat == 0)
		first_unsat = i
	}

	if (first_unsat > 1)
	    call dp_pfswap (dao, 1, first_unsat)

	return (nstar)
end


# DP_REINIT -- Reinitialize the psf function parameters.

procedure dp_reinit (dao)

pointer	dao			# pointer to the daophot structure

pointer	psffit
bool	streq()

begin
	psffit = DP_PSFFIT(dao)

	# Define the psf function.
	if (streq (DP_FUNCTION(dao), "gauss"))
	    DP_PSFUNCTION(psffit) = FCTN_GAUSS
	else if (streq (DP_FUNCTION(dao), "moffat25"))
	    DP_PSFUNCTION(psffit) = FCTN_MOFFAT25
	else if (streq (DP_FUNCTION(dao), "moffat15"))
	    DP_PSFUNCTION(psffit) = FCTN_MOFFAT15
	else if (streq (DP_FUNCTION(dao), "penny1"))
	    DP_PSFUNCTION(psffit) = FCTN_PENNY1
	else if (streq (DP_FUNCTION(dao), "penny2"))
	    DP_PSFUNCTION(psffit) = FCTN_PENNY2
	else if (streq (DP_FUNCTION(dao), "lorentz"))
	    DP_PSFUNCTION(psffit) = FCTN_LORENTZ
	else
	    call error (0, "Unknown PSF function\n")

	switch (DP_VARORDER(dao)) {
	case -1:
	    DP_NVLTABLE(psffit) = 0
	case 0:
	    DP_NVLTABLE(psffit) = 1
	case 1:
	    DP_NVLTABLE(psffit) = 3
	case 2:
	    DP_NVLTABLE(psffit) = 6
	}
	if (DP_FEXPAND(dao) == NO)
	    DP_NFEXTABLE(psffit) = 0
	else
	    DP_NFEXTABLE(psffit) = 5

	# Set the initial values of the function parameters.
	switch (DP_PSFUNCTION(psffit)) {
	case FCTN_GAUSS:
	    DP_PSFNPARS(psffit) = 2
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dao) / 2.0
	case FCTN_MOFFAT25:
	    DP_PSFNPARS(psffit) = 3
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.0
	    Memr[DP_PSFPARS(psffit)+3] = 2.5
	case FCTN_MOFFAT15:
	    DP_PSFNPARS(psffit) = 3
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.0
	    Memr[DP_PSFPARS(psffit)+3] = 1.5
	case FCTN_PENNY1:
	    DP_PSFNPARS(psffit) = 4
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.75
	    Memr[DP_PSFPARS(psffit)+3] = 0.0
	case FCTN_PENNY2:
	    DP_PSFNPARS(psffit) = 5
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.75
	    Memr[DP_PSFPARS(psffit)+3] = 0.0
	    Memr[DP_PSFPARS(psffit)+4] = 0.0
	case FCTN_LORENTZ:
	    DP_PSFNPARS(psffit) = 3
	    Memr[DP_PSFPARS(psffit)] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+1] = DP_FWHMPSF(dao) / 2.0
	    Memr[DP_PSFPARS(psffit)+2] = 0.0
	default:
	    call error (0, "Unknown PSF function\n")
	}
end


# DP_IFITANA -- Fit the PSF stars to each of the analytic functions in
# turn to determine which one gives the best fit.

int procedure dp_ifitana (dao, im, funclist, nfunc)

pointer	dao				# pointer to the daophot structure
pointer	im				# pointer to the input image
char	funclist			# the list of functions to be fit
int	nfunc				# number of functions

int	i, psftype, npars
pointer	psf, psffit, sp, fstr, func
pointer	ixtmp, iytmp, ihtmp, istmp, xtmp, ytmp, htmp, ptmp, stmp
real	osig, osum, height, dhdxc, dhdyc, junk, ofactor, factor
int	dp_strwrd(), strdic(), dp_fitana()
real	dp_profile()

begin
	# Get some pointers.
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Allocate some temporary storage space.
	call smark (sp)
	call salloc (fstr, SZ_FNAME, TY_CHAR)
	call salloc (func, SZ_FNAME, TY_CHAR)

	call salloc (ixtmp, DP_PNUM(psf), TY_REAL)
	call salloc (iytmp, DP_PNUM(psf), TY_REAL)
	call salloc (ihtmp, DP_PNUM(psf), TY_REAL)
	call salloc (istmp, DP_PNUM(psf), TY_INT)

	call salloc (xtmp, DP_PNUM(psf), TY_REAL)
	call salloc (ytmp, DP_PNUM(psf), TY_REAL)
	call salloc (htmp, DP_PNUM(psf), TY_REAL)
	call salloc (stmp, DP_PNUM(psf), TY_INT)
	call salloc (ptmp, MAX_NFCTNPARS, TY_REAL)

	# Initialize.
	call strcpy (DP_FUNCTION(dao), Memc[func], SZ_FNAME)
	npars = 0
	osig = MAX_REAL
	call amovr (Memr[DP_PXCEN(psf)], Memr[ixtmp], DP_PNUM(psf))
	call amovr (Memr[DP_PYCEN(psf)], Memr[iytmp], DP_PNUM(psf))
	call amovr (Memr[DP_PH(psf)], Memr[ihtmp], DP_PNUM(psf))
	call amovi (Memi[DP_PSAT(psf)], Memi[istmp], DP_PNUM(psf))
	ofactor = dp_profile (DP_PSFUNCTION(psffit), 0.0, 0.0,
	    Memr[DP_PSFPARS(psffit)], dhdxc, dhdyc, junk, 0)

	factor = 1
	do i = 1, nfunc {

	    # Get the function name and set it.
	    if (dp_strwrd (i, Memc[fstr], SZ_FNAME, funclist) <= 0)
		next
	    if (strdic (Memc[fstr], Memc[fstr], SZ_FNAME, FCTN_FTYPES) <= 0)
		next
	    call strcpy (Memc[fstr], DP_FUNCTION(dao), SZ_FNAME)

	    # Start from the same initial state.
	    call dp_reinit (dao)
	    call amovr (Memr[ixtmp], Memr[xtmp], DP_PNUM(psf))
	    call amovr (Memr[iytmp], Memr[ytmp], DP_PNUM(psf))
	    if (i == 1)
	        call amovr (Memr[ihtmp], Memr[htmp], DP_PNUM(psf))
	    else {
		factor = ofactor / dp_profile (DP_PSFUNCTION(psffit), 0.0, 0.0,
	    	    Memr[DP_PSFPARS(psffit)], dhdxc, dhdyc, junk, 0)
		call amulkr (Memr[ihtmp], factor, Memr[htmp], DP_PNUM(psf))
	    }
	    call amovi (Memi[istmp], Memi[stmp], DP_PNUM(psf))

	    call printf ("Trying function %s norm scatter = ")
		call pargstr (Memc[fstr])

	    # Do the fit.
	    if (dp_fitana (dao, im, Memr[xtmp], Memr[ytmp], Memr[htmp],
	        Memi[stmp], DP_PNUM(psf)) == ERR) {
		call printf ("error\n")
		next
	    } else {
		call printf ("%g\n")
		    call pargr (DP_PSIGANA(psf))
	    }

	    # Save the better fit.
	    if (DP_PSIGANA(psf) < osig) {
		call strcpy (Memc[fstr], Memc[func], SZ_FNAME)
		psftype = DP_PSFUNCTION(psffit)
		height = DP_PSFHEIGHT(psffit)
		npars = DP_PSFNPARS(psffit)
		call amovr (Memr[DP_PSFPARS(psffit)], Memr[ptmp],
		    MAX_NFCTNPARS)
	        call amovr (Memr[xtmp], Memr[DP_PXCEN(psf)], DP_PNUM(psf))
	        call amovr (Memr[ytmp], Memr[DP_PYCEN(psf)], DP_PNUM(psf))
	        call amovr (Memr[htmp], Memr[DP_PH(psf)], DP_PNUM(psf))
	        call amovi (Memi[stmp], Memi[DP_PSAT(psf)], DP_PNUM(psf))
		osig = DP_PSIGANA(psf)
		osum = DP_PSUMANA(psf)
	    }
	}

	# Restore the best fit parameters.
	if (npars > 0) {
	    call strcpy (Memc[func], DP_FUNCTION(dao), SZ_FNAME)
	    DP_PSFUNCTION(psffit) = psftype
	    DP_PSFHEIGHT(psffit) = height
	    DP_PSFNPARS(psffit) = npars
	    DP_PSIGANA(psf) = osig
	    DP_PSUMANA(psf) = osum
	    call amovr (Memr[ptmp], Memr[DP_PSFPARS(psffit)],
	        MAX_NFCTNPARS)
	    call printf ("Best fitting function is %s\n")
		call pargstr (DP_FUNCTION(dao))
	}

	# Cleanup.
	call sfree (sp)

	if (npars > 0)
	    return (OK)
	else
	    return (ERR)
end


# DP_FITANA -- Fit the analytic part of the psf function

int procedure dp_fitana (dao, im, pxcen, pycen, ph, pstat, npsfstars)

pointer	dao				# pointer to the daophot structure
pointer	im				# pointer to the input image
real	pxcen[ARB]			# x coordinates of the psf stars
real	pycen[ARB]			# y coordinates of the psf stars
real	ph[ARB]				# heights of the psf stars
int	pstat[ARB]			# saturation status of psf stars
int	npsfstars			# the number of psf stars

int	i, niter, istar, mpar, lx, ly, nx, ny, ier
pointer	apsel, psf, psffit, data
real	fitrad, rsq, oldchi, sumfree
pointer	imgs2r()

begin
	# Get the psf fitting structure pointer.
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Define some variables.
	oldchi = 0.0
	mpar = 2
	fitrad = DP_FITRAD(dao)
	rsq = fitrad ** 2

	# Get some memory.
	call dp_amempsf (dao)

	# Initialize the fit.
	call amovkr (0.5, Memr[DP_PCLAMP(psf)], DP_PSFNPARS(psffit))
	call aclrr (Memr[DP_PZ(psf)], DP_PSFNPARS(psffit))
	call aclrr (Memr[DP_POLD(psf)], DP_PSFNPARS(psffit))
	Memr[DP_PCLAMP(psf)] = 2.0
	Memr[DP_PCLAMP(psf)+1] = 2.0

	call aclrr (Memr[DP_PXOLD(psf)], npsfstars)
	call aclrr (Memr[DP_PYOLD(psf)], npsfstars)
	call amovkr (1.0, Memr[DP_PXCLAMP(psf)], npsfstars)
	call amovkr (1.0, Memr[DP_PYCLAMP(psf)], npsfstars)

	# Iterate.
	do niter = 1, MAX_NPSFITER {

	    # Initialize the current integration.
	    call aclrr (Memr[DP_PV(psf)], DP_PSFNPARS(psffit)) 
	    call aclrr (Memr[DP_PC(psf)], DP_PSFNPARS(psffit) *
	        DP_PSFNPARS(psffit)) 

	    # Loop over the stars.
	    DP_PSIGANA(psf) = 0.0
	    DP_PSUMANA(psf) = 0.0
	    do istar = 1, npsfstars {

		# Test for saturation.
		if (pstat[istar] == YES)
		    next

		# Define the subraster to be read in.
		lx = int (pxcen[istar] - fitrad) + 1
		ly = int (pycen[istar] - fitrad) + 1
		nx = (int (pxcen[istar] + fitrad) - lx) + 1
		ny = (int (pycen[istar] + fitrad) - ly) + 1

		# Is the star off the image?
		if (lx > IM_LEN(im,1) || ly > IM_LEN(im,2) || (lx + nx - 1) <
		    1 || (ly + ny - 1) < 1) {
		    if (DP_VERBOSE(dao) == YES) {
		        call printf ("Star %d is outside the image\n")
			    call pargi (Memi[DP_APID(apsel)+istar-1])
		    }
		    next
		}

		# Is the star too near the edge of the frame?
		if (lx < 1 || ly < 1 || (lx + nx - 1) > IM_LEN(im,1) ||
		    (ly + ny - 1) > IM_LEN(im,2)) {
		    if (DP_VERBOSE(dao) == YES) {
		        call printf (
			    "Star %d is too near the edge of the image\n")
			    call pargi (Memi[DP_APID(apsel)+istar-1])
		    }
		    next
		}

		# Read in the subraster.
		data = imgs2r (im, lx, lx + nx - 1, ly, ly + ny - 1)

		# Fit x, y, and height for the PSF star istar.
		call dp_xyhiter (DP_PSFUNCTION(psffit),
		    Memr[DP_PSFPARS(psffit)], rsq, Memr[data], nx, ny, lx, ly,
		    pxcen[istar], pycen[istar],
		    Memr[DP_APMSKY(apsel)+istar-1], ph[istar],
		    Memr[DP_PXCLAMP(psf)+istar-1],
		    Memr[DP_PYCLAMP(psf)+istar-1], Memr[DP_PXOLD(psf)+istar-1],
		    Memr[DP_PYOLD(psf)+istar-1])

		# Fit the parameters for the entire list of stars
		call dp_paccum (DP_PSFUNCTION(psffit),
		    Memr[DP_PSFPARS(psffit)], DP_PSFNPARS(psffit), mpar, rsq,
		    Memr[data], nx, ny, lx, ly, pxcen[istar],
		    pycen[istar], Memr[DP_APMSKY(apsel)+istar-1],
		    ph[istar], niter, Memr[DP_PC(psf)],
		    Memr[DP_PV(psf)], Memr[DP_PTMP(psf)], DP_PSIGANA(psf),
		    DP_PSUMANA(psf))
	    }

	    # Invert the matrix and compute the new parameters.
	    call invers (Memr[DP_PC(psf)], DP_PSFNPARS(psffit), mpar, ier)
	    call mvmul (Memr[DP_PC(psf)], DP_PSFNPARS(psffit), mpar,
	        Memr[DP_PV(psf)], Memr[DP_PZ(psf)])

	    do  i = 1, mpar {
	        if ((Memr[DP_PZ(psf)+i-1] * Memr[DP_POLD(psf)+i-1]) < 0.0)
		    Memr[DP_PCLAMP(psf)+i-1] = 0.5 *
		        Memr[DP_PCLAMP(psf)+i-1]
		else
		    Memr[DP_PCLAMP(psf)+i-1] = 1.1 *
		        Memr[DP_PCLAMP(psf)+i-1]
	    }
	    call amovr (Memr[DP_PZ(psf)], Memr[DP_POLD(psf)], mpar)
	    call amulr (Memr[DP_PZ(psf)], Memr[DP_PCLAMP(psf)],
		Memr[DP_PZ(psf)], mpar)

	    Memr[DP_PZ(psf)] = max (-0.1 * Memr[DP_PSFPARS(psffit)],
		min (0.1 * Memr[DP_PSFPARS(psffit)], Memr[DP_PZ(psf)]))
	    Memr[DP_PZ(psf)+1] = max (-0.1 * Memr[DP_PSFPARS(psffit)+1],
		min (0.1 * Memr[DP_PSFPARS(psffit)+1], Memr[DP_PZ(psf)+1]))
	    #if (mpar > 2)
	        #Memr[DP_PZ(psf)+2] = Memr[DP_PZ(psf)+2] /
		    #(1.0 + abs (Memr[DP_PZ(psf)+2]) /
		    #(min (0.1, 1.0 - abs (Memr[DP_PSFPARS(psffit)+2]))))
	    call aaddr (Memr[DP_PSFPARS(psffit)], Memr[DP_PZ(psf)],
		Memr[DP_PSFPARS(psffit)], mpar)

	    # Check for convergence.
	    sumfree = DP_PSUMANA(psf) - real (mpar + 3 * npsfstars)
	    if (sumfree > 0.0 && DP_PSIGANA(psf) >= 0.0)
		DP_PSIGANA(psf) = sqrt (DP_PSIGANA(psf) / sumfree)
	    else
		DP_PSIGANA(psf) = 9.999

	    if (mpar == DP_PSFNPARS(psffit)) {
		if (abs (oldchi / DP_PSIGANA(psf) - 1.0) < 1.0e-5) {
		    DP_PSFHEIGHT(psffit) = ph[1]
		    if (IS_INDEFR(Memr[DP_PMAG(psf)]))
		        DP_PSFMAG(psffit)  = Memr[DP_APMAG(apsel)]
		    else
		        DP_PSFMAG(psffit)  = Memr[DP_PMAG(psf)]
		    DP_PSFX(psffit) = real (IM_LEN(im,1) - 1) / 2.0
		    DP_PSFY(psffit) = real (IM_LEN(im,2) - 1) / 2.0
		    return (OK)
		} else
	    	    oldchi = DP_PSIGANA(psf)
	    } else {
		if (abs (oldchi / DP_PSIGANA(psf) - 1.0) < 1.0e-3) {
		    mpar = mpar + 1
		    oldchi = 0.0
		} else
	    	    oldchi = DP_PSIGANA(psf)
	    }
	}

	return (ERR)
end


# DP_XYHITER -- Increment the initial x, y, and height values for a star.

procedure dp_xyhiter (psftype, params, rsq, data, nx, ny, lx, ly, x, y, sky, h,
	xclamp, yclamp, xold, yold)

int	psftype		# analytic point spread function type
real	params[ARB]	# current function parameter values
real	rsq		# the fitting radius squared
real	data[nx,ARB]	# the input image data
int	nx, ny		# the dimensions of the input image data
int	lx, ly		# the coordinates of the ll corner of the image data
real	x, y		# the input/output stellar coordinates
real	sky		# the input sky value
real	h		# the input/output height value
real	xclamp, yclamp	# the input/output clamping factors for x and y
real	xold, yold	# the input/output x and y correction factors

int	i, j
real	dhn, dhd, dxn, dxd, dyn, dyd, dx, dy, wt, dhdxc, dhdyc, junk, p, dp
real	prod
real	dp_profile()

begin
	dhn = 0.0
	dhd = 0.0
	dxn = 0.0
	dxd = 0.0
	dyn = 0.0
	dyd = 0.0

	do j = 1, ny {
	    dy = real ((ly + j) - 1) - y
	    do i = 1, nx {
	        dx = real ((lx + i) - 1) - x
		wt = (dx ** 2 + dy ** 2) / rsq
		#if (wt >= 1.0)
		if (wt >= 0.999998)
		    next
		p = dp_profile (psftype, dx, dy, params, dhdxc, dhdyc, junk, 0)
		dp = data[i,j] - h * p - sky
		dhdxc = dhdxc * h
		dhdyc = dhdyc * h
		wt = 5.0 / (5.0 + (wt / (1.0 - wt)))
		prod = wt * p
		dhn = dhn + prod * dp
		dhd = dhd + prod * p
		prod = wt * dhdxc
		dxn = dxn + prod * dp
		dxd = dxd + prod * dhdxc
		prod = wt * dhdyc
		dyn = dyn + prod * dp
		dyd = dyd + prod * dhdyc
	    }
	}

	h = h + (dhn / dhd)
	dxn = dxn / dxd
	if ((xold * dxn) < 0.0)
	    xclamp = 0.5 * xclamp
	xold = dxn
	x = x + (dxn / (1.0 + (abs(dxn) / xclamp)))
	dyn = dyn / dyd
	if ((yold * dyn) < 0.0)
	    yclamp = 0.5 * yclamp
	yold = dyn
	y = y + (dyn / (1.0 + (abs(dyn) / yclamp)))
end


# DP_PACCUM -- Accumulate the data for the parameter fit.

procedure dp_paccum (psftype, params, npars, mpars, rsq, data, nx, ny, lx,
	ly, x, y, sky, h, iter, c, v, temp, chi, sumwt)

int	psftype		# analytic point spread function type
real	params[ARB]	# current function parameter values
int	npars		# number of function parameters
int	mpars		# the number of active parameters
real	rsq		# the fitting radius squared
real	data[nx,ARB]	# the input image data
int	nx, ny		# the dimensions of the input image data
int	lx, ly		# the coordinates of the ll corner of the image data
real	x, y		# the input/output stellar coordinates
real	sky		# the input sky value
real	h		# the input/output height value
int	iter		# the current iteration
real	c[npars,ARB]	# accumulation matrix
real	v[ARB]		# accumulation vector
real	temp[ARB]	# temporary storage vector
real	chi		# the chi sum
real	sumwt		# the number of points sum

int	i, j, k, l
real	peak, dx, dy, wt, dhdxc, dhdyc, p, dp
real	dp_profile()

begin
	peak = h * dp_profile (psftype, 0.0, 0.0, params, dhdxc, dhdyc, temp, 0)
	do j = 1, ny {
	    dy = real ((ly + j) - 1) - y
	    do i = 1, nx {
	        dx = real ((lx + i) - 1) - x
		wt = (dx ** 2 + dy ** 2) / rsq
		#if (wt >= 1.0)
		if (wt >= 0.999998)
		    next
		p = dp_profile (psftype, dx, dy, params, dhdxc, dhdyc, temp, 1) 
		dp = data[i,j] - h * p - sky
		do k = 1, mpars
		    temp[k] = h * temp[k]
		chi = chi + (dp / peak) ** 2
		sumwt = sumwt + 1.0
		wt = 5.0 / (5.0 + (wt / (1.0 - wt)))
		if (iter >= 4)
		    wt = wt / (1.0 + abs (20.0 * dp / peak))
		do k = 1, mpars {
		    v[k] = v[k] + wt * dp * temp[k]
		    do l = 1, mpars {
			c[l,k] = c[l,k] + wt * temp[l] * temp[k]
		    }
		}
	    }
	}
end


# DP_FITLT -- Given the analytic function compute the lookup tables.

int procedure dp_fitlt (dao, im)

pointer	dao		# pointer to the daophot structure
pointer	im		# pointer to the input image

int	istar, nexp, lx, mx, ly, my, iter, nclean, ndata, fit_saturated, nfit
int	nunsat
double	volume
pointer	apsel, psf, psffit, sp, wimname, wim, data
real	datamin, datamax, sumfree, resid, dfdx, dfdy, junk
int	dp_dclean(), dp_resana(), dp_ltcompute(), dp_fsaturated()
double	dp_pnorm()
pointer	immap(), imps2r(), dp_subrast()
real	dp_profile(), dp_sweight()
define	fitsaturated_	11

begin
	# Get some pointers.
	apsel = DP_APSEL(dao)
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Check to see whether lookup tables are required.
	nexp = DP_NVLTABLE(psffit) + DP_NFEXTABLE(psffit)
	if (nexp <= 0)
	    return (OK)

	# Return if there are too few stars to fit the lookup tables.
	if (DP_PNUM(psf) < nexp)
	    return (ERR)

	# Determine the number of saturated stars.
	nunsat = 0
	do istar = 1, DP_PNUM(psf) {
	    if (Memi[DP_PSAT(psf)+istar-1] == NO)
		next
	    if ((Memr[DP_PH(psf)+istar-1] * dp_profile (DP_PSFUNCTION(psffit),
	        0.0, 0.0, Memr[DP_PSFPARS(psffit)], dfdx, dfdy, junk, 0) +
		Memr[DP_APMSKY(apsel)+istar-1]) <= datamax)
		next
	    Memi[DP_PSAT(psf)+istar-1] = YES
	    Memr[DP_PH(psf)+istar-1] = INDEFR
	    nunsat = nunsat + 1
	}
	nunsat = DP_PNUM(psf) - nunsat

	# Return if there are too few unsaturated psf stars to fit the lookup
	# tables.
	if (nunsat < nexp)
	    return (ERR)

	# Allocate memory for computing lookup tables.
	call dp_tmempsf (dao)

	# Define some constants.
	fit_saturated = DP_SATURATED(dao)
	if (IS_INDEFR(DP_MINGDATA(dao)))
	    datamin = -MAX_REAL
	else
	    datamin = DP_MINGDATA(dao)
	if (IS_INDEFR(DP_MAXGDATA(dao)))
	    datamax = MAX_REAL
	else
	    datamax = DP_MAXGDATA(dao)
	sumfree = sqrt (DP_PSUMANA(psf) / (DP_PSUMANA(psf) - (nexp +
	    3.0 * DP_PNUM(psf))))

	# Get the image name.
	call smark (sp)
	call salloc (wimname, SZ_FNAME, TY_CHAR)
	call mktemp ("tmp", Memc[wimname], SZ_FNAME)

	# Open a temporary image to hold the weights.
	wim = immap (Memc[wimname], NEW_IMAGE, 0)
	IM_NDIM(wim) = 2
	IM_LEN(wim,1) = DP_PNUM(psf)
	IM_LEN(wim,2) = DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit) 
	IM_PIXTYPE(wim) = TY_REAL

	# Compute the constant part of the psf in preparation for normalizing
	# the lookup tables.
	if (nexp > 1)
	    call dp_pconst (DP_PSFUNCTION(psffit), Memr[DP_PSFPARS(psffit)],
	        Memr[DP_PH(psf)], Memr[DP_PCONST(psf)], DP_PSFSIZE(psffit))

	nfit = 0

fitsaturated_

	# Compute the look-up table.
	do iter = 1, DP_NCLEAN(dao) + 1 {

	    # Initialize the fitting arrays.
	    call aclrr (Memr[DP_PV(psf)], nexp)
	    call aclrr (Memr[DP_PC(psf)], nexp * nexp)
	    call aclrr (Memr[DP_PSUMN(psf)], DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit))
	    call aclrr (Memr[DP_PSUMSQ(psf)], DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit))
	    call aclrr (Memr[DP_PSFLUT(psffit)], nexp * DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit))

	    # Loop over the PSF stars.
	    do istar = 1, DP_PNUM(psf) {

		# Get the weight image.
		DP_PSUMW(psf) = imps2r (wim, istar, istar, 1,
		    DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit)) 
	        call aclrr (Memr[DP_PSUMW(psf)], DP_PSFSIZE(psffit) *
	            DP_PSFSIZE(psffit))

		# Skip saturated star?
		if (IS_INDEFR(Memr[DP_PH(psf)+istar-1]))
		    next

	        # Get the data.
	        data = dp_subrast (im, Memr[DP_PXCEN(psf)+istar-1],
		    Memr[DP_PYCEN(psf)+istar-1], DP_PSFRAD(dao), lx, mx,
		    ly, my)

	        # Is the star off the image?
	        if (lx > IM_LEN(im,1) || ly > IM_LEN(im,2) || mx < 1 ||
		    my < 1) {
		    if (DP_VERBOSE(dao) == YES) {
		        call printf ("Star %d is outside the image\n")
		            call pargi (Memi[DP_APID(apsel)+istar-1])
		    }
		    next
	        }

		# Clean bad pixels outside the fitting radius but inside
		# the psf radius from the subraster.
		nclean = dp_dclean (Memr[data], (mx - lx + 1), (my - ly + 1),
		    lx, ly, Memr[DP_PXCEN(psf)+istar-1], Memr[DP_PYCEN(psf)+
		    istar-1], DP_FITRAD(dao), datamin, datamax)

	        # Subtract the analytic part of the fit from the data
		# and compute the normalized residual of the star.
	        ndata = dp_resana (im, DP_PSFUNCTION(psffit),
		    Memr[DP_PSFPARS(psffit)], Memr[data], (mx - lx + 1),
		    (my - ly + 1), lx, ly, Memr[DP_PXCEN(psf)],
		    Memr[DP_PYCEN(psf)], Memr[DP_APMSKY(apsel)],
		    Memr[DP_PH(psf)], DP_PNUM(psf), istar, DP_PSFRAD(dao),
		    DP_FITRAD(dao), datamax, resid)

		# Compute the proper weight for the star.
		if (IS_INDEFR(Memr[DP_PH(psf)+istar-1]) ||
		    Memr[DP_PH(psf)+istar-1] <= 0.0) {
		    Memr[DP_PWEIGHT(psf)+istar-1] = 0.0
		} else if (Memi[DP_PSAT(psf)+istar-1] == YES) {
		    Memr[DP_PWEIGHT(psf)+istar-1] = 0.5 *
		        Memr[DP_PH(psf)+istar-1] / Memr[DP_PH(psf)]
		} else {
		    Memr[DP_PWEIGHT(psf)+istar-1] = (Memr[DP_PH(psf)+
			istar-1] / Memr[DP_PH(psf)]) * dp_sweight (resid,
			sumfree, DP_PSIGANA(psf))
		}

	        # Compute the expansion vector.
	        call dp_eaccum (Memr[DP_PXCEN(psf)+istar-1],
		    Memr[DP_PYCEN(psf)+istar-1], DP_PSFX(psffit),
		    DP_PSFY(psffit), Memr[DP_PTMP(psf)], DP_NVLTABLE(psffit),
		    DP_NFEXTABLE(psffit))

	        # Compute the contribution to the lookup table of the
		# particular star.
	        call dp_ltinterp (Memr[data], (mx - lx + 1), (my - ly + 1),
	            lx, ly, Memr[DP_PXCEN(psf)+istar-1], Memr[DP_PYCEN(psf)+
		    istar-1], Memr[DP_APMSKY(apsel)+istar-1],
		    Memr[DP_PH(psf)+istar-1] / Memr[DP_PH(psf)],
		    Memr[DP_PWEIGHT(psf)+istar-1], Memr[DP_PSUMN(psf)],
		    Memr[DP_PSUMW(psf)], Memr[DP_PSUMSQ(psf)],
		    Memr[DP_PSIGMA(psf)], Memr[DP_POLDLUT(psf)],
		    Memr[DP_PTMP(psf)], Memr[DP_PSFLUT(psffit)], nexp,
		    DP_PSFSIZE(psffit), datamax, iter, DP_NCLEAN(dao) + 1)

	        call mfree (data, TY_REAL)
	    }

	    call imflush (wim)

	    # Compute the lookup table.
	    if (dp_ltcompute (Memr[DP_PXCEN(psf)], Memr[DP_PYCEN(psf)],
	        DP_PNUM(psf), DP_PSFX(psffit), DP_PSFY(psffit),
		Memr[DP_PSUMN(psf)], wim, Memr[DP_PSFLUT(psffit)],
	        Memr[DP_PC(psf)], Memr[DP_PTMP(psf)], Memr[DP_PV(psf)],
		nexp, DP_PSFSIZE(psffit), DP_NVLTABLE(psffit),
		DP_NFEXTABLE(psffit)) == ERR) {
		call sfree (sp)
		return (ERR)
	    }


	    # Compute the standard deviation arrays for the next pass.
	    if (iter < (DP_NCLEAN(dao) + 1)) {
	        if (nunsat <= nexp)
		    break
		call amovr (Memr[DP_PSFLUT(psffit)], Memr[DP_POLDLUT(psf)],
		    nexp * DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit))
		call dp_stdcompute (Memr[DP_PSUMN(psf)], Memr[DP_PSUMSQ(psf)],
		    Memr[DP_PSIGMA(psf)], DP_PSFSIZE(psffit),
		    DP_PSFSIZE(psffit), nexp)
	    }

	}

	if (nexp > 1) {

	    # Accumulate the v vector.
	    call dp_vaccum (Memr[DP_PXCEN(psf)], Memr[DP_PYCEN(psf)], 
	        Memr[DP_PH(psf)], Memr[DP_PWEIGHT(psf)], DP_PNUM(psf),
	        DP_PSFX(psffit), DP_PSFY(psffit), Memr[DP_PTMP(psf)],
		Memr[DP_PV(psf)], nexp, DP_NVLTABLE(psffit),
		DP_NFEXTABLE(psffit))

	    # Compute the constant part of the psf.
	    volume = dp_pnorm (Memr[DP_PCONST(psf)], Memr[DP_PSIGMA(psf)],
	        DP_PSFSIZE(psffit))

	    # Normalize lookup tables.
	    call dp_ltnorm (Memr[DP_PCONST(psf)], Memr[DP_PV(psf)],
	        Memr[DP_PSFLUT(psffit)], Memr[DP_PSIGMA(psf)], nexp,
		DP_PSFSIZE(psffit), volume)

	}

	# Make a copy of the psf.
	call dp_pcopy (Memr[DP_PSFLUT(psffit)], Memr[DP_POLDLUT(psf)],
	    DP_PSFSIZE(psffit), DP_PSFSIZE(psffit), nexp)

	# Include the saturated psf stars in the fit.
	if (fit_saturated == YES) {
	    nfit = dp_fsaturated (dao, im, Memi[DP_APID(apsel)],
	        Memr[DP_PXCEN(psf)], Memr[DP_PYCEN(psf)], Memr[DP_PH(psf)],
		Memr[DP_APMSKY(apsel)], Memi[DP_PSAT(psf)], DP_PNUM(psf))
	    fit_saturated = NO
	    if (nfit > 0) {
		nunsat = nunsat + nfit
		if (nexp > 1)
		    call aaddr (Memr[DP_PCONST(psf)], Memr[DP_POLDLUT(psf)],
		        Memr[DP_PCONST(psf)], DP_PSFSIZE(psffit) *
		        DP_PSFSIZE(psffit))
	        goto fitsaturated_
	    }
	}

	# Cleanup the temporary images and arrays.
	call imunmap (wim)
	call imdelete (Memc[wimname])
	call sfree (sp)

	return (OK)
end


# DP_DCLEAN -- Clean bad pixels that are outside the fitting radius from
# the data. Note that the star must not be considered to be saturated to
# arrive at this point.

int procedure dp_dclean (data, nx, ny, lx, ly, x, y, fitrad, datamin, datamax)

real	data[nx,ARB]		# the input image data
int	nx, ny			# the dimensions of the input image data
int	lx, ly			# the coordinates of the ll image corner
real	x, y			# the input/output stellar coordinates
real	fitrad			# the fitting radius
real	datamin			# the min good data value
real	datamax			# the max good data value

bool	redo
int	i, j, l, k, nclean
real	frad2, dy2, dr2, sumd, sumn

begin
	nclean = 0
	repeat {

	    redo = false
	    frad2 = fitrad ** 2

	    do j = 1, ny {

	        dy2 = (real ((ly - 1) + j) - y) ** 2
	        if (dy2 < frad2)
		    next
	        do i = 1, nx {

		    if (data[i,j] >= datamin && data[i,j] <= datamax)
		        next
	            dr2 = dy2 + (real ((lx - 1) + i) - x) ** 2
	            if (dr2 < frad2)
		        next

		    sumd = 0.0
		    sumn = 0.0
		    do l = max (1, j-1), min (ny, j+2) {
		        do k = max (1, i-1), min (nx, i+2) {
			    if (data[k,l] < datamin || data[k,l] > datamax)
			        next
			    sumd = sumd + data[k,l]
			    sumn = sumn + 1.0
		        }
		    }
		    if (sumn < 2.5)
		        redo = true
		    else {
			nclean = nclean + 1
		        data[i,j] = sumd / sumn 
		    }
	        }
	    }

	} until (! redo)

	return (nclean)
end


# DP_RESANA -- Compute the residuals from the analytic function.

int procedure dp_resana (im, psftype, params, data, nx, ny, lx, ly,
	x, y, sky, h, nstar, psfstar, psfrad, fitrad, maxgdata, resid)

pointer	im			# the input image descriptor
int	psftype			# analytic point spread function type
real	params[ARB]		# current function parameter values
real	data[nx,ARB]		# the input image data
int	nx, ny			# the dimensions of the input image data
int	lx, ly			# the coordinates of the ll image corner
real	x[ARB]			# the input x coords of the psf stars
real	y[ARB]			# the input y coords of the psf stars
real	sky[ARB]		# the input sky values of the psf stars
real	h[ARB]			# the input height values of the psf stars
int	nstar			# the number of psf stars
int	psfstar			# the psf star in question
real	psfrad			# the psf radius
real	fitrad			# the fitting radius
real	maxgdata		# the maximum good data value
real	resid			# standard deviation of fit

int	i, j, istar, rx, ry, x1, x2, y1, y2, nresid
real	frad2, dx, dy, dy2, dr2, p, dhdxc, dhdyc, junk
int	dp_lsubrast()
real	dp_profile()

begin
	frad2 = fitrad ** 2
	rx = lx + nx - 1
	ry = ly + ny - 1

	resid = 0.0
	nresid = 0
	do istar = 1, nstar {

	    # Check for saturation.
	    if (IS_INDEFR(h[istar]))
		next

	    # Does the subraster of another PSF star overlap the current
	    # subraster ?.
	    if (dp_lsubrast (im, x[istar], y[istar], psfrad, x1, x2,
	        y1, y2) == ERR)
		next
	    if (x2 < lx || y2 < ly || x1 > rx || y1 > ry)
		next

	    # Check the limits of overlap.
	    if (x1 < lx)
		x1 = lx
	    if (x2 > rx)
		x2 = rx
	    if (y1 < ly)
		y1 = ly
	    if (y2 > ry)
		y2 = ry

	    # Subract off the analytic part of the fits and accumulate
	    # the residuals for the psf star.
	    do j = y1 - ly + 1, y2 - ly + 1 {
	        dy = real ((ly - 1) + j) - y[istar]
	        dy2 = dy ** 2
	        do i = x1 - lx + 1, x2 - lx + 1 {
		    if (data[i,j] > maxgdata)
			next
	            dx = real ((lx - 1) + i) - x[istar]
		    p = dp_profile (psftype, dx, dy, params, dhdxc, dhdyc,
		        junk, 0)
		    data[i,j] = data[i,j] - h[istar] * p
		    if (istar != psfstar)
		        next
		    dr2 = dy2 + dx ** 2
		    if (dr2 >= frad2)
		        next
		    resid = resid + (data[i,j] - sky[istar]) ** 2
		    nresid = nresid + 1
	        }
	    }
	}

	if (nresid <= 0)
	    resid = 0.0
	else
	    resid = sqrt (resid / nresid) / (h[psfstar] * dp_profile (psftype,
	        0.0, 0.0, params, dhdxc, dhdyc, junk, 0))

	return (nresid)
end


# DP_SWEIGHT -- Compute the weight for the star.

real procedure dp_sweight (resid, sumfree, sumana)

real	resid		# normalized residual wrt analytic fit
real	sumfree		# number of degrees of freedom
real	sumana		# number of points contributing to analytic fit

real	weight

begin
	weight = resid * sumfree / sumana
	weight = 1.0 / (1.0 + (weight / 2.0) ** 2)
	return (weight)
end


# DP_EACCUM -- Calcuate the expansion vector for a single PSF star.

procedure dp_eaccum (x, y, xmid, ymid, junk, nvexp, nfexp)

real	x, y			# the stellar coordinates
real	xmid, ymid		# the psf coordinates
real	junk[ARB]		# temporary storage vector
int	nvexp			# the number of variable psf look-up tables
int	nfexp			# the number of pixel expansion tables

int	j

begin
	# The variable psf terms.
	switch (nvexp) {
	case 1:
	    junk[1] = 1.0
	case 3:
	    junk[1] = 1.0
	    junk[2] = ((x - 1.0) / xmid) - 1.0
	    junk[3] = ((y - 1.0) / ymid) - 1.0
	case 6:
	    junk[1] = 1.0
	    junk[2] = ((x - 1.0) / xmid) - 1.0
	    junk[3] = ((y - 1.0) / ymid) - 1.0
	    junk[4] = (1.5 * (junk[2] ** 2)) - 0.5
	    junk[5] = junk[2] * junk[3]
	    junk[6] = (1.5 * (junk[3] ** 2)) - 0.5
	}

	# The fractional pixel expansion terms if any.
	if (nfexp > 0) {
	    j = nvexp + 1
	    junk[j] = 2.0 * (x - real (nint(x)))
	    j = j + 1
	    junk[j] = 2.0 * (y - real (nint(y)))
	    j = j + 1
	    junk[j] = (1.5 * (junk[j-2] ** 2)) - 0.5
	    j = j + 1
	    junk[j] = junk[j-3] * junk[j-2]
	    j = j + 1
	    junk[j] = (1.5 * (junk[j-3] ** 2)) - 0.5
	}

end


# DP_LTINTERP -- Compute the contribution to the lookup table of a single
# PSF star.

procedure dp_ltinterp (data, nx, ny, lx, ly, x, y, sky, hratio, weight, sumn,
	sumw, sumsq, sig, old, temp, psflut, nexp, nxlut, maxgdata, iter, niter)

real	data[nx,ARB]		# the input image data
int	nx, ny			# the dimensions of the input image data
int	lx, ly			# the coordinates of the ll image corner
real	x, y			# the input/output stellar coordinates
real	sky			# sky value for star
real	hratio			# scale factor for star
real	weight			# weight for the star
real	sumn[nxlut,ARB]		# number of points
real	sumw[nxlut,ARB]		# sum of the weights
real	sumsq[nxlut,ARB]	# sum of the residuals
real	sig[nxlut,ARB]		# residuals of previous iteration
real	old[nexp,nxlut,ARB]	# old lookup table
real	temp[ARB]		# the single star expansion vector
real	psflut[nexp,nxlut,ARB]	# the psf lookup table
int	nexp, nxlut		# the dimensions of the lookup table
real	maxgdata		# maximum good data value
int	iter			# the current iteration
int	niter			# the maximum number of iterations

bool	omit
int	i, j, k, kx, ky, ii, jj, middle, jysq, irsq, midsq
real	jy, ix, dx, dy, diff, dfdx, dfdy, wt, oldsum
real	bicubic()

begin
	middle = (nxlut + 1) / 2
	midsq = middle ** 2
	do j = 1, nxlut {
	    jysq = (j - middle) ** 2
	    if (jysq > midsq)
		next
	    jy = y + real (j - (nxlut + 1) / 2) / 2.0 - real (ly - 1)
	    ky = int (jy)
	    if (ky < 2 || (ky + 2) > ny) 
		next
	    dy = jy - real (ky)
	    do i = 1, nxlut {
	        irsq = jysq + (i - middle) ** 2
		if (irsq > midsq)
		    next
		ix = x + real (i - (nxlut + 1) / 2) / 2.0 - real (lx - 1)
		kx = int (ix)
	        if (kx < 2 || (kx + 2) > nx) 
		    next
		omit = false
		do jj = ky - 1, ky + 2 {
		    do ii = kx - 1, kx + 2 {
			if (data[ii,jj] <= maxgdata)
			    next
			omit = true
		    }
		}
		if (omit)
		    next
		dx = ix - real (kx)
		diff = (bicubic (data[kx-1,ky-1], nx, dx, dy, dfdx, dfdy) -
		    sky) / hratio
		if (iter == 1 || sig[i,j] <= 0.0) {
		    wt = 1.0
		} else {
		    oldsum = 0.0
		    do k = 1, nexp
		        oldsum = oldsum + old[k,i,j]  * temp[k]
		    if ((iter - 1) <= max (3, (niter  - 1) / 2))
		        wt = 1.0 / (1.0 + abs (diff - oldsum) / sig[i,j])
		    else
		        wt = 1.0 / (1.0 + ((diff - oldsum) / sig[i,j]) ** 2)
		}
		wt = wt * weight
		sumn[i,j] = sumn[i,j] + 1.0
		sumw[i,j] = wt
		sumsq[i,j] = sumsq[i,j] + abs (diff)
		psflut[1,i,j] = psflut[1,i,j] + wt * diff
		if (nexp <= 1)
		    next
		do k = 2, nexp
		    psflut[k,i,j] = psflut[k,i,j] + (temp[k] * wt * diff)
	    }
	}
end


# DP_LTCOMPUTE -- Compute the lookup table.

int procedure dp_ltcompute (x, y, npsf, xmid, ymid, sumn, wim, psflut, c,
        junk, v, nexp, nxlut, nvexp, nfexp)

real	x[ARB]			# array of psf star x coordinates
real	y[ARB]			# array of psf star y coordinates
int	npsf			# the number of psf stars
real	xmid, ymid		# the mid-point of the psf
real	sumn[nxlut,ARB]		# number of points
pointer	wim			# pointer to the temporary weight image
real	psflut[nexp,nxlut,ARB]	# the psf lookup table
real	c[nexp,ARB]		# the expansion matrix
real	junk[ARB]		# temporary junk vector
real	v[ARB]			# temporary vector
int	nexp, nxlut		# the size of the lookup table
int	nvexp, nfexp		# size of the expansion look-up tables

int	i, j, k, l, line, istar, ier, middle, midsq, jysq, irsq
pointer	sumw
real	weight
pointer	imgs2r()

begin
	middle = (nxlut + 1) / 2
	midsq = middle ** 2 
	do j = 1, nxlut {
	    jysq = (j - middle) ** 2
	    if (jysq > midsq)
		next
	    do i = 1, nxlut {
		irsq = (i - middle) ** 2 + jysq 
		if (irsq > midsq)
		    next
		if (nint (sumn[i,j]) < nexp)
		    return (ERR)
		line = i + (j - 1) * nxlut
		sumw = imgs2r (wim, 1, npsf, line, line)
		do k = 1, nexp
		    do l = 1, nexp
			c[k,l] = 0.0
		do istar = 1, npsf {
		    weight = Memr[sumw+istar-1]
		    if (weight <= 0.0)
			next
		    call dp_eaccum (x[istar], y[istar], xmid, ymid, junk,
		        nvexp, nfexp)
		    do k = 1, nexp
		        do l = 1, nexp
			    c[k,l] = c[k,l] + weight * junk[k] * junk[l]
		}
		call invers (c, nexp, nexp, ier)
		call mvmul (c, nexp, nexp, psflut[1,i,j], v)
		do k = 1, nexp
		    psflut[k,i,j] = v[k]
	    }
	}

	return (OK)
end


# DP_STDCOMPUTE -- Estimate the standard deviation of the fit.

procedure dp_stdcompute (sumn, sumsq, sigma, nx, ny, nexp)

real	sumn[nx,ARB]		# number of point
real	sumsq[nx,ARB]		# sum of the standard deviations
real	sigma[nx,ARB]		# output standard deviation array
int	nx, ny			# size of the arrays
int	nexp			# number of expansion vectors

int	i, j

begin
	do j = 1, ny {
	    do i = 1, nx {
		if (sumn[i,j] <= nexp)
		    sigma[i,j] = 0.0
		else
		    sigma[i,j] = 1.2533 * sumsq[i,j] / sqrt (sumn[i,j] *
		        (sumn[i,j] - nexp))
	    }
	}
end


# DP_VACCUM -- Accumulate the expansion vector.

procedure dp_vaccum (x, y, h, weight, nstar, xmid, ymid, junk, avetrm,
	nexp, nvexp, nfexp)

real	x[ARB]			# the stellar x coordinates
real	y[ARB]			# the stellar y coordinates
real	h[ARB]			# the stellar heights
real	weight[ARB]		# the stellar weights
int	nstar			# number of stars
real	xmid, ymid		# the psf coordinates
real	junk[ARB]		# temporary storage vector
real	avetrm[ARB]		# the expansion vector
int	nexp			# the total number of expansion terms
int	nvexp			# number of variable expansion terms
int	nfexp			# number of fractional pixel expansion terms

int	k, j

begin
	# Zero the accumulation vector
	do j = 1, nexp
	    avetrm[j] = 0.0

	do k = 1, nstar {
	    if (IS_INDEFR(h[k]))
		next

	    # The variable psf expansion terms.
	    switch (nvexp) {
	    case 1:
	        junk[1] = 1.0
	    case 3:
	        junk[1] = 1.0
	        junk[2] = ((x[k] - 1.0) / xmid) - 1.0
	        junk[3] = ((y[k] - 1.0) / ymid) - 1.0
	    case 6:
	        junk[1] = 1.0
	        junk[2] = ((x[k] - 1.0) / xmid) - 1.0
	        junk[3] = ((y[k] - 1.0) / ymid) - 1.0
	        junk[4] = (1.5 * (junk[2] ** 2)) - 0.5
	        junk[5] = junk[2] * junk[3]
	        junk[6] = (1.5 * (junk[3] ** 2)) - 0.5
	    }

	    # The fractional pixel expansion terms if any.
	    if (nfexp > 0) {
	        j = nvexp + 1
	        junk[j] = 2.0 * (x[k] - real (nint(x[k])))
	        j = j + 1
	        junk[j] = 2.0 * (y[k] - real (nint(y[k])))
	        j = j + 1
	        junk[j] = (1.5 * (junk[j-2] ** 2)) - 0.5
	        j = j + 1
	        junk[j] = junk[j-3] * junk[j-2]
	        j = j + 1
	        junk[j] = (1.5 * (junk[j-3] ** 2)) - 0.5
	    }

	    # Accumulate the expansion vector.
	    do j = 1, nexp
	        avetrm[j] = avetrm[j] + weight[k] * junk[j]
	}

	# Average the expansion vector.
	do j = nexp, 1, -1
	    avetrm[j] = avetrm[j] / avetrm[1]
end


# DP_PCONST -- Compute the analytic part of the psf.

procedure dp_pconst (psftype, params, hpsf1, ana, nxlut)

int	psftype			# the type of analytic psf function
real	params[ARB]		# the analytic parameters array
real	hpsf1			# the height of the psf function
real	ana[nxlut,ARB]		# the computed analytic function
int	nxlut			# the size of the constant lookup table

int	i, j, middle, midsq, dj2, dr2
real	dx, dy, dfdx, dfdy, junk
real	dp_profile()

begin
	# Compute the constant part of the psf.
	middle = (nxlut + 1) / 2
	midsq = middle ** 2
	do j = 1, nxlut {
	    dj2 = (j - middle) ** 2
	    if (dj2 > midsq)
		next
	    dy = real (j - middle) / 2.0
	    do i = 1, nxlut {
		dr2 = (i - middle) ** 2 + dj2
		if (dr2 > midsq)
		    next
	        dx = real (i - middle) / 2.0
		ana[i,j] = hpsf1 * dp_profile (psftype, dx, dy, params,
		    dfdx, dfdy, junk, 0)
	    }
	}
end


# DP_PNORM -- Compute the psf normalization parameters.

double procedure dp_pnorm (ana, pixels, nxlut)

real	ana[nxlut,ARB]		# the computed analytic function
real	pixels[ARB]		# pixel storage array
int	nxlut			# the size of the constant lookup table

int	i, j, middle, midsq, edgesq, npts, dj2, dr2
double	vol
real	median
real	pctile()

begin
	# Ensure that the profile which will be added and subtracted
	# from the various lookup tables has a median value of zero 
	# around the rim

	middle = (nxlut + 1) / 2
	midsq = middle ** 2
	edgesq = (middle - 2) ** 2
	npts = 0
	do j = 1, nxlut {
	    dj2 = (j - middle) ** 2
	    if (dj2 > midsq)
		next
	    do i = 1, nxlut {
		dr2 = (i - middle) ** 2 + dj2
	        if (dr2 > midsq)
		    next
		if (dr2 < edgesq)
		    next
		npts = npts + 1
		pixels[npts] = ana[i,j]
	    }
	}
	median = pctile (pixels, npts, (npts+1) / 2)

	vol = 0.0d0
	do j = 1, nxlut {
	    dj2 = (j - middle) ** 2
	    if (dj2 > midsq)
		next
	    do i = 1, nxlut {
		dr2 = (i - middle) ** 2 + dj2
	        if (dr2 > midsq)
		    next
		ana[i,j] = ana[i,j] - median
		vol = vol + double (ana[i,j])
	    }
	}

	return (vol)
end


# DP_LTNORM -- Compute the final lookup table.

procedure dp_ltnorm (ana, v, psflut, pixels, nexp, nxlut, volume)

real	ana[nxlut,ARB]		# analytic part of the profile
real	v[ARB]			# the expansion vector
real	psflut[nexp,nxlut,ARB]	# the psf lookup table
real	pixels[ARB]		# scratch array for determining median
int	nexp, nxlut		# the size of the lookup table
double	volume			# total flux in the constant psf

int	i, j, k, middle, midsq, edgesq, npts, dj2, dr2
double	sum
real	median, dmedian
real	pctile()

begin
	# Ensure that the psf which will be added and subtracted from the
	# various lookup tables has a median value of zero around the rim.

	middle = (nxlut + 1) / 2
	midsq = middle ** 2
	edgesq = (middle - 2) ** 2
	do k = 2, nexp {

	    npts = 0
	    do j = 1, nxlut {
	        dj2 = (j - middle) ** 2
		if (dj2 > midsq)
		    next
	        do i = 1, nxlut {
		    dr2 = (i - middle) ** 2 + dj2
		    if (dr2 > midsq)
			next
		    if (dr2 < edgesq)
		        next
		    npts = npts + 1
		    pixels[npts] = psflut[k,i,j]
	        }
	    }

	    median = pctile (pixels, npts, (npts+1) / 2)
	    dmedian = v[k] * median

	    do j = 1, nxlut {
	        dj2 = (j - middle) ** 2
		if (dj2 > midsq)
		    next
		do i = 1, nxlut {
		    dr2 = (i - middle) ** 2 + dj2
		    if (dr2 > midsq)
			next
		    psflut[k,i,j] = psflut[k,i,j] - median
		    psflut[1,i,j] = psflut[1,i,j] + dmedian
		}
	    }
	}

	# Determine the net volume of each of the higher order PSF 
	# tables and force it to zero by subtracting a scaled copy
	# of the constant psf. Scale the part that has been subtracted
	# off by the mean polynomial term and add it in to the constant
	# part of the psf so that at the centroid of the psf stars
	# positions the psf remains unchanged.

	do k = 2, nexp {

	    sum = 0.0d0
	    do j = 1, nxlut {
	        dj2 = (j - middle) ** 2
		if (dj2 > midsq)
		    next
		do i = 1, nxlut {
		    dr2 = (i - middle) ** 2 + dj2
		    if (dr2 > midsq)
			next
		    sum = sum + double (psflut[k,i,j])
		}
	    }

	    median = real (sum / volume)
	    dmedian = v[k] * median

	    do j = 1, nxlut {
	        dj2 = (j - middle) ** 2
		if (dj2 > midsq)
		    next
		do i = 1, nxlut {
		    dr2 = (i - middle) ** 2 + dj2
		    if (dr2 > midsq)
			next
		    psflut[k,i,j] = psflut[k,i,j] - median * ana[i,j]
		    psflut[1,i,j] = psflut[1,i,j] + dmedian * ana[i,j]
		}
	    }
	}
end


# DP_FSATURATED -- Fit the saturated stars.

int procedure dp_fsaturated (dao, im, id, xcen, ycen, h, sky, sat, nstars)

pointer	dao			# pointer to the main daophot structure
pointer	im			# pointer to the input image
int	id[ARB]			# array of stellar ids
real	xcen[ARB]		# array of stellar y coordinates
real	ycen[ARB]		# array of stellar y coordinates
real	h[ARB]			# array of stellar amplitudes
real	sky[ARB]		# array of sky values
int	sat[ARB]		# array of saturation indicators
int	nstars			# number of stars

int	nfit, nsat, istar, lowx, lowy, nxpix, nypix, recenter, fitsky
int	clipexp, ier, niter
pointer	psf, psffit, subim, psflut
real	x, y, dx, dy, skyval, scale, errmag, chi, sharp, cliprange
int	dp_pkfit()
pointer dp_gsubrast()

begin
	# Get some pointers.
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Allocate memory.
	call dp_pksetup (dao)
	call dp_mempk (dao, 3)

	# Save the default values of some critical parameters.
	recenter = DP_RECENTER(dao)
	fitsky = DP_FITSKY(dao)
	psflut = DP_PSFLUT(psffit)
	clipexp = DP_CLIPEXP(dao)
	cliprange = DP_CLIPRANGE(dao)

	# Set some fitting parameters.
	DP_RECENTER(dao) = YES
	DP_FITSKY(dao) = NO
	DP_CLIPEXP(dao) = 8
	DP_CLIPRANGE(dao) = 2.5
	DP_PSFLUT(psffit) = DP_POLDLUT(psf)

	nfit = 0
	nsat = 0
	do istar = 1, nstars {

	    # Skip saturated stars.
	    if (sat[istar] == NO)
		next
	    nsat = nsat + 1

	    # Get the data.
	    subim = dp_gsubrast (im, xcen[istar], ycen[istar], DP_PSFRAD(dao),
	        lowx, lowy, nxpix, nypix)
	    if (subim == NULL)
		next

	    # Set the intial values for the fit parameters.
	    x = xcen[istar] - lowx + 1.0
	    y = ycen[istar] - lowy + 1.0
	    dx = (xcen[istar] - 1.0) / DP_PSFX(psffit) - 1.0
	    dy = (ycen[istar] - 1.0) / DP_PSFY(psffit) - 1.0
	    skyval = sky[istar]
	    scale = 3.0 

	    # Fit the star.
	    ier = dp_pkfit (dao, Memr[subim], nxpix, nypix, 0.5 *
	        DP_PSFRAD(dao), x, y, dx, dy, scale, skyval, errmag, chi,
		sharp, niter)

	    # Compute the fit parameters.
	    if (ier != PKERR_OK) {
		scale = INDEFR
		errmag = INDEFR
		niter = 0
		chi = INDEFR
		sharp = INDEFR
	    } else {
		nfit = nfit + 1
	        xcen[istar] = x + lowx - 1.0
	        ycen[istar] = y + lowy - 1.0
	        h[istar] = scale * h[1]
		errmag = 1.085736 * errmag / scale
		if (errmag >= 2.0)
		    errmag = INDEFR
		scale = DP_PSFMAG(psffit) - 2.5 * log10 (scale)
	    }

	    if (DP_VERBOSE(dao) == YES) {
		if (nsat == 1)
	            call printf ("\nFit for saturated stars\n")
		call printf (
		    "    %6d  %7.2f  %7.2f  %8.3f  %6.3f  %3d  %7.2f  %7.2f\n")
		    call pargi (id[istar])
		    call pargr (xcen[istar])
		    call pargr (ycen[istar])
		    call pargr (scale)
		    call pargr (errmag)
		    call pargi (niter)
		    call pargr (chi)
		    call pargr (sharp)
	    }
	}

	if (DP_VERBOSE(dao) == YES && nsat > 0)
	    call printf ("\n")

	# Restore the default values of some critical parameters.
	DP_RECENTER(dao) = recenter
	DP_FITSKY(dao) = fitsky
	DP_CLIPEXP(dao) = clipexp
	DP_CLIPRANGE(dao) = cliprange
	DP_PSFLUT(psffit) = psflut

	# Free memory.
	call dp_pkclose (dao)

	return (nfit)
end


# DP_PCOPY -- Make a copy of the psf in correct storage order.

procedure dp_pcopy (inlut, outlut, nx, ny, nexp)

real	inlut[nexp,nx,ny]		# the input look-up table
real	outlut[nx,ny,nexp]		# the output look-up table
int	nx,ny,nexp			# the size of the look-up table

int	k,i,j

begin
	do k = 1, nexp {
	    do j = 1, ny {
		do i = 1, nx {
		    outlut[i,j,k] = inlut[k,i,j]
		}
	    }
	}
end
