# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>
include	"icfit.h"
include	"names.h"

# IC_FERRORS -- Compute error diagnositic information.

procedure ic_ferrorsr (ic, cv, x, y, wts, npts, fd)

pointer	ic		# ICFIT pointer
pointer	cv		# Curfit pointer
real	x[ARB]		# Ordinates
real	y[ARB]		# Abscissas
real	wts[ARB]	# Weights
int	npts		# Number of data points
int	fd		# Output file descriptor

int	i, n, deleted, ncoeffs
real	chisqr, rms
pointer	sp, fit, wts1, coeffs, errors

int	rcvstati()
real	ic_rmsr()

begin
	# Determine the number of coefficients and allocate memory.

	ncoeffs = rcvstati (cv, CVNCOEFF)
	call smark (sp)
	call salloc (coeffs, ncoeffs, TY_REAL)
	call salloc (errors, ncoeffs, TY_REAL)

	if (npts == IC_NFIT(ic)) {
	    # Allocate memory for the fit.

	    n = npts
	    call salloc (fit, n, TY_REAL)
	    call salloc (wts1, n, TY_REAL)

	    # Eliminate rejected points and count deleted points.

	    call amovr (wts, Memr[wts1], n)
	    if (IC_NREJECT(ic) > 0) {
		do i = 1, npts {
		    if (Memi[IC_REJPTS(ic)+i-1] == YES)
			Memr[wts1+i-1] = 0.
		}
	    }
	    deleted = 0
	    do i = 1, n {
		if (wts[i] == 0.)
		    deleted = deleted + 1
	    }

	    # Get the coefficients and compute the errors.

	    call rcvvector (cv, x, Memr[fit], n)
	    call rcvcoeff (cv, Memr[coeffs], ncoeffs)
	    call rcverrors (cv, y, Memr[wts1], Memr[fit], n, chisqr,
		Memr[errors])
	    rms = ic_rmsr (x, y, Memr[fit], Memr[wts1], n)
	} else {
	    # Allocate memory for the fit.

	    n = IC_NFIT(ic)
	    call salloc (fit, n, TY_REAL)
	    call salloc (wts1, n, TY_REAL)

	    # Eliminate rejected points and count deleted points.

	    call amovr (Memr[IC_WTSFIT(ic)], Memr[wts1], n)
	    if (IC_NREJECT(ic) > 0) {
	        do i = 1, npts {
		    if (Memi[IC_REJPTS(ic)+i-1] == YES)
			Memr[wts1+i-1] = 0.
		}
	    }
	    deleted = 0
	    do i = 1, n {
		if (wts[i] == 0.)
		    deleted = deleted + 1
	    }

	    # Get the coefficients and compute the errors.

	    call rcvvector (cv, Memr[IC_XFIT(ic)], Memr[fit], n)
	    rms = ic_rmsr (Memr[IC_XFIT(ic)], Memr[IC_YFIT(ic)],
		Memr[fit], Memr[wts1], n)
	    call rcvcoeff (cv, Memr[coeffs], ncoeffs)
	    call rcverrors (cv, Memr[IC_YFIT(ic)], Memr[wts1], Memr[fit],
		n, chisqr, Memr[errors])
	}

	# Print the error analysis.

	call fprintf (fd, "# total points = %d\nsample points = %d\n")
	    call pargi (npts)
	    call pargi (n)
	call fprintf (fd, "# nrejected = %d\ndeleted = %d\n")
	    call pargi (IC_NREJECT(ic))
	    call pargi (deleted)
	call fprintf (fd, "# RMS = %7.4g\n")
	    call pargr (rms)
	call fprintf (fd, "# square root of reduced chi square = %7.4g\n")
	    call pargr (sqrt (chisqr))

	# Free allocated memory.

	call sfree (sp)
end

# IC_RMS -- Compute RMS of points which have not been deleted.

real procedure ic_rmsr (x, y, fit, wts, npts)

real	x[ARB]		# Ordinates
real	y[ARB]		# Abscissas
real	fit[ARB]	# Fit
real	wts[ARB]	# Weights
int	npts		# Number of data points

int	i, n
real	resid, rms

begin
	rms = 0.
	n = 0
	do i = 1, npts {
	    if (wts[i] == 0.)
		next
	    resid = y[i] - fit[i]
	    rms = rms + resid * resid
	    n = n + 1
	}

	if (n > 0)
	    rms = sqrt (rms / n)

	return (rms)
end
