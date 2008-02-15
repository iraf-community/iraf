# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>
include	"icfit.h"
include	"names.h"

# IC_FVSHOW -- Show fit parameters in verbose mode.

procedure ic_fvshowr (ic, cv, x, y, wts, npts, fd)

pointer	ic		# ICFIT pointer
pointer	cv		# Curfit pointer
real	x[ARB]		# Ordinates
real	y[ARB]		# Abscissas
real	wts[ARB]	# Weights
int	npts		# Number of data points
int	fd		# Output descriptor

int	i, n, deleted, ncoeffs
real	chisqr, rms
pointer	sp, fit, wts1, coeffs, errors

int	rcvstati()
real	ic_rmsr()

begin
	# Do the standard ic_show option, then add on the verbose part.
	call ic_fshow (ic, fd)

	if (npts == 0) {
	    call eprintf ("# Incomplete output - no data points for fit\n")
	    return
	}

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

	call fprintf (fd, "# total points = %d\n# sample points = %d\n")
	    call pargi (npts)
	    call pargi (n)
	call fprintf (fd, "# nrejected = %d\n# deleted = %d\n")
	    call pargi (IC_NREJECT(ic))
	    call pargi (deleted)
	call fprintf (fd, "# RMS = %10.7g\n")
	    call pargr (rms)
	call fprintf (fd, "# square root of reduced chi square = %10.7g\n")
	    call pargr (sqrt (chisqr))

	call fprintf (fd, "# \t  coefficent\t  error\n")
	do i = 1, ncoeffs {
	    call fprintf (fd, "# \t%14.7e\t%14.7e\n")
		call pargr (Memr[coeffs+i-1])
	 	call pargr (Memr[errors+i-1])
	}

	# Free allocated memory.

	call sfree (sp)
end


# IC_FXYSHOW -- List data as x, y, fit, weight lines on output.

procedure ic_fxyshowr (ic, cv, x, y, w, npts, fd)

pointer	ic			# ICFIT pointer
pointer	cv			# Pointer to curfit structure
real	x[npts]			# Array of x data values
real	y[npts]			# Array of y data values
real	w[npts]			# Array of weight data values
int	npts			# Number of data values
int	fd			# Output file descriptor

int	i
real	rcveval()

begin
	# List the data being fit (not necessarily the input data).
	call fprintf (fd, "#      X        Y    Y FIT   WEIGHT\n")
	if (npts == IC_NFIT(ic)) {
	    do i = 1, npts {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargr (x[i])
		    call pargr (y[i])
		    call pargr (rcveval (cv, x[i]))
		    call pargr (w[i])
	    }
	} else {
	    do i = 1, IC_NFIT(ic) {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargr (Memr[IC_XFIT(ic)+i-1])
		    call pargr (Memr[IC_YFIT(ic)+i-1])
		    call pargr (rcveval (cv, Memr[IC_XFIT(ic)+i-1]))
		    call pargr (Memr[IC_WTSFIT(ic)+i-1])
	    }
	}
end
