# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>
include	"icfit.h"
include	"names.h"

# IC_FVSHOW -- Show fit parameters in verbose mode.

procedure ic_fvshowd (ic, cv, x, y, wts, npts, fd)

pointer	ic		# ICFIT pointer
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points
int	fd		# Output descriptor

int	i, n, deleted, ncoeffs
double	chisqr, rms
pointer	sp, fit, wts1, coeffs, errors

int	dcvstati()
double	ic_rmsd()

begin
	# Do the standard ic_show option, then add on the verbose part.
	call ic_fshow (ic, fd)

	if (npts == 0) {
	    call eprintf ("# Incomplete output - no data points for fit\n")
	    return
	}

	# Determine the number of coefficients and allocate memory.

	ncoeffs = dcvstati (cv, CVNCOEFF)
	call smark (sp)
	call salloc (coeffs, ncoeffs, TY_DOUBLE)
	call salloc (errors, ncoeffs, TY_DOUBLE)

	if (npts == IC_NFIT(ic)) {
	    # Allocate memory for the fit.

	    n = npts
	    call salloc (fit, n, TY_DOUBLE)
	    call salloc (wts1, n, TY_DOUBLE)

	    # Eliminate rejected points and count deleted points.

	    call amovd (wts, Memd[wts1], n)
	    if (IC_NREJECT(ic) > 0) {
		do i = 1, npts {
		    if (Memi[IC_REJPTS(ic)+i-1] == YES)
			Memd[wts1+i-1] = 0.
		}
	    }
	    deleted = 0
	    do i = 1, n {
		if (wts[i] == 0.)
		    deleted = deleted + 1
	    }

	    # Get the coefficients and compute the errors.

	    call dcvvector (cv, x, Memd[fit], n)
	    call dcvcoeff (cv, Memd[coeffs], ncoeffs)
	    call dcverrors (cv, y, Memd[wts1], Memd[fit], n, chisqr,
		Memd[errors])
	    rms = ic_rmsd (x, y, Memd[fit], Memd[wts1], n)
	} else {
	    # Allocate memory for the fit.

	    n = IC_NFIT(ic)
	    call salloc (fit, n, TY_DOUBLE)
	    call salloc (wts1, n, TY_DOUBLE)

	    # Eliminate rejected points and count deleted points.

	    call amovd (Memd[IC_WTSFIT(ic)], Memd[wts1], n)
	    if (IC_NREJECT(ic) > 0) {
	        do i = 1, npts {
		    if (Memi[IC_REJPTS(ic)+i-1] == YES)
			Memd[wts1+i-1] = 0.
		}
	    }
	    deleted = 0
	    do i = 1, n {
		if (wts[i] == 0.)
		    deleted = deleted + 1
	    }

	    # Get the coefficients and compute the errors.

	    call dcvvector (cv, Memd[IC_XFIT(ic)], Memd[fit], n)
	    rms = ic_rmsd (Memd[IC_XFIT(ic)], Memd[IC_YFIT(ic)],
		Memd[fit], Memd[wts1], n)
	    call dcvcoeff (cv, Memd[coeffs], ncoeffs)
	    call dcverrors (cv, Memd[IC_YFIT(ic)], Memd[wts1], Memd[fit],
		n, chisqr, Memd[errors])
	}

	# Print the error analysis.

	call fprintf (fd, "# total points = %d\n# sample points = %d\n")
	    call pargi (npts)
	    call pargi (n)
	call fprintf (fd, "# nrejected = %d\n# deleted = %d\n")
	    call pargi (IC_NREJECT(ic))
	    call pargi (deleted)
	call fprintf (fd, "# RMS = %10.7g\n")
	    call pargd (rms)
	call fprintf (fd, "# square root of reduced chi square = %10.7g\n")
	    call pargd (sqrt (chisqr))

	call fprintf (fd, "# \t  coefficent\t  error\n")
	do i = 1, ncoeffs {
	    call fprintf (fd, "# \t%14.7e\t%14.7e\n")
		call pargd (Memd[coeffs+i-1])
	 	call pargd (Memd[errors+i-1])
	}

	# Free allocated memory.

	call sfree (sp)
end


# IC_FXYSHOW -- List data as x, y, fit, weight lines on output.

procedure ic_fxyshowd (ic, cv, x, y, w, npts, fd)

pointer	ic			# ICFIT pointer
pointer	cv			# Pointer to curfit structure
double	x[npts]			# Array of x data values
double	y[npts]			# Array of y data values
double	w[npts]			# Array of weight data values
int	npts			# Number of data values
int	fd			# Output file descriptor

int	i
double	dcveval()

begin
	# List the data being fit (not necessarily the input data).
	call fprintf (fd, "#      X        Y    Y FIT   WEIGHT\n")
	if (npts == IC_NFIT(ic)) {
	    do i = 1, npts {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargd (x[i])
		    call pargd (y[i])
		    call pargd (dcveval (cv, x[i]))
		    call pargd (w[i])
	    }
	} else {
	    do i = 1, IC_NFIT(ic) {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargd (Memd[IC_XFIT(ic)+i-1])
		    call pargd (Memd[IC_YFIT(ic)+i-1])
		    call pargd (dcveval (cv, Memd[IC_XFIT(ic)+i-1]))
		    call pargd (Memd[IC_WTSFIT(ic)+i-1])
	    }
	}
end
