# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>
include	"icfit.h"
include	"names.h"

# IC_VSHOW -- Show fit parameters in verbose mode.

procedure ic_vshowd (ic, file, cv, x, y, wts, npts, gt)

pointer	ic		# ICFIT pointer
char	file[ARB]	# Output file
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points
pointer	gt		# Graphics tools pointer

int	i, n, deleted, ncoeffs, fd
double	chisqr, rms
pointer	sp, fit, wts1, coeffs, errors

int	dcvstati(), open()
double	ic_rmsd()
errchk	open()

begin
	# Do the standard ic_show option, then add on the verbose part.
	call ic_show (ic, file, gt)

	if (npts == 0) {
	    call eprintf ("Incomplete output - no data points for fit\n")
	    return
	}

	# Open the output file.
	fd = open (file, APPEND, TEXT_FILE)

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

	call fprintf (fd, "total points = %d\nsample points = %d\n")
	    call pargi (npts)
	    call pargi (n)
	call fprintf (fd, "nrejected = %d\ndeleted = %d\n")
	    call pargi (IC_NREJECT(ic))
	    call pargi (deleted)
	call fprintf (fd, "RMS = %10.7g\n")
	    call pargd (rms)
	call fprintf (fd, "square root of reduced chi square = %10.7g\n")
	    call pargd (sqrt (chisqr))

	call fprintf (fd, "\t  coefficent\t  error\n")
	do i = 1, ncoeffs {
	    call fprintf (fd, "\t%14.7e\t%14.7e\n")
		call pargd (Memd[coeffs+i-1])
	 	call pargd (Memd[errors+i-1])
	}

	# Free allocated memory.

	call sfree (sp)
	call close (fd)
end


# IC_XYSHOW -- List data as x,fit,y triplets on output.

procedure ic_xyshowd (file, cv, xvals, yvals, nvalues)

char	file[ARB]		# Output file
pointer	cv			# Pointer to curfit structure
int	nvalues			# Number of data values
double	xvals[nvalues]		# Array of x data values
double	yvals[nvalues]		# Array of y data values

int	i, fd, open()
double	dcveval()
errchk	open

begin
	# Open the output file.
	fd = open (file, APPEND, TEXT_FILE)

	call fprintf (fd, "\n\t         X     \t   Yc   \t    Y    \n")

	do i = 1, nvalues {
	    call fprintf (fd, "\t%14.7e \t%14.7e \t%14.7e\n")
		call pargd (xvals[i])
		call pargd (dcveval (cv, xvals[i]))
		call pargd (yvals[i])
	}

	call close (fd)
end
