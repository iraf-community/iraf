include	<math/curfit.h>
include	"hdicfit.h"

# IC_ERRORS -- Compute and error diagnositic information.

procedure ic_errorsd (ic, file, cv, x, y, wts, npts)

pointer	ic		# ICFIT pointer
char	file[ARB]	# Output file
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points

int	i, n, deleted, ncoeffs, fd
double	chisqr, rms
pointer	sp, fit, wts1, coeffs, errors

int	dcvstati(), open()
double	ic_rmsd()
errchk	open()

begin
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
	call fprintf (fd, "RMS = %7.4g\n")
	    call pargd (rms)
	call fprintf (fd, "square root of reduced chi square = %7.4g\n")
	    call pargd (sqrt (chisqr))

	#call fprintf (fd, "\tcoefficent\terror\n")
	#do i = 1, ncoeffs {
	#    call fprintf (fd, "\t%10.4e\t%10.4e\n")
	#	call parg$t (Mem$t[coeffs+i-1])
	#	call parg$t (Mem$t[errors+i-1])
	#}

	# Free allocated memory.
	call sfree (sp)
	call close (fd)
end


# IC_RMS -- Compute RMS of points which have not been deleted.

double procedure ic_rmsd (x, y, fit, wts, npts)

double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	fit[ARB]	# Fit
double	wts[ARB]	# Weights
int	npts		# Number of data points

int	i, n
double	resid, rms

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
