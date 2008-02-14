include <math/nlfit.h>
include	<pkg/inlfit.h>

# IN_ERRORS -- Compute the reduced chi-square of the fit and the
# parameter errors. This procedure must be used instead of nlerrors()
# because the weigths are changed during the data rejection process.
# If no data rejection is used, then both procedures are equivalent.

procedure in_errorsr (in, nl, x, y, wts, npts, nvars, variance, chisqr,
	scatter, rms, errors)

pointer	in			# INLFIT pointer
pointer	nl			# NLFIT pointer
real	x[ARB]			# Ordinates (npts * nvars)
real	y[npts]			# Data to be fit
real	wts[npts]		# Weights
int	npts			# Number of points
int	nvars			# Number of variables
real	variance		# variance of the fit (output)
real	chisqr			# reduced chi-squared of fit (output)
real	scatter			# additional scatter in equation
real	rms			# RMS of the fit (output)
real	errors[ARB]		# errors in coefficients (output)

int	i
real	in_rmsr(), nlstatr
pointer	sp, fit, wts1, rejpts

int	in_geti()
pointer	in_getp()

begin
#	# Debug.
#	call eprintf ("in_errors: in=%d, nl=%d, npts=%d, nvars=%d\n")
#	    call pargi (in)
#	    call pargi (nl)
#	    call pargi (npts)
#	    call pargi (nvars)

	# Allocate memory for fit and weights.
	call smark (sp)
	call salloc (fit, npts, TY_REAL)
	call salloc (wts1, npts, TY_REAL)

	# Set zero weight for rejeceted points.
	call amovr (wts, Memr[wts1], npts)
	if (in_geti (in, INLNREJPTS) > 0) {
	    rejpts = in_getp (in, INLREJPTS)
	    do i = 1, npts {
		if (Memi[rejpts+i-1] == YES)
		    Memr[wts1+i-1] = real (0.0)
	    }
	}

	# Evaluate the fit, and compute the rms, reduced chi
	# squared and errors.

	call nlvectorr (nl, x, Memr[fit], npts, nvars)
	call nlerrorsr (nl, y, Memr[fit], Memr[wts1], npts,
	    variance, chisqr, errors)
	rms = in_rmsr (y, Memr[fit], Memr[wts1], npts)
	scatter = nlstatr (nl, NLSCATTER)

	# Free memory.
	call sfree (sp)
end
