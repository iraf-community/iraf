include <math/nlfit.h>
include	<pkg/inlfit.h>

# IN_ERRORS -- Compute the reduced chi-square of the fit and the
# parameter errors. This procedure must be used instead of nlerrors()
# because the weigths are changed during the data rejection process.
# If no data rejection is used, then both procedures are equivalent.

procedure in_errorsd (in, nl, x, y, wts, npts, nvars, variance, chisqr,
	scatter, rms, errors)

pointer	in			# INLFIT pointer
pointer	nl			# NLFIT pointer
double	x[ARB]			# Ordinates (npts * nvars)
double	y[npts]			# Data to be fit
double	wts[npts]		# Weights
int	npts			# Number of points
int	nvars			# Number of variables
double	variance		# variance of the fit (output)
double	chisqr			# reduced chi-squared of fit (output)
double	scatter			# additional scatter in equation
double	rms			# RMS of the fit (output)
double	errors[ARB]		# errors in coefficients (output)

int	i
double	in_rmsd(), nlstatd
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
	call salloc (fit, npts, TY_DOUBLE)
	call salloc (wts1, npts, TY_DOUBLE)

	# Set zero weight for rejeceted points.
	call amovd (wts, Memd[wts1], npts)
	if (in_geti (in, INLNREJPTS) > 0) {
	    rejpts = in_getp (in, INLREJPTS)
	    do i = 1, npts {
		if (Memi[rejpts+i-1] == YES)
		    Memd[wts1+i-1] = double (0.0)
	    }
	}

	# Evaluate the fit, and compute the rms, reduced chi
	# squared and errors.

	call nlvectord (nl, x, Memd[fit], npts, nvars)
	call nlerrorsd (nl, y, Memd[fit], Memd[wts1], npts,
	    variance, chisqr, errors)
	rms = in_rmsd (y, Memd[fit], Memd[wts1], npts)
	scatter = nlstatd (nl, NLSCATTER)

	# Free memory.
	call sfree (sp)
end
