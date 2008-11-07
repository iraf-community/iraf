include	<math/nlfit.h>
include	<pkg/inlfit.h>

# IN_FIT -- Fit a function using non-linear least squares. The function
# can have an arbitrary number of independent variables. This is the main
# entry point for the non-interactive part of the INLFIT package.

procedure in_fitd (in, nl, x, y, wts, npts, nvars, wtflag, stat)

pointer	in				# INLFIT pointer
pointer	nl				# NLFIT pointer
double	x[ARB]				# Ordinates (npts * nvars)
double	y[npts]				# Data to be fit
double	wts[npts]			# Weights
size_t	npts				# Number of points
int	nvars				# Number of variables
int	wtflag				# Type of weighting
int	stat				# Error code (output)

size_t	sz_val
long	i, ndeleted
pointer	sp, wts1, str
int	in_geti()
long	in_getl()
double	in_getd

begin

#	# Debug.
#	call eprintf ("in_fit: in=%d, nl=%d, npts=%d, nvars=%d\n")
#	    call pargi (in)
#	    call pargi (nl)
#	    call pargi (npts)
#	    call pargi (nvars)

	# Allocate string, and rejection weight space. The latter are
	# are used to mark rejected points with a zero weight before
	# calling NLFIT.

	call smark (sp)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)
	call salloc (wts1, npts, TY_DOUBLE)
	call amovd (wts, Memd[wts1], npts)

	# Initialize rejected point list, and the buffer containing
	# the minimum and maximum variable values.
	call in_bfinitd (in, npts, nvars)

	# Set independent variable limits.
	call in_limitd (in, x, npts, nvars)

	# Reinitialize.
	call in_nlinitd (in, nl)

	# Check number of data points. If no points are present
	# set the error flag to the appropiate value, and return.
	if (npts == 0) {
	    stat = NO_DEG_FREEDOM
	    call in_puti (in, INLFITERROR, NO_DEG_FREEDOM)
	    call sfree (sp)
	    return
	}

	# Check the number of deleted points.
	ndeleted = 0
	do i = 1, npts {
	    if (wts[i] <= double(0.0))
		ndeleted = ndeleted + 1
	}
	if ((npts - ndeleted) < in_getl (in, INLNFPARAMS)) {
	    stat = NO_DEG_FREEDOM
	    call in_puti (in, INLFITERROR, NO_DEG_FREEDOM)
	    call sfree (sp)
	    return
	}

	# Call NLFIT.
	call nlfitd (nl, x, y, wts, npts, nvars, wtflag, stat)

	# Update fit status into the INLFIT structure.
	call in_puti (in, INLFITERROR, stat)

	# Do pixel rejection and refit, if at least one of the rejection
	# limits is positive. Otherwise clear number of rejected points.

	if (in_getd (in, INLLOW)  > double (0.0) ||
	    in_getd (in, INLHIGH) > double (0.0)) {
	    call in_rejectd (in, nl, x, y, Memd[wts1], npts, nvars, wtflag)
	    if (in_getl (in, INLNREJPTS) > 0) {
		do i = 1, npts {
		    if (Memd[wts1+i-1] > double(0.0))
			wts[i] = Memd[wts1+i-1]
		}
	    }
	    stat = in_geti (in, INLFITERROR)
	} else
	    call in_puti (in, INLNREJPTS, 0)

	# Free memory.
	call sfree (sp)
end
