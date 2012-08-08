include	<pkg/gtools.h>
include	"hdicfit.h"

# ICG_PARAMS -- Set parameter string.

procedure icg_paramsd (ic, cv, x, y, wts, npts, gt)

pointer	ic		# ICFIT pointer
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points
pointer	gt		# GTOOLS pointer

double	rms
int	i, n, deleted
pointer	sp, fit, wts1, str, params
double	ic_rmsd()

begin
	call smark (sp)

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

	    # Set the fit and compute the RMS error.
	    call dcvvector (cv, x, Memd[fit], n)
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

	    # Set the fit and compute the rms error.
	    call dcvvector (cv, Memd[IC_XFIT(ic)], Memd[fit], n)
	    rms = ic_rmsd (Memd[IC_XFIT(ic)], Memd[IC_YFIT(ic)], Memd[fit],
		Memd[wts1], n)
	}

	# Print the parameters and errors.
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (params, 2*SZ_LINE, TY_CHAR)

	call sprintf (Memc[str],SZ_LINE, "function=%s, order=%d, transform=%s")
	    call ic_gstr (ic, "function", Memc[params], 2*SZ_LINE)
	    call pargstr (Memc[params])
	    call pargi (IC_ORDER(ic))
	    call ic_gstr (ic, "transform", Memc[params], SZ_LINE)
	    call pargstr (Memc[params])
	call sprintf (Memc[params], 2*SZ_LINE,
            "%s\nfog=%.5f,  total=%d, deleted=%d, RMS=%7.4g")
	    call pargstr (Memc[str])
	    call pargr (IC_FOG(ic))
	    call pargi (npts)
	    call pargi (deleted)
	    call pargd (rms)
	call gt_sets (gt, GTPARAMS, Memc[params])

	call sfree (sp)
end
