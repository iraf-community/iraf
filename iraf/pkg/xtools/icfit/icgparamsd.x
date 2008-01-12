# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pkg/gtools.h>
include	"icfit.h"
include	"names.h"

# ICG_PARAMS -- Set parameter string.

procedure icg_paramsd (ic, cv, x, y, wts, npts, gt)

pointer	ic		# ICFIT pointer
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points
pointer	gt		# GTOOLS pointer

int	i, n, deleted
double	rms
pointer	sp, fit, wts1, str, params

double	ic_rmsd()

begin
	call smark (sp)

	n = IC_NFIT(ic)
	deleted = 0
	rms = INDEFD

	if (n == npts) {
	    # Allocate memory for the fit.

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

	    if (IC_FITERROR(ic) == NO) {
	        call dcvvector (cv, x, Memd[fit], n)
	        rms = ic_rmsd (x, y, Memd[fit], Memd[wts1], n)
	    } else
		rms = INDEFD
	} else if (n > 0) {
	    # Allocate memory for the fit.

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

	    if (IC_FITERROR(ic) == NO) {
	        call dcvvector (cv, Memd[IC_XFIT(ic)], Memd[fit], n)
	        rms = ic_rmsd (Memd[IC_XFIT(ic)], Memd[IC_YFIT(ic)],
		    Memd[fit], Memd[wts1], n)
	    } else
		rms = INDEFD
	}

	# Print the parameters and errors.

	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (params, 2*SZ_LINE, TY_CHAR)

	call sprintf (Memc[str], SZ_LINE,
	    "func=%s, order=%d, low_rej=%r, high_rej=%r, niterate=%d, grow=%r")
	    call ic_gstr (ic, "function", Memc[params], 2*SZ_LINE)
	    call pargstr (Memc[params])
	    call pargi (IC_ORDER(ic))
	    call pargr (IC_LOW(ic))
	    call pargr (IC_HIGH(ic))
	    call pargi (IC_NITERATE(ic))
	    call pargr (IC_GROW(ic))
	call sprintf (Memc[params], 2*SZ_LINE,
	    "%s\ntotal=%d, sample=%d, rejected=%d, deleted=%d, RMS=%7.4g")
	    call pargstr (Memc[str])
	    call pargi (npts)
	    call pargi (n)
	    call pargi (IC_NREJECT(ic))
	    call pargi (deleted)
	    call pargd (rms)
	call gt_sets (gt, GTPARAMS, Memc[params])

	# Free allocated memory.

	call sfree (sp)
end
