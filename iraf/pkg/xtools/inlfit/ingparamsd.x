include	<pkg/gtools.h>
include	<math/nlfit.h>
include	<pkg/inlfit.h>


# ING_PARAMS -- Set parameter string.

procedure ing_paramsd (in, nl, x, y, wts, npts, nvars, gt)

pointer	in		# INLFIT pointer
pointer	nl		# NLFIT pointer
double	x[ARB]		# Ordinates (npts * nvars)
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points
int	nvars		# Number of variables
pointer	gt		# GTOOLS pointer

int	i, rejected, deleted, length
int	len3, len4
double	rms
pointer	sp, fit, wts1, rejpts
pointer	str1, str2, str3, str4, line

int	strlen()
int	nlstati()
int	inlstrwrd()
int	in_geti()
double	nlstatd()
double	in_rmsd()
double	in_getd()
pointer	in_getp()

begin
	# Allocate memory
	call smark (sp)
	call salloc (fit, npts, TY_DOUBLE)
	call salloc (wts1, npts, TY_DOUBLE)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	call salloc (str3, SZ_LINE, TY_CHAR)
	call salloc (str4, SZ_LINE, TY_CHAR)

	# Mark rejected points as deleted for rms comnputation,
	# and count number of deleted points.
	call amovd (wts, Memd[wts1], npts)
	rejected = in_geti (in, INLNREJPTS)
	rejpts   = in_getp (in, INLREJPTS)
	if (rejected > 0) {
	    do i = 1, npts {
	        if (Memi[rejpts+i-1] == YES)
		    Memd[wts1+i-1] = double (0.0)
	    }
	}
	deleted = 0
	do i = 1, npts {
	    if (wts[i] == double (0.0))
	        deleted = deleted + 1
	}

	# Set the fit and compute the RMS error.
	if (in_geti (in, INLFITERROR) == DONE) {
	    call nlvectord (nl, x, Memd[fit], npts, nvars)
	    rms = in_rmsd (y, Memd[fit], Memd[wts1], npts)
	} else
	    rms = INDEFD

	# Build interactive graphics and NLFIT parameter strings
	call sprintf (Memc[str1], SZ_LINE,
	    #"low_rej=%7.4g, high_rej=%7.4g, nreject=%d, grow=%7.4g")
	    "low_rej=%.4g, high_rej=%.4g, nreject=%d, grow=%.4g")
	    call pargd (in_getd (in, INLLOW))
	    call pargd (in_getd (in, INLHIGH))
	    call pargi  (in_geti  (in, INLNREJECT))
	    call pargd (in_getd (in, INLGROW))
	call sprintf (Memc[str2], SZ_LINE,
	    #"total=%d, rejected=%d, deleted=%d, RMS=%7.4g")
	    "total=%d, rejected=%d, deleted=%d, RMS=%.4g")
	    call pargi (npts)
	    call pargi (rejected)
	    call pargi (deleted)
	    call pargd (rms)
	call sprintf (Memc[str3], SZ_LINE,
	    #"tolerance=%7.4g, maxiter=%d, iterations=%d")
	    "tolerance=%.4g, maxiter=%d, iterations=%d")
	    call pargd (nlstatd (nl, NLTOL))
	    call pargi (nlstati (nl, NLITMAX))
	    call pargi (nlstati (nl, NLITER))

	# Set the output parameter line.
	length = strlen (Memc[str1]) + strlen (Memc[str2]) +
	    strlen (Memc[str3]) + 3
	call salloc (line, length + 1, TY_CHAR)
	call sprintf (Memc[line], length, "%s\n%s\n%s")
	    call pargstr (Memc[str1])
	    call pargstr (Memc[str2])
	    call pargstr (Memc[str3])
	call gt_sets (gt, GTPARAMS, Memc[line])

	# Get the error and function label strings.
	call nlerrmsg (in_geti (in, INLFITERROR), Memc[str1], SZ_LINE)
	call in_gstr (in, INLFLABELS, Memc[str2], SZ_LINE)

	# Set the output title line.
	len3 = inlstrwrd (1, Memc[str3], SZ_LINE, Memc[str2])
	len4 = inlstrwrd (2, Memc[str4], SZ_LINE, Memc[str2])
	if (len3 != 0 && len4 != 0) {
	    call sprintf (Memc[line], length, "%s = %s\n%s")
		call pargstr (Memc[str3])
		call pargstr (Memc[str4])
		call pargstr (Memc[str1])
	} else {
	    call sprintf (Memc[line], length, "%s")
		call pargstr (Memc[str2])
	}
	call gt_sets (gt, GTTITLE, Memc[line])

	# Free allocated memory.
	call sfree (sp)
end
