include	<mach.h>
include	<math/gsurfit.h>

define	ECFTYPES	"|chebyshev|legendre|"		# Fit types


# ECF_SOLVE -- Fit
#
#	f(x, slope*y+offset) = (y+slope*offset)*z
#
# with offset minimizing the RMS.

procedure ecf_solve (ecf, x, y, z, w, r, npts, fixedorder)

pointer	ecf			# GSURFIT pointer
double	x[npts]			# X points
double	y[npts]			# Y points
double	z[npts]			# Z points
double	w[npts]			# Weights
double	r[npts]			# Residuals
int	npts			# Number of points
int	fixedorder		# Fixed order?

int	i, j, k, err
double	ya, yb, newrms, ecf_rms()
pointer	sp, y1, ecf1
errchk	ecf_solve1
include	"ecffit.com"
define	fit_	99

begin
	if (fixedorder == YES) {
	    call ecf_solve1 (ecf, x, y, z, w, r, npts)
	    return
	}

	call smark (sp)
	call salloc (y1, npts, TY_DOUBLE)

	# Determine if the orders are reversed.
	j = 1
	k = 1
	do i = 1, npts {
	    if (z[i] < z[j])
		j = i
	    if (z[i] > z[k])
		k = i
	}
	if (y[j] >= y[k]) {
	    slope = 1
	    offset = max (offset, int(1. - ymin))
	} else {
	    slope = -1
	    offset = max (offset, int(1. + ymax))
	}

	call dgsfree (ecf)
	shift = 0.

	rms = MAX_DOUBLE
	j = 1
	k = 0

	for (i=offset;;i=i+j) {
	    if (slope == 1) {
		ya = i + ymin
		yb = i + ymax
	    } else {
		ya = i - ymax
		yb = i - ymin
	    }
	    if (ya < 1.)
		break

	    call altmd (y, Memd[y1], npts, double(slope), double(i))
	    call amuld (Memd[y1], z, r, npts)

fit_	    call dgsinit (ecf1, gstype, xorder, yorder, YES, xmin, xmax, ya, yb)
	    call dgsfit (ecf1, x, Memd[y1], r, w, npts, WTS_USER, err)

	    if (err != OK) {
	        if (xorder > 2 || yorder > 2) {
	            call dgsfree (ecf)
	            xorder = max (2, xorder - 1)
	            yorder = max (2, yorder - 1)
	            goto fit_
	        }

	        switch (err) {
	        case SINGULAR:
		    call dgsfree (ecf)
		    ecf = ecf1
	            call eprintf ("Singular solution\n")
	        case NO_DEG_FREEDOM:
	            call sfree (sp)
	            call error (0, "No degrees of freedom")
	        }
	    }

	    call dgsvector (ecf1, x, Memd[y1], r, npts)
	    call adivd (r, Memd[y1], r, npts)
	    call asubd (z, r, r, npts)

	    newrms = ecf_rms (r, w, npts)
	    k = k + 1

	    if (newrms / rms < 0.999) {
	        call dgsfree (ecf)
	        ecf = ecf1
		offset = i
		rms = newrms
	    } else {
	        call dgsfree (ecf1)
		if (k > 2)
		    break
		i = offset
		j = -j
	    }
	}

	call altmd (y, Memd[y1], npts, double(slope), double(offset))
	call dgsvector (ecf, x, Memd[y1], r, npts)
	call adivd (r, Memd[y1], r, npts)
	call asubd (z, r, r, npts)

	call sfree (sp)

end


# ECF_SOLVE1 -- Fit f(x, y+offset) = (y+offset)*z with offset fixed.

procedure ecf_solve1 (ecf, x, y, z, w, r, npts)

pointer	ecf			# GSURFIT pointer
double	x[npts]			# X points
double	y[npts]			# Y points
double	z[npts]			# Z points
double	w[npts]			# Weights
double	r[npts]			# Residuals
int	npts			# Number of points

int	err
pointer	sp, y1
double	ya, yb, ecf_rms()
include	"ecffit.com"
define	fit_	99

begin
	call smark (sp)
	call salloc (y1, npts, TY_DOUBLE)

	call dgsfree (ecf)
	shift = 0.

	if (slope == 1) {
	    offset = max (offset, int(1. - ymin))
	    ya = offset + ymin
	    yb = offset + ymax
	} else {
	    offset = max (offset, int(1. + ymax))
	    ya = offset - ymax
	    yb = offset - ymin
	}

	call altmd (y, Memd[y1], npts, double (slope), double (offset))
	call amuld (Memd[y1], z, r, npts)

fit_	call dgsinit (ecf, gstype, xorder, yorder, YES, xmin, xmax,
	    min (ya, yb), max (ya, yb))
	call dgsfit (ecf, x, Memd[y1], r, w, npts, WTS_USER, err)

	if (err != OK) {
	    if (xorder > 2 || yorder > 2) {
	        call dgsfree (ecf)
	        xorder = max (2, xorder - 1)
	        yorder = max (2, yorder - 1)
	        goto fit_
	    }

	    switch (err) {
	    case SINGULAR:
	        call eprintf ("Singular solution\n")
	    case NO_DEG_FREEDOM:
	        call sfree (sp)
	        call error (0, "No degrees of freedom")
	    }
	}

	call dgsvector (ecf, x, Memd[y1], r, npts)
	call adivd (r, Memd[y1], r, npts)
	call asubd (z, r, r, npts)
	rms = ecf_rms (r, w, npts)

	call sfree (sp)
end
