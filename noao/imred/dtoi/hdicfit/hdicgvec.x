include	<mach.h>
include	"hdicfit.h"

# HDIC_GVEC -- Get a vector of data to be plotted.  The raw density
# values are stored in common.  From these values, all possible transforms
# can be generated.

procedure hdic_gvec (ic, xout, npts, transform)

pointer	ic		# Pointer to ic structure
double	xout[npts]	# Output vector
int	npts		# Desired npoints to output - possibly changed on output
int	transform	# Integer code for desired type of transform

pointer	sp, bigdenaf
int	i, j, npts_desired
double	fog, dval
real	ic_getr()
include	"hdic.com"

begin
	npts_desired = npts
	if (npts_desired != NVALS_FIT) 
	    call error (0, "hdicgvec: nvals != NVALS_FIT")

	call smark (sp)
	call salloc (bigdenaf, npts, TY_DOUBLE)

	j = 1

	fog = double (ic_getr (ic, "fog"))
	call asubkd (Memd[big_den], fog, Memd[bigdenaf], npts)

	switch (transform) {
	case HD_NONE:
	    call amovd (Memd[bigdenaf], xout, NVALS_FIT)

	case HD_LOGO:
	    do i = 1, npts_desired {
		dval = Memd[bigdenaf+i-1]
		if (dval < 0.0D0)
		    npts = npts - 1
		else {
		    xout[j] = log10 ((10.**dval) - 1.0)
		    j = j + 1
		}
	    }

	case HD_K75:
	    do i = 1, npts_desired {
		dval = Memd[bigdenaf+i-1]
		if (dval < 0.0D0)
		    npts = npts - 1
		else {
		    xout[j] = dval + 0.75 * log10 (1.0 - (10.** (-dval)))
		    j = j + 1
		}
	    }

	case HD_K50:
	    do i = 1, npts_desired {
		dval = Memd[bigdenaf+i-1]
		if (dval < 0.0D0)
		    npts = npts - 1
		else {
		    xout[j] = dval + 0.50 * log10 (1.0 - (10.** (-dval)))
		    j = j + 1
		}
	    }

	}

	call sfree (sp)
end
