include	<mach.h>
include	"hdicfit.h"

# HDIC_TRANSFORM -- Transform density to independent variable of fit.  The
# desired transform is stored in the ic structure.  A vector of x values
# is returned, as is a possibly modified weights array.  The minimum and
# maximum limits of the fit are updated in the ic structure; the labels
# are set also when IC_NEWTRANSFORM = YES.  The fog value is subtracted
# from the input density array and the transform performed. 

procedure hdic_transform (ic, density, userwts, xout, wts, whydel, npts)

pointer	ic		# Pointer to ic structure
double	density[npts]	# Array of original density values
double	userwts[npts]	# Array of original weights values
double	xout[npts]	# Transformed density above fog (returned)
double	wts[npts]	# Input weights array
int	whydel[npts]	# Reason for deletion array 
int	npts		# The number of density points - maybe changed on output

int	i
pointer	denaf, sp
double	fog, xxmin, xxmax, dval
bool	fp_equald()
real	ic_getr()
include	"hdic.com"

begin
	# Allocate space for density above fog array
	call smark (sp)
	call salloc (denaf, npts, TY_DOUBLE)

	fog = double (ic_getr (ic, "fog"))
	call asubkd (density, fog, Memd[denaf], npts)

	switch (IC_TRANSFORM(ic)) {
	case HD_NONE:
	    do i = 1, npts {
		xout[i] = Memd[denaf+i-1]
		# In every case, if the point was deleted by the program, 
		# restore it.
		if (whydel[i] == PDELETE) {
	            wts[i] = userwts[i]
		    whydel[i] = NDELETE
	        }
	    }

	    call ic_pstr (ic, "xlabel", "Density above Fog")
	    xxmin = MIN_DEN - fog
	    xxmax = maxden
	    call ic_putr (ic, "xmin", real (xxmin))
	    call ic_putr (ic, "xmax", real (xxmax))

	case HD_LOGO:
	    call ic_pstr (ic, "xlabel", "Log Opacitance: Log (10**Den - 1)")
	    xxmin = log10 ((10. ** (MIN_DEN)) - 1.0)
	    xxmax = log10 ((10. ** (maxden)) - 1.0)
	    call ic_putr (ic, "xmin", real (xxmin))
	    call ic_putr (ic, "xmax", real (xxmax))

	    do i = 1, npts {
		dval = Memd[denaf+i-1]
		if (dval < 0.0D0 || (fp_equald (dval, 0.0D0))) {
		    xout[i] = dval
		    wts[i] = 0.0D0
		    whydel[i] = PDELETE

		} else {
		    xout[i] = log10 ((10. ** (dval)) - 1.0)

		    # If point had been deleted, find out why.  It affects the
		    # weights value returned.  Only if the point was previously
		    # deleted by the program, restore it; otherwise, leave it 
		    # alone.

		    if (fp_equald (wts[i], 0.0D0)) {
		        if (whydel[i] == PDELETE) {
		            wts[i] = userwts[i]
			    whydel[i] = NDELETE
			}
		    } else
			wts[i] = userwts[i]
	    }
	}

	case HD_K75:
	    call ic_pstr (ic, "xlabel", "Den + 0.75 * Log (1 - (10 ** -Den))")
	    xxmin = MIN_DEN + 0.75 * log10 (1.0 - (10. ** (-MIN_DEN)))
	    xxmax = maxden + 0.75 * log10 (1.0 - (10. ** (-maxden)))
	    call ic_putr (ic, "xmin", real (xxmin))
	    call ic_putr (ic, "xmax", real (xxmax))

	    do i = 1, npts {
		dval = Memd[denaf+i-1]
		if (dval < 0.0D0 || (fp_equald (dval, 0.0D0))) {
		    xout[i] = dval
		    wts[i] = 0.0D0
		    whydel[i] = PDELETE

		} else {
		    xout[i] = dval + 0.75 * log10 (1.0 - (10.** (-dval)))

		    # If point had been deleted, find out why.  It affects the
		    # weights value returned.  Only if the point was previously
		    # deleted by the program, restore it; otherwise, leave it 
		    # alone.

		    if (fp_equald (wts[i], 0.0D0)) {
		        if (wts[i] == PDELETE) {
		            wts[i] = userwts[i]
			    whydel[i] = NDELETE
			}
		    } else
			wts[i] = userwts[i]
	    }
	}

	case HD_K50:
	    call ic_pstr (ic, "xlabel", "Den + 0.50 * Log (1 - (10 ** -Den))")
	    xxmin = MIN_DEN + 0.50 * log10 (1.0 - (10. ** (-MIN_DEN)))
	    xxmax = maxden + 0.50 * log10 (1.0 - (10. ** (-maxden)))
	    call ic_putr (ic, "xmin", real (xxmin))
	    call ic_putr (ic, "xmax", real (xxmax))

	    do i = 1, npts {
		dval = Memd[denaf+i-1]
		if (dval < 0.0D0 || (fp_equald (dval, 0.0D0))) {
		    xout[i] = dval
		    wts[i] = 0.0D0
		    whydel[i] = PDELETE

		} else {
		    xout[i] = dval + 0.50 * log10 (1.0 - (10.** (-dval)))

		    # If point had been deleted, find out why.  It affects the
		    # weights value returned.  Only if the point was previously
		    # deleted by the program, restore it; otherwise, leave it 
		    # alone.

		    if (fp_equald (wts[i], 0.0D0)) {
		        if (wts[i] == PDELETE) {
		            wts[i] = userwts[i]
			    whydel[i] = NDELETE
			}
		    } else
			wts[i] = userwts[i]
		}
	    }

	default:
	    call eprintf ("Unrecognizable Transform\n")
	}

	call sfree (sp)
end
