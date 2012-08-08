# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>
include	"icfit.h"
include	"names.h"

# IC_DOSETUP -- Setup the fit.  This is called at the start of each call
# to ic_fit to update the fitting parameters if necessary.

procedure ic_dosetupd (ic, cv, x, wts, npts, newx, newwts, newfunction, refit)

pointer	ic				# ICFIT pointer
pointer	cv				# Curfit pointer
double	x[npts]				# Ordinates of data
double	wts[npts]			# Weights
int	npts				# Number of points in data
int	newx				# New x points?
int	newwts				# New weights?
int	newfunction			# New function?
int	refit				# Use cvrefit?

int	ord
double	xmin, xmax

pointer	rg_xrangesd()
#extern	hd_power$t()
errchk	rg_xrangesd

begin
	# Set sample points.
	if ((newx == YES) || (newwts == YES)) {
	    if (npts == 0)
		call error (0, "No data points for fit")

	    call mfree (IC_XFIT(ic), TY_DOUBLE)
	    call mfree (IC_YFIT(ic), TY_DOUBLE)
	    call malloc (IC_XFIT(ic), npts, TY_DOUBLE)

	    call mfree (IC_WTSFIT(ic), TY_DOUBLE)
	    call malloc (IC_WTSFIT(ic), npts, TY_DOUBLE)

	    call mfree (IC_REJPTS(ic), TY_INT)
	    call malloc (IC_REJPTS(ic), npts, TY_INT)
	    call amovki (NO, Memi[IC_REJPTS(ic)], npts)
	    IC_NREJECT(ic) = 0

	    # Set sample points.

	    call rg_free (IC_RG(ic))
	    IC_RG(ic) = rg_xrangesd (Memc[IC_SAMPLE(ic)], x, npts)
	    call rg_order (IC_RG(ic))
	    call rg_merge (IC_RG(ic))
	    call rg_wtbind (IC_RG(ic), max (1, abs (IC_NAVERAGE(ic))), x, wts,
		npts, Memd[IC_XFIT(ic)], Memd[IC_WTSFIT(ic)], IC_NFIT(ic))

	    if (IC_NFIT(ic) == 0)
		call error (0, "No sample points for fit")

	    if (IC_NFIT(ic) == npts) {
	        call rg_free (IC_RG(ic))
	        call mfree (IC_XFIT(ic), TY_DOUBLE)
	        call mfree (IC_WTSFIT(ic), TY_DOUBLE)
	        IC_YFIT(ic) = NULL
		IC_WTSFIT(ic) = NULL
		call alimd (x, npts, xmin, xmax)
	    } else {
	        call malloc (IC_YFIT(ic), IC_NFIT(ic), TY_DOUBLE)
		if (IC_NFIT(ic) == 1)
		    call alimd (x, npts, xmin, xmax)
		else
		    call alimd (Memd[IC_XFIT(ic)], IC_NFIT(ic), xmin, xmax)
	    }

	    IC_XMIN(ic) = min (IC_XMIN(ic), real(xmin))
	    IC_XMAX(ic) = max (IC_XMAX(ic), real(xmax))
	    refit = NO
	}

	# Set curve fitting parameters.
	# For polynomials define fitting range over range of data in fit
	# and assume extrpolation is ok.  For spline functions define
	# fitting range to be range of evaluation set by the caller
	# since extrapolation will not make sense.

	if ((newx == YES) || (newfunction == YES)) {
	    if (cv != NULL)
	        call dcvfree (cv)

	    switch (IC_FUNCTION(ic)) {
	    case LEGENDRE, CHEBYSHEV:
		ord = min (IC_ORDER(ic), IC_NFIT(ic))
	        call dcvinit (cv, IC_FUNCTION(ic), ord, double (xmin),
		    double (xmax))
	    case SPLINE1:
		ord = min (IC_ORDER(ic), IC_NFIT(ic) - 1)
		if (ord > 0)
	            call dcvinit (cv, SPLINE1, ord, double (IC_XMIN(ic)),
			double (IC_XMAX(ic)))
		else
	            call dcvinit (cv, LEGENDRE, IC_NFIT(ic),
			double (IC_XMIN(ic)), double (IC_XMAX(ic)))
	    case SPLINE3:
		ord = min (IC_ORDER(ic), IC_NFIT(ic) - 3)
		if (ord > 0)
	            call dcvinit (cv, SPLINE3, ord, double (IC_XMIN(ic)),
			double (IC_XMAX(ic)))
		else
	            call dcvinit (cv, LEGENDRE, IC_NFIT(ic),
			double (IC_XMIN(ic)), double (IC_XMAX(ic)))
#	    case USERFNC:
#		ord = min (IC_ORDER(ic), IC_NFIT(ic))
#		call $tcvinit (cv, USERFNC, ord, PIXEL (IC_XMIN(ic)),
#		    PIXEL (IC_XMAX(ic)))
#		call $tcvuserfnc (cv, hd_power$t)
	    default:
		call error (0, "Unknown fitting function")
	    }

	    refit = NO
	}
end
