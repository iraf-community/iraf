include	<math/curfit.h>
include	"hdicfit.h"

# IC_DOSETUP -- Setup the fit.  This is called at the start of each call
# to ic_fit to update the fitting parameters if necessary.

procedure ic_dosetupd (ic, cv, x, wts, npts, newx, newwts, newfunction, refit)

pointer	ic				# ICFIT pointer
pointer	cv				# Curfit pointer
double	x[ARB]				# Ordinates of data
double	wts[ARB]			# Weights
int	npts				# Number of points in data
int	newx				# New x points?
int	newwts				# New weights?
int	newfunction			# New function?
int	refit				# Use cvrefit?

int	ord
pointer	rg_xrangesd()
extern	hd_powerd()
errchk	rg_xrangesd, malloc

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
	    call rg_wtbind (IC_RG(ic), abs (IC_NAVERAGE(ic)), x, wts, npts,
		Memd[IC_XFIT(ic)], Memd[IC_WTSFIT(ic)], IC_NFIT(ic))

	    if (IC_NFIT(ic) == 0)
		call error (0, "No sample points for fit")

	    if (IC_NFIT(ic) == npts) {
	        call rg_free (IC_RG(ic))
	        call mfree (IC_XFIT(ic), TY_DOUBLE)
	        call mfree (IC_WTSFIT(ic), TY_DOUBLE)
	        IC_YFIT(ic) = NULL
		IC_WTSFIT(ic) = NULL
	    } else
	        call malloc (IC_YFIT(ic), IC_NFIT(ic), TY_DOUBLE)

	    refit = NO
	}

	# Set curve fitting parameters.

	if ((newx == YES) || (newfunction == YES)) {
	    if (cv != NULL)
	        call dcvfree (cv)

	    switch (IC_FUNCTION(ic)) {
	    case LEGENDRE, CHEBYSHEV:
		ord = min (IC_ORDER(ic), IC_NFIT(ic))
	        call dcvinit (cv, IC_FUNCTION(ic), ord, double (IC_XMIN(ic)),
		    double (IC_XMAX(ic)))
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
 	    case USERFNC:
 		ord = min (IC_ORDER(ic), IC_NFIT(ic))
 		call dcvinit (cv, USERFNC, ord, double (IC_XMIN(ic)),
 		    double (IC_XMAX(ic)))
 		call dcvuserfnc (cv, hd_powerd)
	    default:
		call error (0, "Unknown fitting function")
	    }

	    refit = NO
	}
end
