include	<pkg/gtools.h>
include	"sensfunc.h"

define	RANGE_AIRMASS	0.1	# Minimum airmass range
define	SIGMA_AIRMASS	0.05	# Minimum sigma in airmass

# SF_EXINCT -- Determine a residual extinction curve.  At each wavelength
# for which there are multiple observations or neighboring wavelengths
# such that there is a sufficient airmass range determine the slope
# of the sensitivity vs airmass.  Residual sensitivity is used to minimize
# wavelength scatter when multiple wavelengths are needed because of
# nonoverlapping standard star data.  Each such slope is a measure of the
# residual extinction at that wavelength.  To make the residual extinction
# curve fit the extinction vs. wavelength using ICFIT.

procedure sf_extinct (gp, stds, nstds, cv, ecv, function, order)

pointer	gp			# Graphics structure
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
pointer	ecv			# Residual extinction curve
char	function[ARB]		# Fitting function
int	order			# Function order

bool	ans
int	i, j, n, nwaves, sum, npts, scan()
real	a, amin, amax, rms, rms1, r2, sig, cveval()
double	x, y, z, sumx, sumy, sumz, sumx2, sumxy
pointer	sp, waves, sens, airm, xp, yp, fp, wp, ic
pointer	gt, gt_init()
errchk	salloc, xt_sort3, icg_fit, ic_open

define	cancel_	99

begin
	# Cancel previous extinction if defined.
	if (ecv != NULL)
	    goto cancel_
	
	# Check for minimum airmass range and determine number of points.
	# Ignore added objects and composite data.
	amin = 100.
	amax = 0.
	nwaves = 0
	do i = 1, nstds - 2 {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    nwaves = nwaves + STD_NWAVES(stds[i])
	    a = STD_AIRMASS(stds[i])
	    amin = min (amin, a)
	    amax = max (amax, a)
	}
	if (amax - amin < RANGE_AIRMASS) {
	    call printf (
		"Insufficient airmass coverage for extinction determination")
	    return
	}

	# Extract data to be used and sort by wavelength.
	# The data is wavelength, airmass, and residual sensitivity.
	call smark (sp)
	call salloc (waves, nwaves, TY_REAL)
	call salloc (sens, nwaves, TY_REAL)
	call salloc (airm, nwaves, TY_REAL)
	    
	nwaves = 0
	do i = 1, nstds-2 {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    a = STD_AIRMASS(stds[i])
	    xp = STD_WAVES(stds[i])
	    yp = STD_SENS(stds[i])
	    fp = STD_FIT(stds[i])
	    wp = STD_WTS(stds[i])
	    do j = 1, n {
		if (Memr[wp] != 0.) {
		    Memr[airm+nwaves] = a
		    Memr[waves+nwaves] = Memr[xp]
		    Memr[sens+nwaves] = Memr[yp] - Memr[fp]
		    nwaves = nwaves + 1
		}
		xp = xp + 1
		yp = yp + 1
		fp = fp + 1
		wp = wp + 1
	    }
	}

	call xt_sort3 (Memr[waves], Memr[sens], Memr[airm], nwaves)

	# Bin points with common wavelengths or at least two points.
	sum = 0
	sumx = 0.
	sumy = 0.
	sumz = 0.
	sumx2 = 0.
	sumxy = 0.
	n = 0
	do i = 0, nwaves-2 {
	    x = Memr[airm+i]
	    y = Memr[sens+i]
	    z = Memr[waves+i]
	    sum = sum + 1
	    sumx = sumx + x
	    sumy = sumy + y
	    sumx2 = sumx2 + x * x
	    sumxy = sumxy + x * y
	    sumz = sumz + z

	    if ((z == Memr[waves+i+1]) || (sum < 2))
		next

	    x = sumx2 - sumx * sumx / sum
	    if (x > SIGMA_AIRMASS) {
	        Memr[waves+n] = sumz / sum
	        Memr[sens+n] = (sumx * sumy / sum - sumxy) / x
	        Memr[airm+n] = 1.
	        n = n + 1
		sum = 0
		sumx = 0.
		sumy = 0.
		sumz = 0.
		sumx2 = 0.
		sumxy = 0.
	    }
	}
	if (sum > 1) {
	    x = sumx2 - sumx * sumx / sum
	    if (x > SIGMA_AIRMASS) {
	        Memr[waves+n] = sumz / sum
	        Memr[sens+n] = (sumx * sumy / sum - sumxy) / x
	        Memr[airm+n] = 1.
		n = n + 1
	    }
	}

	if (n < 2) {
	    call printf ("Cannot determine residual extinction")
	    call sfree (sp)
	    return
	}

	# Fit residual extinction curve using ICFIT.
	gt = gt_init()
	call gt_sets (gt, GTTYPE, "mark")
	call gt_seti (gt, GTCOLOR, GP_PLCOLOR(gp))
	call ic_open (ic)
	call ic_putr (ic, "xmin", min (STD_WSTART(stds[1]), STD_WEND(stds[1])))
	call ic_putr (ic, "xmax", max (STD_WSTART(stds[1]), STD_WEND(stds[1])))
	call ic_pstr (ic, "function", "chebyshev")
	call ic_puti (ic, "order", 1)
	call ic_pstr (ic, "xlabel", "wavelength")
	call ic_pstr (ic, "ylabel", "residual extinction")
	call ic_pstr (ic, "yunits", "mag")
	call icg_fit (ic, GP_GIO(gp), "cursor", gt, ecv, Memr[waves],
	    Memr[sens], Memr[airm], n)
	call ic_closer (ic)
	call gt_free (gt)

	# Determine significance of the fit.
	call sf_fit (stds, nstds, cv, function, order,
	    min (GP_WSTART(gp), GP_WEND(gp)), max (GP_WSTART(gp), GP_WEND(gp)))
	call sf_rms (stds, nstds, rms1, npts)
	do i = 1, nstds - 2 {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    a = STD_AIRMASS(stds[i])
	    xp = STD_WAVES(stds[i])
	    yp = STD_SENS(stds[i])
	    call cvvector (ecv, Memr[xp], Memr[sens], n)
	    call amulkr (Memr[sens], a, Memr[sens], n)
	    call aaddr (Memr[yp], Memr[sens], Memr[yp], n)
	}
	call sf_fit (stds, nstds, cv, function, order,
	    min (GP_WSTART(gp), GP_WEND(gp)), max (GP_WSTART(gp), GP_WEND(gp)))
	call sf_rms (stds, nstds, rms, npts)
	do i = 1, SF_NGRAPHS
	    if (GP_SHDR(gp,i) != NULL)
		call shdr_close (GP_SHDR(gp,i))

	r2 = 1 - rms ** 2 / rms1 ** 2
	sig = r2 * (nwaves - 2) / max (0.01, 1. - r2)
	if (sig <= 0.0)
	    sig = 0.
	else
	    sig = sqrt (sig)

	# Apply to data if desired.
	call printf (
	"Significance = %4.1f sigma:  Apply residual extinction correction? ")
	    call pargr (sig)
	call flush (STDOUT)

	ans = false
	if (scan() != EOF)
	    call gargb (ans)

	# Undo last fit if not applying correction.
	if (!ans)
	    goto cancel_

	call printf ("Residual extinction correction applied")
	call sfree (sp)
	return

cancel_
	do i = 1, nstds - 2 {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    a = STD_AIRMASS(stds[i])
	    xp = STD_WAVES(stds[i])
	    yp = STD_SENS(stds[i])
	    do j = 1, n {
		Memr[yp] = Memr[yp] - a * cveval (ecv, Memr[xp])
		xp = xp + 1
		yp = yp + 1
	    }
	}
	call cvfree (ecv)
	call printf ("Residual extinction correction canceled")
	call sfree (sp)
end
