include	<error.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"


# FORCE_REBIN - Rebin the input spectrum in log space from a specified
# uniform log binning.  This is to force both object and templates to the
# same dispersion if at all possible.

int procedure force_rebin (rv)

pointer	rv					#I RV struct pointer

int     npts, dcflag, which, status
int	force_which(), rv_getim()
real    w0, wpc, w2
errchk  realloc

begin
	# Skip this if not dispersion corrected.
	if (RV_DCFLAG(rv) == -1)
	    return (OK)

	# First get the intermediate values to rebin the spectrum
	dcflag = 1				# Rebinning to log dispersion.
	if (force_which (rv, which, w0, wpc, npts) == NO)
	    return (OK)

	# Move the data before rebinning and increase the size of the data cache
	if (which == OBJECT_SPECTRUM) {
	    call realloc (RV_OPIXX(rv), npts, TY_REAL)
	    call realloc (RV_OPIXY(rv), npts, TY_REAL)
	    call aclrr (OBJPIXX(rv,1), npts)
	    call aclrr (OBJPIXY(rv,1), npts)
	} else {
	    call realloc (RV_RPIXX(rv), npts, TY_REAL)
	    call realloc (RV_RPIXY(rv), npts, TY_REAL)
	    call aclrr (REFPIXX(rv,1), npts)
	    call aclrr (REFPIXY(rv,1), npts)
	}

	w2 = w0 + (npts - 1) * wpc
	if (DEBUG(rv)) {
	    call d_printf (DBG_FD(rv), "force_rebin:\n")
	    call d_printf (DBG_FD(rv), "\tw0,wpc,w2,npts = %g,%g,%g,%d - %d\n")
	        call pargr(w0);call pargr(wpc);call pargr(w2)
		call pargi(npts);call pargi(which)
	    call d_printf (DBG_FD(rv), "\to_w0=%g  o_wpc=%g o_w2=%g\n")
	      	call pargr(RV_OW0(rv))   ;   call pargr(RV_OWPC(rv))
		call pargr(RV_OW2(rv))
	    call d_printf (DBG_FD(rv), "\tr_w0=%g  r_wpc=%g r_w2=%g\n")
	      	call pargr(RV_RW0(rv))   ;   call pargr(RV_RWPC(rv))
		call pargr(RV_RW2(rv))
	}

	# Now rebin to the desired dispersion.
	#wpc = (10.0 ** (w0 + (npts-1)*wpc) - 10.0 ** w0) / (npts - 1)
	w2 = 10.0 ** w2
	w0 = 10.0 ** w0
	if (which == OBJECT_SPECTRUM)
            status = rv_getim (rv, IMAGE(rv), OBJECT_SPECTRUM, w0, w2, npts)
	else 
	    status = rv_getim (rv, RIMAGE(rv), REFER_SPECTRUM, w0, w2, npts)

	if (DEBUG(rv)) {
	    call d_printf (DBG_FD(rv), "\tend: w0,wpc,w2,npts = %g,%g,%g,%d\n")
	        call pargr(w0);call pargr(wpc);call pargr(w2);call pargi(npts)
	}

	# Re-calculate the velocity dispersion and global endpoints
	RV_DELTAV(rv) = wpc * CLN10
	RV_GLOB_W1(rv) = min (RV_OW0(rv), RV_RW0(rv))
	RV_GLOB_W2(rv) = max (RV_OW2(rv), RV_RW2(rv))

	return (OK)
end


# FORCE_WHICH - Determine if spectra have to be rebinned to have the same
# dispersion and same starting wavelengths to within an integer shift.

int procedure force_which (rv, which, w0, wpc, npts)

pointer	rv					#I RV struct pointer
int	which					#O Which spectrum?
real	w0					#O Rebinned W0
real	wpc					#O Rebinned WPC
int	npts					#O Rebinned number of pixels

int	i
int	stat
bool	fp_equalr()

begin
	# Check if the spectra have the same dispersion.
	which = OBJECT_SPECTRUM
	w0 = RV_RW0(rv)
	wpc = RV_RWPC(rv)
	npts = RV_RNPTS(rv)
	stat = NO
	if (fp_equalr (w0, RV_OW0(rv)) && fp_equalr (wpc, RV_OWPC(rv)))
	    return (stat)

	# Determine spectrum to rebin.
	switch (RV_REBIN(rv)) {
	case RB_OBJ:
	    which = REFER_SPECTRUM
	case RB_TEMP:
	    which = OBJECT_SPECTRUM
	case RB_SMALL:
	    if (abs (RV_OWPC(rv)) < abs (RV_RWPC(rv)))
	        which = REFER_SPECTRUM
	    else
	        which = OBJECT_SPECTRUM
	case RB_BIG:
	    if (abs (RV_OWPC(rv)) > abs (RV_RWPC(rv)))
	        which = REFER_SPECTRUM
	    else
	        which = OBJECT_SPECTRUM
	}

	# Set the new dispersion parameters.  The dispersion is the same as
	# the target spectrum and the starting wavelength is adjusted by a
	# fractional pixel amount into the data so that the starting
	# wavelengths of the two spectra are an integer number of pixels
	# apart in the common dispersion.

	switch (which) {
	case REFER_SPECTRUM:
	    w0 = RV_OW0(rv)
	    wpc = RV_OWPC(rv)
	    if (RV_RWPC(rv) / RV_OWPC(rv) > 0)
		i = (RV_RW0(rv) - w0) / wpc + 0.999
	    else
		i = (RV_RW0(rv) - w0) / wpc
	    w0 = w0 + i * wpc
	    npts = (RV_RW2(rv) - w0) / wpc + 1
	    if (!fp_equalr (w0, RV_RW0(rv)) || !fp_equalr (wpc, RV_RWPC(rv)))
		stat = YES
	case OBJECT_SPECTRUM:
	    w0 = RV_RW0(rv)
	    wpc = RV_RWPC(rv)
	    if (RV_RWPC(rv) / RV_OWPC(rv) > 0)
		i = (RV_OW0(rv) - w0) / wpc + 0.999
	    else
		i = (RV_OW0(rv) - w0) / wpc
	    w0 = w0 + i * wpc
	    npts = (RV_OW2(rv) - w0) / wpc + 1
	    if (!fp_equalr (w0, RV_OW0(rv)) || !fp_equalr (wpc, RV_OWPC(rv)))
		stat = YES
	}

	return (stat)
end
