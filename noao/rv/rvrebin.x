include	<error.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"

# RV_REBIN - Rebin the input and template spectrum to log space from linear.
# Most of the code for this file was taken directly from the system version
# of the REBIN task.

procedure rv_rebin (rv, indata, dnpts, outdata, npts, w0, wpc, dcflag, new_pars)

pointer	rv					#I RV struct pointer
real	indata[ARB]				#U Data array to be converted
int	dnpts					#I Npts in original data
real	outdata[ARB]				#U Converted data array 
int	npts					#U Suggested npts to convert
real	w0, wpc					#U Suggested wavlength params 
int	dcflag					#U Suggested DC-FLAG
bool	new_pars				#I Change suggested parameters

pointer	sp, dp
int	x_npts
real	x_w0, x_wpc, a, b, w2

begin
	# Initialize
	x_w0 = w0
	x_wpc = wpc
	x_npts = npts

	if (new_pars)
	    a = log10(x_w0)
	else 
	    a = x_w0
	x_w0 = a
	if (dcflag == 0)
	    b = log10 (w0 + (dnpts-1) * wpc)
	else {
	    if (new_pars)
	        b = w0 + (dnpts-1) * wpc
	    else
	        b = w0 + (npts-1) * wpc
	}
	w2 = INDEF
	#w2 = b
	if (new_pars)
	    x_wpc = INDEF
	else
	    x_npts = npts
	call dc_defaults (a, b, dnpts, x_w0, w2, x_wpc, x_npts)

	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
	    call d_printf (DBG_FD(rv), "rv_rebin:\n")
	    call d_printf (DBG_FD(rv), "\tB4 - w0,wpc,npts = %g,%g,%d\n")
		call pargr(w0);call pargr(wpc);call pargi(dnpts)
	    call d_printf (DBG_FD(rv), "\tx  - w0,wpc,npts = %g,%g,%d\n")
		call pargr(x_w0);call pargr(x_wpc);call pargi(x_npts)
	    call d_printf (DBG_FD(rv), "\t   - a,b,w2 = %g,%g,%g\n")
		call pargr(a);call pargr(b);call pargr(w2)
	    call d_flush (DBG_FD(rv))
	}

	# Now do the rebinning
	call smark  (sp)
	call salloc (dp, x_npts+4, TY_REAL)
	if (new_pars)
	    call dc_seteval (w0, wpc, 0)
	call dispcor (indata, dnpts, Memr[dp], x_w0, x_wpc, x_npts, true, 
	    true, RV_INTERP(rv))

	# Find out how many new pixels are in the data
	call amovr (Memr[dp], outdata, x_npts)

	# Recover (new) values
	npts = x_npts
	w0 = x_w0
	wpc = x_wpc

	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116)
	    call d_printf (DBG_FD(rv), "\texiting - rv_rebin\n")
	call sfree (sp)
end


# FORCE_REBIN - Rebin the input spectrum in log space from a specified uniform
# log binning.  This is to force both object and templates to the same disper-
# if at all possible.

int procedure force_rebin (rv)

pointer	rv					#I RV struct pointer

pointer	sp, dy, tmp
int	i, a, npts, dcflag, which
real	diff, w0, wpc, w2
errchk	realloc

begin
	# Save the shift for the correction
	RV_W0_SHIFT(rv) = (RV_OW0(rv) - RV_RW0(rv)) / RV_OWPC(rv)

	# First get the intermediate values to rebin the spectrum
	dcflag = 1				# Rebinning a log dispersion.
	call force_which (rv, wpc, which)

	# Fix starting wavelength so it's an even wpc increment difference.
	diff  = max (RV_RW0(rv),RV_OW0(rv)) - min (RV_RW0(rv),RV_OW0(rv))
	a = int (diff / wpc)
	if (diff != 0.0)
	    w0 = max (RV_RW0(rv),RV_OW0(rv)) - (a+1) * wpc
	else
	    w0 = RV_OW0(rv)
	
	# Set the number of points to be done.
	npts = max (RV_NPTS(rv), RV_RNPTS(rv))
	if (which == OBJECT_SPECTRUM)
	    npts = (RV_OW2(rv) - w0) / wpc - 1
	else
	    npts = (RV_RW2(rv) - w0) / wpc - 1
	w2 = w0 + (npts-1) * wpc

	call smark (sp)
	call salloc (dy, npts, TY_REAL)
	call salloc (tmp, SZ_LINE, TY_CHAR)

	# Move the data before rebinning and increase the size of the data cache
	call amovkr (0.0, Memr[dy], npts)
	if (which == OBJECT_SPECTRUM) {
	    call amovr (OBJPIXY(rv,1), Memr[dy], RV_NPTS(rv))
	    call realloc (RV_OPIXX(rv), npts, TY_REAL)
	    call realloc (RV_OPIXY(rv), npts, TY_REAL)
	} else {
	    call amovr (REFPIXY(rv,1), Memr[dy], RV_RNPTS(rv))
	    call realloc (RV_RPIXX(rv), npts, TY_REAL)
	    call realloc (RV_RPIXY(rv), npts, TY_REAL)
	}

	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
	    call d_printf (DBG_FD(rv), "force_rebin:\n")
	    call d_printf (DBG_FD(rv), "\tw0,wpc,w2,npts = %g,%g,%g,%d - %d\n")
	        call pargr(w0);call pargr(wpc);call pargr(w2)
		call pargi(npts);call pargi(which)
	    call d_printf (DBG_FD(rv), "\to_wpc=%g  r_wpc=%g  w=%d\n")
	      call pargr(RV_OWPC(rv));call pargr(RV_RWPC(rv));call pargi(which)
	}

	# Now rebin the object to the template dispersion
	if (CON_SAMPLE(rv) == NULL)
	    call calloc (CON_SAMPLE(rv), SZ_LINE, TY_CHAR)
	else
	    call strcpy (Memc[CON_SAMPLE(rv)], Memc[tmp], SZ_LINE)
	call strcpy ("\0", Memc[CON_SAMPLE(rv)], SZ_LINE)
	if (which == OBJECT_SPECTRUM) {
	    call dc_seteval (RV_OW0(rv), RV_OWPC(rv), RV_DCFLAG(rv))
	    call rv_rebin (rv, OBJPIXY(rv,1), RV_NPTS(rv), Memr[dy], npts, 
		w0, wpc, dcflag, false)

	    # Now move the data back into the original array and recalculate the
	    # X object array.
	    call amovr (Memr[dy], OBJPIXY(rv,1), npts)
	    do i = 1, npts
	        OBJPIXX(rv,i) = w0 + (i-1) * wpc

	    # Update the params
	    RV_OW0(rv) = w0
	    RV_OWPC(rv)= wpc
	    RV_NPTS(rv) = npts
	    RV_OW2(rv) = w0 + (npts-1) * wpc

	    # Since we rebinned the original, we need to reset some flags so
	    # things behave properly.  The do_continuum() task will reset the
	    # data preparation flags, otherwise we do it by hand.
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == OBJ_ONLY) {
		call sprintf (Memc[CON_SAMPLE(rv)], SZ_LINE, "%d:%d\0")
		    call pargi (1)
		    call pargi (npts-1)
	        call do_continuum (rv, OBJECT_SPECTRUM)
	    } else {
	        OBJCONT(rv) = NO
	    }

	} else {
	    call dc_seteval (RV_RW0(rv), RV_RWPC(rv), RV_DCFLAG(rv))
	    call rv_rebin (rv, REFPIXY(rv,1), RV_RNPTS(rv), Memr[dy], npts, 
		w0, wpc, dcflag, false)

	    # Now move the data back into the original array and recalculate the
	    # X object array.
	    call amovr (Memr[dy], REFPIXY(rv,1), npts)
	    do i = 1, npts
	        REFPIXX(rv,i) = w0 + (i-1) * wpc

	    # Update the params
	    RV_RW0(rv) = w0
	    RV_RWPC(rv)= wpc
	    RV_RNPTS(rv) = npts
	    RV_RW2(rv) = w0 + (npts-1) * wpc

	    # Since we rebinned the original, we need to reset some flags so
	    # things behave properly.  The do_continuum() task will reset the
	    # data preparation flags, otherwise we do it by hand.
	    if (RV_CONTINUUM(rv) == BOTH || RV_CONTINUUM(rv) == TEMP_ONLY) {
		call sprintf (Memc[CON_SAMPLE(rv)], SZ_LINE, "%d:%d\0")
		    call pargi (1)
		    call pargi (npts-1)
	        call do_continuum (rv, REFER_SPECTRUM)
	    } else {
	        REFCONT(rv) = NO
	    }
	}
	call strcpy (Memc[tmp], Memc[CON_SAMPLE(rv)], SZ_LINE)

	# Re-calculate the velocity dispersion and sundry
	RV_DELTAV(rv) = wpc * CLN10
	RV_GLOB_W1(rv) = min (RV_OW0(rv), RV_RW0(rv))
	RV_GLOB_W2(rv) = max (RV_OW2(rv), RV_RW2(rv))

	call sfree (sp)
	return (OK)
end


# FORCE_WHICH - Determine which spectrum is to be rebinned.

procedure force_which (rv, wpc, which)

pointer	rv					#I RV struct pointer
real	wpc					#O Rebinned WPC
int	which					#O Which spectrum?

begin
	switch (RV_REBIN(rv)) {
	case RB_OBJ:
	    wpc = RV_OWPC(rv)			# temp to object wpc
	    which = REFER_SPECTRUM
	case RB_TEMP:
	    wpc = RV_RWPC(rv)			# obj to template wpc
	    which = OBJECT_SPECTRUM
	case RB_SMALL:
	    if (RV_OWPC(rv) < RV_RWPC(rv)) {
	        wpc = RV_OWPC(rv)		# temp to object wpc
	        which = REFER_SPECTRUM
	    } else {
	        wpc = RV_RWPC(rv)		# obj to template wpc
	        which = OBJECT_SPECTRUM
	    }
	case RB_BIG:
	    if (RV_OWPC(rv) > RV_RWPC(rv)) {
	        wpc = RV_OWPC(rv)		# temp to object wpc
	        which = REFER_SPECTRUM
	    } else {
	        wpc = RV_RWPC(rv)		# obj to template wpc
	        which = OBJECT_SPECTRUM
	    }
	}
end
