include <math.h>
include "rvpackage.h"
include "rvflags.h"
include "rvcont.h"

# RV_FFTCORR - Parent routine for the FFT correlation. This procedure prepares
# the data and processes the FFT.

procedure rv_fftcorr (rv, plot_only)

pointer	rv					#I RV struct pointer
int	plot_only				#I Plot prepared spectra only?

pointer	sp
pointer	otempy, rtempy
pointer	wkobj, wkref, ans
int	npts, fnpts
int	i, ishift
int	fft_pow2()
errchk	realloc

begin
	RV_UPDATE(rv) = YES			# set the update flag

	npts = int ((RV_GLOB_W2(rv) - RV_GLOB_W1(rv)) / RV_OWPC(rv) + 1)
	npts = max (npts, max (RV_NPTS(rv),RV_RNPTS(rv)))
	fnpts = fft_pow2 (npts)
	RV_CCFNPTS(rv) = fnpts
	RV_FFTNPTS(rv) = fnpts

	# (Re)allocate the answer vectors.
	call realloc (RV_WKPIXX(rv), 2*fnpts, TY_REAL)
	call realloc (RV_WKPIXY(rv), 2*fnpts, TY_REAL)

	call smark (sp)				# allocate some pointers
	call salloc (wkobj, 2*fnpts, TY_REAL)
	call salloc (wkref, 2*fnpts, TY_REAL)
	call salloc (otempy, 2*fnpts, TY_REAL)
	call salloc (rtempy, 2*fnpts, TY_REAL)
	call salloc (ans, 2*fnpts, TY_REAL)

	# Clear the work arrays.
	call aclrr (Memr[wkobj], 2*fnpts)
	call aclrr (Memr[wkref], 2*fnpts)
	call aclrr (Memr[otempy], 2*fnpts)
	call aclrr (Memr[rtempy], 2*fnpts)

	# Prepare the data in the temp arrays before processing. First we'll
	# do the object spectrum
	if (OBJCONT(rv) == NO)
	    call amovr (OBJPIXY(rv,1), Memr[otempy], RV_NPTS(rv))
	else
	    call amovr (OCONT_DATA(rv,1), Memr[otempy], RV_NPTS(rv))

	if (RV_OW0(rv) == RV_GLOB_W1(rv) || RV_DCFLAG(rv) == -1)
	    ishift = 0
	else
	    ishift = nint ((RV_OW0(rv) - RV_GLOB_W1(rv)) / RV_OWPC(rv))

	call prep_spec (rv, RV_OSAMPLE(rv), npts, fnpts, RV_NPTS(rv), 
	    otempy, wkobj, ishift, YES)


	# Now do the template spectrum.
	if (REFCONT(rv) == NO)
	    call amovr (REFPIXY(rv,1), Memr[rtempy], RV_RNPTS(rv))
	else
	    call amovr (RCONT_DATA(rv,1), Memr[rtempy], RV_RNPTS(rv))

	if (RV_RW0(rv) == RV_GLOB_W1(rv) || RV_DCFLAG(rv) == -1)
	    ishift = 0
	else
	    ishift = nint ((RV_RW0(rv) - RV_GLOB_W1(rv)) / RV_RWPC(rv))

	call prep_spec (rv, RV_RSAMPLE(rv), npts, fnpts, RV_RNPTS(rv), 
	    rtempy, wkref, ishift, YES)


	# Normalize the correlation.
	call rv_normalize (Memr[wkobj], fnpts)
	call rv_normalize (Memr[wkref], fnpts)

	# Now do a plot of the prepared spectra if that what's requested.
	if (plot_only == YES) {
	    call gclear (RV_GP(rv))			# clear the screen

            if (RV_FILTER(rv) == OBJ_ONLY || RV_FILTER(rv) == BOTH) {
                call realft (Memr[wkobj], fnpts, 1)        # forward FFT
                call rv_filter (rv, Memr[wkobj], fnpts)    # Object
                call realft (Memr[wkobj], fnpts, -1)       # inverse FFT
            }
            if (RV_FILTER(rv) == TEMP_ONLY || RV_FILTER(rv) == BOTH) {
                call realft (Memr[wkref], fnpts, 1)        # forward FFT
                call rv_filter (rv, Memr[wkref], fnpts)    # Template
                call realft (Memr[wkref], fnpts, -1)       # inverse FFT
	    }

	    call split_plot (rv, RV_GP(rv), TOP, Memr[wkobj], fnpts, 
		OBJ_ONLY, PREPARED_PLOT)
	    call split_plot (rv, RV_GP(rv), BOTTOM, Memr[wkref], fnpts, 
		TEMP_ONLY, PREPARED_PLOT)

	    call sfree (sp)
	    return
	}

	# Call correlation routine to get answer vector.
	call rv_correl (rv, Memr[wkobj], Memr[wkref], fnpts, Memr[ans])

	# Load work arrays and fix wrap-around ordering of ans vector.
	do i = 1, fnpts {
	     WRKPIXY(rv,i) = Memr[ans+i-1]
	     WRKPIXX(rv,i) = real ((i-fnpts/2-1))
	}
	call fft_fixwrap (WRKPIXY(rv,1), fnpts)

	RV_Y1(rv) = INDEF				# Reset some plot flags
	RV_Y2(rv) = INDEF
	call sfree (sp)
end
