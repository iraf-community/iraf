include "rvpackage.h"
include "rvflags.h"
include "rvsample.h"

# RV_DATA_CHECK - Examine the data quickly to make sure that a proper
# correlation can be done.

int procedure rv_data_check (rv)

pointer	rv				# RV struct pointer

real	w1, w2
int	srange, erange
double	dex()
int	rv_chk_filter(), force_rebin()

begin
	# Check that both spectra have dispersion info
	if ((RV_OW0(rv) == 1. && RV_OWPC(rv) == 1.) && RV_DCFLAG(rv) != -1) {
	    RV_DCFLAG(rv) = -1
	    RV_RW0(rv) = 1.
	    RV_RWPC(rv) = 1.
	}
	if ((RV_RW0(rv) == 1. && RV_RWPC(rv) == 1.) && RV_DCFLAG(rv) != -1) {
	    RV_DCFLAG(rv) = -1
	    RV_OW0(rv) = 1.
	    RV_OWPC(rv) = 1.
	}

	# Check/reset dispersion info.
	if (force_rebin (rv) != OK) {
	    if (RV_INTERACTIVE(rv) == YES) {
		call rv_errmsg (
		    "Data cannot be put on same WPC dispersion.")
	    }
	    call rv_err_comment (rv,
		"Data cannot be put on same WPC dispersion.", "")
	    return (ERR_CORREL)
	}

	# Check the filter specifications
	switch (RV_FILTER(rv)) {
	case BOTH:
	    if (rv_chk_filter(rv,OBJECT_SPECTRUM) != OK || 
		rv_chk_filter(rv,REFER_SPECTRUM) != OK) {
		    if (RV_INTERACTIVE(rv) == YES)
		       call rv_errmsg ("Invalid or ambiguous filter specified.")
		    return (ERR_CORREL)
	    }
	case OBJ_ONLY:
	    if (rv_chk_filter(rv,OBJECT_SPECTRUM) != OK) {
		if (RV_INTERACTIVE(rv) == YES)
		    call rv_errmsg ("Invalid or ambiguous filter specified.")
		return (ERR_CORREL)
	    }
	case TEMP_ONLY:
	    if (rv_chk_filter(rv,REFER_SPECTRUM) != OK) {
		if (RV_INTERACTIVE(rv) == YES)
		    call rv_errmsg ("Invalid or ambiguous filter specified.")
		return (ERR_CORREL)
	    }
	}

        # Re-calculate the velocity dispersion and sundry
	if (RV_DCFLAG(rv) != -1)
            RV_DELTAV(rv) = RV_OWPC(rv) * CLN10
        RV_GLOB_W1(rv) = min (RV_OW0(rv), RV_RW0(rv))
        RV_GLOB_W2(rv) = max (RV_OW2(rv), RV_RW2(rv))

	# Check that ranges are in bounds, fix them if not
	if (ORCOUNT(rv) != ALL_SPECTRUM) {
	    if (ORUNITS(rv) == PIXELS || RV_DCFLAG(rv) == -1) {
	        srange = int (OSRANGE(rv,1))
	        erange = int (OERANGE(rv,ORCOUNT(rv)))
	        OSRANGE(rv,1) = max (srange, 1)
	        OSRANGE(rv,1) = min (srange, RV_NPTS(rv))
	        OERANGE(rv,ORCOUNT(rv)) = min (erange, RV_NPTS(rv))
	        OERANGE(rv,ORCOUNT(rv)) = max (erange, 1)
	    } else {
	        w1 = real (dex (RV_GLOB_W1(rv)))
	        w2 = real (dex (RV_GLOB_W2(rv)))
	        OSRANGE(rv,1) = max (OSRANGE(rv,1), w1)
	        OSRANGE(rv,1) = min (OSRANGE(rv,1), w2)
	        OERANGE(rv,ORCOUNT(rv)) = min (OERANGE(rv,ORCOUNT(rv)), w2)
	        OERANGE(rv,ORCOUNT(rv)) = max (OERANGE(rv,ORCOUNT(rv)), w1)
	    } 
	    if (OSRANGE(rv,1) == OERANGE(rv,ORCOUNT(rv))) {
	        if (RV_INTERACTIVE(rv) == YES) {
	            call rv_errmsg (
	    "Object sample range is out of bounds, using entire spectrum.")
	        }
	        call rv_err_comment (rv,
	    "Object `sample' range is out of bounds, using entire spectrum.",
		    "")
	        ORCOUNT(rv) = ALL_SPECTRUM
	    }
	}

	if (RRCOUNT(rv) != ALL_SPECTRUM) {
	    if (RRUNITS(rv) == PIXELS || RV_DCFLAG(rv) == -1) {
	        srange = int (RSRANGE(rv,1))
	        erange = int (RERANGE(rv,RRCOUNT(rv)))
	        RSRANGE(rv,1) = max (srange, 1)
	        RSRANGE(rv,1) = min (srange, RV_NPTS(rv))
	        RERANGE(rv,RRCOUNT(rv)) = min (erange, RV_NPTS(rv))
	        RERANGE(rv,RRCOUNT(rv)) = max (erange, 1)
	    } else {
	        w1 = real (dex (RV_GLOB_W1(rv)))
	        w2 = real (dex (RV_GLOB_W2(rv)))
	        RSRANGE(rv,1) = max (RSRANGE(rv,1), w1)
	        RSRANGE(rv,1) = min (RSRANGE(rv,1), w2)
	        RERANGE(rv,RRCOUNT(rv)) = min (RERANGE(rv,RRCOUNT(rv)), w2)
	        RERANGE(rv,RRCOUNT(rv)) = max (RERANGE(rv,RRCOUNT(rv)), w1)
	    } 
	    if (RSRANGE(rv,1) == RERANGE(rv,RRCOUNT(rv))) {
	        if (RV_INTERACTIVE(rv) == YES) {
	            call rv_errmsg (
	    "Temp sample range is out of bounds, using entire spectrum.")
	        }
	        call rv_err_comment (rv,
	    "Temp `sample' range is out of bounds, using entire spectrum.",
		    "")
	        RRCOUNT(rv) = ALL_SPECTRUM
	    }
	}
	return (OK)
end
