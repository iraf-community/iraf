include <math.h>
include "rvpackage.h"
include "rvflags.h"
include "rvsample.h"

# PREP_SPEC - Prepare the input spectra prior to a Fourier transform.  Data
# are centered in the transform array according to the number of points covered
# by both spectra.  Apply the DC bias and the cosine bell.

procedure prep_spec (rv, ssp, npts, fftnpts, datanpts, data, tform, ishift, 
    apodize)

pointer	rv				#I RV struct pointer
pointer	ssp				#I Sample struct pointer
int	npts				#I npts in global data array
int	fftnpts				#I Output size of prepared array
int	datanpts			#I npts in original data array
pointer	data				#I pointer to original temp space
pointer	tform				#I Prepared transform work space
int	ishift				#I Shift of spectrum
int	apodize				#I Apodize the data?

pointer sp, tp
int	i, iremain, num
real	dc, rv_avgpix()

define	FORWARD			-1

begin
	call smark (sp)
	call salloc (tp, fftnpts, TY_REAL)

	# Do some error checking.
	if (data == NULL || tform == NULL) 
	    call error (0, "NULL pointer passed to prep_spec()")
	else if (ishift < 0)
	    call error (0, "Negative shift found preparing spectrum.")

	if (DEBUG(rv)) {
	    call d_printf (DBG_FD(rv), "prep_spec:\t")
	    call d_printf (DBG_FD(rv), "ishift=%d np=%d dnp=%d fnp=%d\n")
		call pargi (ishift); call pargi(npts)
		call pargi (datanpts); call pargi(fftnpts)
	}

	# Get things started.
	call aclrr (Memr[tform], fftnpts)
	#call amovr (Memr[data], Memr[tform+ishift], npts)
	call amovr (Memr[data], Memr[tform+ishift], datanpts)

	# Generate size dependent parameters.
	if (SR_COUNT(ssp) != ALL_SPECTRUM && apodize == YES) {
	    #call prep_samples (rv, ssp, Memr[tform+ishift], npts, apodize)
	    call prep_samples (rv, ssp, Memr[tform], npts, apodize)
	} else {
	    num = datanpts
	    if (apodize == YES) {
	        call fft_cosbel (Memr[tform+ishift], num, FORWARD, 
		    RV_APODIZE(rv))
	    }
	}

	# Center the data in the transform arrays.
	if (fftnpts != npts) {
	    call aclrr (Memr[tp], fftnpts)
	    call amovr (Memr[tform], Memr[tp], npts)
	    call aclrr (Memr[tform], fftnpts)
	    iremain = (fftnpts-npts) / 2
	    do i = 1,npts 				# get centered
	        Memr[tform+iremain+i-1] = Memr[tp+i-1]
	} else
	    iremain = 0

	# Now compute the bias to raise zero level to spectrum mean.
	dc = rv_avgpix (Memr[tform], fftnpts)		  # compute DC level
	call asubkr (Memr[tform], dc, Memr[tform], fftnpts) # apply DC bias

	call sfree (sp)
end


# PREP_SAMPLES - Prepare the data by masking according to the samples and
# apodizing each region.

procedure prep_samples (rv, ssp, data, npts, apodize)

pointer	rv					#I RV struct pointer
pointer	ssp					#I Sample struct pointer
real	data[ARB]				#U Data array
int	npts					#I Npts in array
int	apodize					#I Apodize the data?

int	i, j, k, np, left, right

begin
	left = 1
	if (SR_UNITS(ssp) == PIXELS) {
	    #right = nint (SRANGE(ssp,1) - RV_GLOB_W1(rv) + 1)
	    right = SRANGE(ssp,1) - RV_GLOB_W1(rv) + 1
	} else {
	    right = nint ((log10(SRANGE(ssp,1)) - RV_GLOB_W1(rv)) / 
		RV_OWPC(rv)) + 1
	}
	do i = 1, SR_COUNT(ssp)+1 {
	    if (DEBUG(rv)) {
		call d_printf (DBG_FD(rv), "\tl=%g r=%g -- s=%g e=%g\n")
		    call pargi (left) ; call pargi (right)
		    call pargr (SRANGE(ssp,i)) ; call pargR (ERANGE(ssp,i))
	    }

	    k = 1
	    j = left
	    np = right - left + 1
	    while (k <= np) {
	        data[j] = 0.0
	        j = j + 1
		k = k + 1
	    }
	    if (i == SR_COUNT(ssp)+1) 
	    	return

	    if (SR_UNITS(ssp) == PIXELS) {
	        #left = nint ((ERANGE(ssp,i) - RV_GLOB_W1(rv) + 1) + 1)
	        left = (ERANGE(ssp,i) - RV_GLOB_W1(rv) + 1) + 1
	    } else {
	        left = nint ((log10(ERANGE(ssp,i)) - RV_GLOB_W1(rv)) / 
		    RV_OWPC(rv)) + 1
	    }
	    if (apodize == YES)
	        call fft_cosbel (data[right], left-right+1, -1, RV_APODIZE(rv))
	    if (i == SR_COUNT(ssp)) {
		right = npts
	    } else {
		if (SR_UNITS(ssp) == PIXELS) {
	    	    right = SRANGE(ssp,i+1) - RV_GLOB_W1(rv) + 1
		} else {
	    	    right = nint ((log10(SRANGE(ssp,i+1)) - RV_GLOB_W1(rv)) / 
			RV_OWPC(rv)) + 1
		}
	    }
	}
end
