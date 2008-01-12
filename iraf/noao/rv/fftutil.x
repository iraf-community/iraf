include <math.h>
include "rvpackage.h"
include "rvflags.h"
include "rvplots.h"

# GET_FFT - Take the input real array and return the absolute value of it's 
# DFT.  The array rfft must be sized to at least to the power of two greater 
# than npts.

procedure get_fft (rv, rinpt, npts, rfft, fnpts)

pointer	rv				#I RV struct pointer
real	rinpt[ARB]			#I Input real array
int	npts				#I No. pts in rinpt
real	rfft[ARB]			#O Output abs value of DFT
int	fnpts				#O No. pts in output array.

pointer	sp, tp, cpr, cpi, fft
int	i, last, ishift
real	xtmp, cx_abs()

begin
	fnpts = RV_FFTNPTS (rv)			# Get FFT size

	call smark (sp)
	call salloc (tp, fnpts, TY_REAL)	# Allocate temp vector
	call salloc (cpr, fnpts, TY_REAL)	# Allocate temp vector
	call salloc (cpi, fnpts, TY_REAL)
	call salloc (fft, 2*fnpts, TY_REAL)
	call aclrr (Memr[tp], fnpts)
	call aclrr (Memr[cpr], fnpts)
	call aclrr (Memr[cpi], fnpts)
	call amovr (rinpt, Memr[tp], npts)
	
	# Do forward transform
	ishift = 0
	if (RV_WHERE(rv) == TOP) {
	    call prep_spec (rv, RV_OSAMPLE(rv), npts, fnpts, RV_NPTS(rv), 
		tp, cpr, ishift, YES)
	} else {
	    call prep_spec (rv, RV_RSAMPLE(rv), npts, fnpts, RV_RNPTS(rv), 
		tp, cpr, ishift, YES)
	}
	call afftrr (Memr[cpr], Memr[cpi], Memr[cpr], Memr[cpi], fnpts)	
	if (RVP_WHEN(rv) == AFTER) {
	    if (RV_WHERE(rv) == TOP) {
		if (RV_FILTER(rv) == OBJ_ONLY || RV_FILTER(rv) == BOTH) {
	            call cx_pak (Memr[cpr], Memr[cpi], Memr[fft], fnpts/2)
	            call rv_filter (rv, Memr[fft], fnpts/2)
	            call cx_unpak (Memr[fft], Memr[cpr], Memr[cpi], fnpts)
		}
	    } else {
		if (RV_FILTER(rv) == TEMP_ONLY || RV_FILTER(rv) == BOTH) {
	            call cx_pak (Memr[cpr], Memr[cpi], Memr[fft], fnpts/2)
	            call rv_filter (rv, Memr[fft], fnpts/2)
	            call cx_unpak (Memr[fft], Memr[cpr], Memr[cpi], fnpts)
		}
	    }
	}

	# Get the absolute value of the transform
	last = fnpts / 2 + 1
	do i = 2, last {
	    xtmp = cx_abs (Memr[cpr+i-1], Memr[cpi+i-1])
	    if (RVP_LOG_SCALE(rv) == YES) {
	        if (xtmp != 0.0) 		# Divide by zero check in log
	            rfft[i-1] = log10 (xtmp)
		else 
		    rfft[i-1] = 0.0
	    } else 
		rfft[i-1] = xtmp
	}

	call sfree (sp)
end


# GET_PSPEC - Take the input real array and return the log of the power 
# spectrum.  The array pspec must be sized to at least to the power of two 
# greater than npts.

procedure get_pspec (rv, rinpt, npts, pspec, fnpts)

pointer	rv				#I RV struct pointer
real	rinpt[ARB]			#I Input real array
int	npts				#I No. pts in rinpt
real	pspec[ARB]			#O Output abs value of DFT
int	fnpts				#O No. pts in output array.

pointer	sp, tp, cpr, cpi, fft
int	i, j, ishift
real	xtmp

begin
	fnpts = RV_FFTNPTS (rv)			# Get FFT size

	call smark (sp)
	call salloc (tp, fnpts, TY_REAL)	# Allocate temp vector
	call salloc (cpr, fnpts, TY_REAL)	# Allocate temp vector
	call salloc (cpi, fnpts, TY_REAL)	# Allocate temp vector
	call salloc (fft, 2*fnpts, TY_REAL)	# Allocate temp vector
	call aclrr (Memr[tp], fnpts)
	call aclrr (Memr[cpr], fnpts)
	call aclrr (Memr[cpi], fnpts)
	call amovr (rinpt, Memr[tp], npts)
	
	# Do forward transform
	ishift = 0
	if (RV_WHERE(rv) == TOP) {
	    call prep_spec (rv, RV_OSAMPLE(rv), npts, fnpts, RV_NPTS(rv), 
		tp, cpr, ishift, YES)
	} else {
	    call prep_spec (rv, RV_RSAMPLE(rv), npts, fnpts, RV_RNPTS(rv), 
		tp, cpr, ishift, YES)
	}
	call afftrr (Memr[cpr], Memr[cpi], Memr[cpr], Memr[cpi], fnpts)	
	if (RVP_WHEN(rv) == AFTER) {
	    if (RV_WHERE(rv) == TOP) {
		if (RV_FILTER(rv) == OBJ_ONLY || RV_FILTER(rv) == BOTH) {
	            call cx_pak (Memr[cpr], Memr[cpi], Memr[fft], fnpts/2)
	            call rv_filter (rv, Memr[fft], fnpts/2)
	            call cx_unpak (Memr[fft], Memr[cpr], Memr[cpi], fnpts)
		}
	    } else {
		if (RV_FILTER(rv) == TEMP_ONLY || RV_FILTER(rv) == BOTH) {
	            call cx_pak (Memr[cpr], Memr[cpi], Memr[fft], fnpts/2)
	            call rv_filter (rv, Memr[fft], fnpts/2)
	            call cx_unpak (Memr[fft], Memr[cpr], Memr[cpi], fnpts)
		}
	    }
        } 

	# Get the power spectrum
	j = fnpts / 2 + 1
	do i = 2, j {
	    xtmp = (Memr[cpr+i-1]*Memr[cpr+i-1]) + (Memr[cpi+i-1]*Memr[cpi+i-1])
	    if (RVP_LOG_SCALE(rv) == YES) {
		if (xtmp != 0.0)
	            pspec[i-1] = log10 (xtmp)
		else
		    pspec[i-1] = 0.0 
	     } else 
		pspec[i-1] = xtmp
	}

	call sfree (sp)
end


# FFT_COSBEL - Apply a cosine bell to the data.

procedure fft_cosbel (v, npts, code, percent)

real	v[ARB]				#U Data vector
int	npts				#I Number of data points
int 	code				#I Direction code 
					     # <0 for forward,  >=0 for reverse
real	percent				#I percent of end to mask

int 	ndpercent, index, i
real	f

begin
	if (IS_INDEF(percent))
	    return

	ndpercent = int (npts * percent) 	# Compute no. of endpoints

	if (code < 0) {				# Forward application of window
	    do i = 1,ndpercent {
	        f = (1.0 - cos(PI*real(i-1)/real(ndpercent))) / 2.0
	        index = npts - i + 1
	        v[i] =  f * v[i]
	        v[index] =  f * v[index]
	    }
	} else if (code >= 0) {			# Reverse application of window
	    do i = 1,ndpercent {
	        f = (1.0 - cos(PI*real(i-1)/real(ndpercent))) / 2.0
	        if (abs(f) < 1.0e-3) 
		    f = 1.0e-3
	        index = npts - i + 1
	        v[i] = v[i] / f
	        v[index] = v[index] / f
	    }
	}
end


# FFT_FIXWRAP - Fix the wrap around ordering that results from the Numerical
# Recipes FFT routines.  Re-ordering is done such that points 1 to N/2 are
# switched in the array with points N/2+1 to N.  Re-ordering is done in-place.

procedure fft_fixwrap (v, npts)

real	v[ARB]				#U Data array in wrap around order
int	npts				#I length of data array

real 	temp
int 	i

begin
	do i = 1, npts/2 {
	    temp = v[i]
	    v[i] = v[i+npts/2]
	    v[i+npts/2] = temp
	}
end


# FFT_POW2 - Find the power of two that is greater than N.
# Returns INDEFI if a power can't be found less than 2^15.

int procedure fft_pow2 (N)
int	N	    	    		#I Number of data points

int 	i, nn

begin
	nn = 0
	do i = 1, 31 {
	    nn = 2 ** i
	    if (nn >= N) 	    	# return 2**i > N
		return (nn)
	}

	return (INDEFI)
end
