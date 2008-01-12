include <math.h>
include "rvpackage.h"
include "rvflags.h"
include "rvfilter.h"

# RV_CORREL -  Computes the correlation of two real data sets DATA1 and DATA2,
# each of length N including any user supplied zero padding.  N must be an
# integer power of two.  The answer is returned as the first N points in ANS
# stored in wraparound order, i.e. correlations at increasingly negative lags
# are in ANS(N) down to ANS(N/2+1), while correlations at increasingly pos-
# itive lags are in ANS(1) (zero lag) on up to ANS(N/2).  Note that ANS must
# be supplied in the calling program with length at least 2*N since it is also
# used as a working space.  Sign convention of this routine, if DATA1 lags 
# DATA2, i.e. is shifted to the right of it, then ANS will show a peak at 
# positive lags.
# Referece:  Numerical Recipes in C, ch 12, Press, et al.

procedure rv_correl (rv, data1, data2, npts, ans)

pointer	rv				#I RV struct pointer
real	data1[ARB]			#I Object intensity array
real	data2[ARB]			#I Template intensity array
int	npts				#I Size of the array
real	ans[ARB]			#O Output correlation array

pointer	sp, fft
real	dum
int	i, no2

begin
	call smark (sp)
	call salloc (fft, 2*npts, TY_REAL)

	# Transform both data vectors at once
	call twofft (data1, data2, Memr[fft], ans, npts)	

	# Filter the data if asked for
	if (RV_FILTER(rv) == OBJ_ONLY || RV_FILTER(rv) == BOTH)
	    call rv_filter (rv, Memr[fft], npts/2) 	# Object
	if (RV_FILTER(rv) == TEMP_ONLY || RV_FILTER(rv) == BOTH)
	    call rv_filter (rv, ans, npts/2) 		# Template

	no2 = npts / 2
	for(i=2; i<=npts+2; i = i + 2) {
	    dum = ans[i-1]		# Multiply to find FFT of correlation
	    ans[i-1] = (Memr[fft+i-2] * dum + Memr[fft+i-1] * ans[i]) / no2
	    ans[i] = (Memr[fft+i-1] * dum - Memr[fft+i-2] * ans[i]) / no2
	}

	ans[2] = ans[npts+1] 		    # Pack first/last into one element

	call realft (ans, no2, -1)	    # Inverse transform gives corr.

	# Normalize by the number of points
	call adivkr (ans, real(npts), ans, npts) 

	call sfree (sp)
end


# RV_ANTISYM -- Compute antisymmetric part of correlation function.

procedure rv_antisym (rv, x, h, width, cor, cnpts, anti, sigmaa, err, r)

pointer	rv				#I RV struct pointer
real	x				#I Peak position
real	h				#I Peak height
real	width				#I Peak width
real	cor[cnpts]			#I Correlation function
int	cnpts				#I Number of correlation points
real	anti[cnpts]			#O Array of antisymmetric function
real	sigmaa				#O Sigma of antisymmetric function
double	err				#O Velocity error estimate
real	r				#O Tonry&Davis R value

int	i, j, ix
real	eps
real	sqrt(), assqr()

begin
	if (DBG_OTHER(rv) == 0) {
	    ix = x + (cnpts/2. + 1) + 0.5 
	    do i = 1, cnpts {
	        j = 1 + mod ((2*(cnpts+ix)-i-1), cnpts)
	        if (j <= cnpts && j >= 1)
	            anti[i] = cor[i] - cor[j]
	        else
	            anti[i] = 0.0
	    }

	    # This is the sigma(a) in the Tonry&Davis paper (Eqn. 20)
	    sigmaa = sqrt (assqr(anti,cnpts) / (2*cnpts))

	} else if (DBG_OTHER(rv) > 0) {
	    ix = x + 0.5
	    do i = 1, cnpts {
	        j = 1 + mod ((2*(cnpts+ix)-i-1), cnpts)
	        if (j <= cnpts && j >= 1)
	            anti[i] = (cor[i] - cor[j])
	        else
	            anti[i] = 0.0
	    }

	    # This is the sigma(a) in the Tonry&Davis paper (Eqn. 20)
	    sigmaa = sqrt (assqr(anti,cnpts) / (2*cnpts))

	} else if (DBG_OTHER(rv) == -1) {
	    ix = x + 0.5
	    do i = 1, cnpts {
	        j = 1 + mod ((2*(cnpts+ix)-i), cnpts)
	        if (j <= cnpts && j >= 1)
	            anti[i] = (cor[i] - cor[j])
	        else
	            anti[i] = 0.0
	    }

	    # This is the sigma(a) in the Tonry&Davis paper (Eqn. 20)
	    sigmaa = sqrt (assqr(anti,cnpts) / (2*cnpts))

	}
	
	# This is the ratio of true peak height to height of average peak in CCF
	r = h / (SQRTOF2 * sigmaa)				# Eqn. 23

	eps = (TWOPI * width) / 8.0 / (1. + r)			# Eqn. 24
	if (RV_DCFLAG(rv) != -1)
	    err = eps * RV_DELTAV(rv)				# Error est.
	else
	    err = eps

	if (DBG_DEBUG(rv) == YES) {
	    call d_printf (DBG_FD(rv), "rv_antisym:\n\t")
	    call d_printf (DBG_FD(rv), 
		     "ix=%d x=%g sig=%g w=%g h=%g R=%g eps=%g => %g k/s\n")
		call pargi (ix) ; call pargr(x) ; call pargr(sigmaa)
		call pargr(width) ; call pargr(h) ; call pargr(r)
		call pargr(eps) ; call pargr(RV_DELTAV(rv))
	}
end


# RV_NORMALIZE -- Normalize data for correlation by dividing by the rms
# of the data.

procedure rv_normalize (data, npts)

real	data[ARB]		#U Data
int	npts			#I Number of points

real	rms, assqr()

begin
	rms = sqrt (assqr(data, npts) / real(npts) )
	if (rms != 0.0)
	    call adivkr (data, rms, data, npts)
end
