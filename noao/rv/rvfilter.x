include <math.h>
include "rvpackage.h"
include "rvflags.h"
include "rvfilter.h"

# RV_FILTER - Filter the FFT data before doing cross correlation.
# the value of N represents the number of complex fourier components
# so we must filter 2*N points for the real and complex elements of
# each component.

procedure rv_filter (rv, fft, N)

pointer	rv				#I RV struct pointer
real	fft[ARB]			#U FFT data array
int	N				#I no. elements in fft[]

int	i, j, npts
int	cuton, cutoff, fullon, fulloff
real	fraction, rv_hanning(), rv_welch()

begin
	cuton = RVF_CUTON(rv)
	cutoff = RVF_CUTOFF(rv)
	fullon = RVF_FULLON(rv)
	fulloff = RVF_FULLOFF(rv)

	switch (RVF_FILTTYPE(rv)) {
	case SQUARE:				# Step function
	    j = 1
	    do i = 1, cuton {
		fft[j] = 0.0			# real
		fft[j+1] = 0.0			# complex
		j = j + 2
	    }
	    j = cutoff * 2 + 1
	    do i = cutoff, N {
		fft[j] = 0.0			# real
		fft[j+1] = 0.0			# complex
		j = j + 2
	    }

	case RAMP:				# Ramp filter
	    j = 1
	    do i = 1, N {
	        if (i < cuton || i > fulloff) {
	             fft[j] = 0.0		# real
	             fft[j+1] = 0.0		# complex
		}
	        if (i >= cuton && i <= fullon) {
	    	     fraction = real(i-cuton) /real(fullon-cuton)
	    	     fft[j] = fft[j] * fraction
	    	     fft[j+1] = fft[j+1] * fraction
	        }
	        if (i >= cutoff && i <= fulloff) {
	    	     fraction = 1.0 - real(i-cutoff)/real(fulloff-cutoff)
	    	     fft[j] = fft[j] * fraction
	    	     fft[j+1] = fft[j+1] * fraction
	        }
		j = j + 2
	    }

	case WELCH:
	    npts = cutoff - cuton + 1
	    j = 1
	    do i = 1, N {
	        if (i < cuton || i > cutoff) {
	             fft[j] = 0.0
	             fft[j+1] = 0.0
		} else {
		     #fraction = rv_welch (i-cuton+1, npts)
		     fraction = rv_welch (i-cuton, npts)
		     fft[j] = fft[j] * fraction
		     fft[j+1] = fft[j+1] * fraction
		}
		j = j + 2
	    }

	case HANNING:
	    npts = cutoff - cuton + 1
	    j = 1
	    do i = 1, N {
	        if (i < cuton || i > cutoff) {
	             fft[j] = 0.0
	             fft[j+1] = 0.0
		} else {
		     #fraction = rv_hanning (i-cuton+1, npts)
		     fraction = rv_hanning (i-cuton, npts)
		     fft[j] = fft[j] * fraction
		     fft[j+1] = fft[j+1] * fraction
		}
		j = j + 2
	    }
	} 				# End switch
end


# RV_CHK_FILTER - Check the filter specification for a sensible filter

int procedure rv_chk_filter (rv, which)

pointer	rv					#I RV struct pointer
int	which					#I Which spectrum to filter

int	con, coff, fon, foff

begin
	con = RVF_CUTON(rv)
	coff = RVF_CUTOFF(rv)
	fon = RVF_FULLON(rv)
	foff = RVF_FULLOFF(rv)

	# Here we get the filter checks.
	if (RV_FILTER(rv) == BOTH ||
	    (which == OBJECT_SPECTRUM && RV_FILTER(rv) == OBJ_ONLY) ||
	    (which == REFER_SPECTRUM && RV_FILTER(rv) == TEMP_ONLY)) {

		# Check for defaults.
		if (con == 0 && coff == 0 && fon == 0 && foff == 0)
		    return (ERR)

		# Check for backward filters.
	        if (RVF_FILTTYPE(rv) == RAMP) {
		    if (coff < con || fon < con || foff < coff || foff < fon ||
		        con > coff || fon > coff || coff > foff || fon > foff)
			    return (ERR)
	        } else {
		    if (coff < con)
		        return (ERR)
	        }
	} else
	    return (ERR)

	return (OK)
end


# RV_DO_FILTER - Given an arbitrary input spectrum, do the filtering and return 
# the filtered spectrum after plotting the filtered data.

procedure rv_do_filter (rv, ssp, vec, npts, filt, fnpts, do_plot)

pointer	rv				#I RV struct pointer
pointer	ssp				#I Sample struct pointer
real	vec[ARB]			#I Prepared input spectrum
int	npts				#I Npts in array
real	filt[ARB]			#O Output Filtered spectrum
int	fnpts				#I Npts in array
int	do_plot				#I Draw a plot?

pointer	sp, data, prep
int	i, iremain
real	dc
real	rv_avgpix()

define	REVERSE		1

begin
	# Allocate some space
	call smark (sp)
	call salloc (data, 2*fnpts, TY_REAL)
	call salloc (prep, 2*fnpts, TY_REAL)

	# Now prepare the vector
	call amovr (vec, Memr[data], npts)
	dc = rv_avgpix (Memr[data], npts)
	call prep_spec (rv, ssp, npts, fnpts, npts, data, prep, 0, NO)

	# Do the filtering
	call afftrx (Memr[prep], Memr[prep], fnpts)   # Do forward tform
	call rv_filter (rv, Memr[prep], fnpts/2)      # Do the filtering
	call aiftrx (Memr[prep], Memr[prep], fnpts)   # Inverse transform gives 
						      #   filtered spectrum

	# Remove the centering effects, inverse apodize, and rebias
	if (fnpts != npts) {
	    iremain = (fnpts - npts) / 2
	    do i = 1, npts
		filt[i] = Memr[prep+i+iremain-1] + dc
	} else 
	    call aaddkr (Memr[prep], dc, filt, npts)

	# Now do the plot to the screen of the filtered data
	if (do_plot == YES)
	    call fft_fltplot (rv, RV_GP(rv), filt, npts)

	call sfree (sp)
end


# RV_HANNING - Compute a Hanning window.

real procedure rv_hanning (j, N)

int	j				#I Point in array
int	N				#I Npts over which window extends

real	hval

begin
	hval = 0.5 * (1.0 - cos (double(TWOPI*j) / double (N-1)) )
	if (hval < 0.0)
	    hval = 0.0
	return (hval)
end


# RV_WELCH - Compute a Welch window.

real procedure rv_welch (j, N)

int	j				#I Point in array
int	N				#I Npts over which window extends

real	wval

begin
	wval = 1.0 - ( (j - (0.5*(N-1))) / (0.5*(N+1)) )**2
	if (wval < 0.0)
	    wval = 0.0
	return (wval)
end
