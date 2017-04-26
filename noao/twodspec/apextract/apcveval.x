include	<math/curfit.h>

# AP_CVEVAL -- Interface to CVEVAL that avoids extrapolation.
# This is necessary because if the tracing was truncated due to loss
# of the profile the trace limits will be smaller than the image axis.
# In the longer term the aperture limits along the dispersion should be
# used to limit the extent of the spectrum.

real procedure ap_cveval (cv, x)

pointer	cv		#I CURFIT pointer
real	x		#I Point to be evaluated.

real	x1, cvstatr(), cveval()

begin
	x1 = min (max (x, cvstatr(cv,CVXMIN)), cvstatr(cv,CVXMAX))
	return (cveval (cv, x1))
end
