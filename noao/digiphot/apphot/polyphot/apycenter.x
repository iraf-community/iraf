include "../lib/center.h"
include "../lib/polyphot.h"

# AP_YCENTER -- Center the polygon using the centering package routines.

int procedure ap_ycenter (py, im, wx, wy, x, y, nver)

pointer	py		# pointer to polyphot structure
pointer	im		# pointer to the IRAF image
real	wx		# initial x center
real	wy		# initial y center
real	x[ARB]		# x coordinates of the polygon
real	y[ARB]		# y coordinates of the polygon
int	nver		# number of vertices

int	cier
int	apfitcenter()
real	apstatr()

begin
	if (IS_INDEFR(apstatr (py, PYCX)) || IS_INDEFR(apstatr (py, PYCY)))
	    cier = apfitcenter (py, im, INDEFR, INDEFR)
	else
	    cier = apfitcenter (py, im, wx, wy)

	if (! IS_INDEFR (apstatr (py, XCENTER)) && ! IS_INDEFR (apstatr (py,
	    YCENTER)) && ! IS_INDEFR (apstatr (py, PYCX)) &&
	    ! IS_INDEFR (apstatr (py, PYCY)))
	    call ap_yshift (py, im, x, y, nver, apstatr (py, XCENTER),
	        apstatr (py, YCENTER))

	return (cier)
end


# AP_YRECENTER -- Recenter the polygon using the existing buffer of pixels
# and the centering package routines.

int procedure ap_yrecenter (py, im, x, y, nver, cier)

pointer	py		# pointer to polyphot structure
pointer	im		# the input image descriptor
real	x[ARB]		# x coordinates of the polygon
real	y[ARB]		# y coordinates of the polygon
int	nver		# number of vertices
int	cier		# original centering error

int	aprefitcenter()
real	apstatr()

begin
	cier = aprefitcenter (py, im, cier)
	if (! IS_INDEFR (apstatr (py, XCENTER)) && ! IS_INDEFR (apstatr (py,
	    YCENTER)) && ! IS_INDEFR (apstatr (py, PYCX)) && !
	    IS_INDEFR (apstatr (py, PYCY)))
	    call ap_yshift (py, im, x, y, nver, apstatr (py, XCENTER),
	        apstatr (py, YCENTER))
	return (cier)
end
