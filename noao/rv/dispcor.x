include	<error.h>
include	<imhdr.h>
include	<math/iminterp.h>

# DISPCOR -- Dispersion correct input spectrum to output spectrum.
# This procedure needs to invert the specified dispersion solutions
# and interpolate the input data, conserving flux if desired.  Image
# interpolation uses the image interpolation package and flux conservation
# integrates the interpolation function across the output pixel.

procedure dispcor (in, npts, out, w1, dw, nw, log, flux, interp)

real	in[npts]			#I Input spectrum
int	npts				#I Number of input pixels
real	out[nw]				#O Output spectrum
real	w1				#U Starting wavelength
real	dw				#U Wavelength per pixel
int	nw				#O Number of output pixels
bool	log				#I Log scale?
bool	flux				#I Conserve flux
int	interp				#I Rebinning interpolator type

int	i, j, i1, i2
real	a, b, asieval(), asigrl()
double	x, dx, xmin, xmax, xmin1, xmax1, y, dxdy, w, w1d, dwd
pointer	asi, temp
double	dc_eval()			# Dispersion function

include "dispcor.com"
define	NIT	10	# Max interations in determining dispersion inversion
define	DX	1.D-5	# Accuracy limit in pixels for dispersion inversion

begin
	# Determine approximate slope of dispersion function.
	xmin = 1
	xmax = npts
	xmin1 = xmin - 0.5
	xmax1 = xmax + 0.5
	w1d = w1
	dwd = dw
	x = dc_eval (xmin)
	y = dc_eval (xmax)
	if (log) {
	    i = (log10 (x) - w1d) / dwd + 1
	    j = (log10 (y) - w1d) / dwd + 1
	} else {
	    i = (x - w1d) / dwd + 1
	    j = (y - w1d) / dwd + 1
	}
	i1 = max (1, min (i, j) - 1)
	i2 = min (nw, max (i, j) + 1)
	dxdy = (xmax - xmin) / (y - x)

	# Initialize starting point.
	if (dwd * dxdy > 0.) {
	    y = x
	    x = 1
	} else
	    x = xmax

	# Get the image buffers, determine the interpolaion type, and
	# fit the interpolation function to the input spectrum.
	# Extend the interpolation by one pixel at each end.

	call malloc (temp, npts+2, TY_REAL)
	call amovr (in, Memr[temp+1], npts)
	Memr[temp] = in[1]
	Memr[temp+npts+1] = in[npts]

	call asiinit (asi, interp)			# V2.10 default
	call asifit (asi, Memr[temp], npts+2)

	call mfree (temp, TY_REAL)

	# If flux conserving determine edges of output pixels in input
	# spectrum and integrate using ASIGRL.  If not flux conserving
	# determine the center of each output pixel in the input spectrum
	# and evaluate the interpolation function with ASIEVAL.

	if (flux) {
	    # Determine initial left edge.
	    w = w1d + dwd * (i1 - 1.5)
	    if (log)
		w = 10. ** w
	    do j = 1, NIT {
	        x = max (xmin, min (xmax, x))
		y = dc_eval (x)
		dx = dxdy * (w - y)
		x = x + dx
	        if (abs (dx) < DX)
		    break
	    }
	    b = max (xmin1, min (xmax1, x)) + 1
	    do i = i1, i2 {
	        w = w1d + dwd * (i - 0.5)
	        if (log)
		    w = 10. ** w
	        do j = 1, NIT {
	            x = max (xmin, min (xmax, x))
		    y = dc_eval (x)
		    dx = dxdy * (w - y)
		    x = x + dx
	            if (abs (dx) < DX)
		        break
	        }
		a = b
	        b = max (xmin1, min (xmax1, x)) + 1
		if (a < b)
		    out[i] = asigrl (asi, a, b)
		else
		    out[i] = asigrl (asi, b, a)
	    }
	} else {
	    do i = i1, i2 {
	        w = w1d + dwd * (i - 1)
		if (log)
		    w = 10. ** w
	        do j = 1, NIT {
	            x = max (xmin, min (xmax, x))
		    y = dc_eval (x)
		    dx = dxdy * (w - y)
		    x = x + dx
	            if (abs (dx) < DX)
		        break
	        }

		if ((x >= xmin1) && (x <= xmax1)) {
		    a = x + 1
	            out[i] = asieval (asi, a)
		} else
	            out[i] = 0.
	    }
	}

	call asifree (asi)
end


# DC_DEFAULTS -- Given some set of wavelength scale with others undefined
# (INDEF) plus some defaults fill in the undefined parameters and make
# the wavelength scale consistent.  The logic of this task is complex
# and is meant to provide an "intelligent" result based on what users
# want.

procedure dc_defaults (a, b, n, w1, w2, dw, nw)

real	a			#I Default wavelength endpoint
real	b			#I Default wavelength endpoint
int	n			#I Default number of pixels
real	w1			#U Starting wavelength
real	w2			#U Ending wavelength
real	dw			#U Wavelength interval
int	nw			#U Number of pixels

int	nindef

begin
	# Determine how many input parameters are specfied.
	nindef = 0
	if (IS_INDEF(w1))
	    nindef = nindef + 1
	if (IS_INDEF(w2))
	    nindef = nindef + 1
	if (IS_INDEF(dw))
	    nindef = nindef + 1
	if (IS_INDEFI(nw))
	    nindef = nindef + 1

	# Depending on how many parameters are specified fill in the
	# INDEF parameters.

	switch (nindef) {
	case 0:
	    # All parameters specified.  First round NW to be consistent with
	    # w1, w2, and dw.  Then adjust w2 to nearest pixel.  It is possible
	    # that nw will be negative.  Checks for this should be made by the
	    # call in program.

	    nw = (w2 - w1) / dw + 1.5
	    w2 = w1 + dw * (nw - 1)
	case 1:
	    # Find the unspecified parameter and compute it from the other
	    # three specified parameters.  For nw need to adjust w2 to
	    # agree with a pixel.

	    if (IS_INDEF(w1))
		w1 = w2 - dw * (nw - 1)
	    if (IS_INDEF(w2))
		w2 = w1 + dw * (nw - 1)
	    if (IS_INDEF(dw))
		dw = (w2 - w1) / (nw - 1)
	    if (IS_INDEFI(nw)) {
	        nw = (w2 - w1) / dw + 1.5
		w2 = w1 + dw * (nw - 1)
	    }
	case 2:
	    # Fill in two unspecified parameters using the defaults.
	    # This is tricky.

	    if (IS_INDEF(dw)) {
		if (IS_INDEF(w1)) {
		    if (abs (w2 - a) > abs (w2 - b))
			w1 = a
		    else
			w1 = b
		    dw = (w2 - w1) / (nw - 1)
		} else if (IS_INDEF(w2)) {
		    if (abs (w1 - a) > abs (w1 - b))
			w2 = a
		    else
			w2 = b
		    dw = (w2 - w1) / (nw - 1)
		} else {
		    dw = (b - a) / n
		    nw = abs ((w2 - w1) / dw) + 1.5
		    dw = (w2 - w1) / (nw - 1)
		}
	    } else if (IS_INDEFI(nw)) {
		if (IS_INDEF(w1)) {
		    if (dw > 0.)
			w1 = min (a, b)
		    else
			w1 = max (a, b)
		    nw = (w2 - w1) / dw + 1.5
		    w1 = w2 - dw * (nw - 1)
		} else {
		    if (dw > 0.)
			w2 = max (a, b)
		    else
			w2 = min (a, b)
		    nw = (w2 - w1) / dw + 1.5
		    w2 = w1 + dw * (nw - 1)
		}
	    } else {
		if (dw > 0.)
		    w1 = min (a, b)
		else
		    w1 = max (a, b)
		w2 = w1 + dw * (nw - 1)
	    }
	case 3:
	    # Find the one specfied parameter and compute the others using
	    # the supplied defaults.

	    if (!IS_INDEF(w1)) {
		if (abs (w1 - a) > abs (w1 - b))
		    w2 = a
		else
		    w2 = b
		dw = (b - a) / n
		nw = abs ((w2 - w1) / dw) + 1.5
		dw = (w2 - w1) / (nw - 1)
	    } else if (!IS_INDEF(w2)) {
		if (abs (w2 - a) > abs (w2 - b))
		    w1 = a
		else
		    w1 = b
		dw = (b - a) / n
		nw = abs ((w2 - w1) / dw) + 1.5
		dw = (w2 - w1) / (nw - 1)
	    } else if (!IS_INDEFI(nw)) {
		w1 = min (a, b)
		w2 = max (a, b)
	        dw = (w2 - w1) / (nw - 1)
	    } else if (dw < 0.) {
		w1 = max (a, b)
		w2 = min (a, b)
		nw = (w2 - w1) / dw + 1.5
		w2 = w1 + dw * (nw - 1)
	    } else {
		w1 = min (a, b)
		w2 = max (a, b)
		nw = (w2 - w1) / dw + 1.5
		w2 = w1 + dw * (nw - 1)
	    }
	case 4:
	    # Given only defaults compute a wavelength scale.  The dispersion
	    # is kept close to the default.
	    w1 = min (a, b)
	    w2 = max (a, b)
	    dw = (b - a) / (n - 1)
	    nw = abs ((w2 - w1) / dw) + 1.5
	    dw = (w2 - w1) / (nw - 1)
	}
end


# DC_EVAL -- Evaluate the dispersion function.

double procedure dc_eval (x)

double	x				#I Pixel coordinate

double	w				# Wavelength

double	dcveval()

include "dispcor.com"

begin
	if (dcflag == -1) {
	    w = shift1 + dcveval (cv1, x)
	    if (wt2 != 0.)
	        w = wt1 * w + wt2 * (shift2 + dcveval (cv2, x))
	} else if (dcflag == 0) {
	    w = crval + cdelt * (x - 1)
	} else {
	    w = 10. ** (crval + cdelt * (x - 1))
	}

	return (w)
end


# DC_SETEVAL -- Set dispersion parameters.

procedure dc_seteval (w0, wpc, flag)

real	w0				#I Starting wavelength
real	wpc				#I Wavelength per pixel
int	flag				#I Dispersion flag

include "dispcor.com"

begin
	crval = w0
	cdelt = wpc
	dcflag = flag
end
