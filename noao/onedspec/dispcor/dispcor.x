include	<math/iminterp.h>

# DISPCOR -- Dispersion correct input spectrum to output spectrum.
# This procedure uses the MWCS forward and inverse transformations
# and interpolate the input data, conserving flux if desired.  Image
# interpolation uses the image interpolation package and flux conservation
# integrates the interpolation function across the output pixel.  This
# procedure does some CLIO to get the interpolation function and to
# query whether to conserve flux.

procedure dispcor (cti, cto, line, in, npts, out, nw, flux)

pointer	cti			#I MWCS input inverse transformation
pointer	cto			#I MWCS output forward transformation
int	line			#I Spectrum line
real	in[npts]		#I Input spectrum
int	npts			#I Number of input pixels
real	out[nw]			#O Output spectrum
int	nw			#I Number of output pixels
bool	flux			#I Conserve flux

char	interp[10]
bool	ofb_a, ofb_b
int	i, j, ia, ib, clgwrd()
real	a, b, sum, asieval(), asigrl()
double	x, xmin, xmax, w, y1, y2
pointer	asi, temp

begin
	# Get the image buffers fit the interpolation function to the
	# input spectrum.  Extend the interpolation by one pixel at each end.

	call malloc (temp, npts+2, TY_REAL)
	call amovr (in, Memr[temp+1], npts)
	Memr[temp] = in[1]
	Memr[temp+npts+1] = in[npts]

	call asiinit (asi, clgwrd ("interp", interp, 10, II_FUNCTIONS))
	call asifit (asi, Memr[temp], npts+2)

	call mfree (temp, TY_REAL)

	# Determine edges of output pixels in input spectrum and integrate
	# using ASIGRL.  If not flux conserving take the average.

	xmin = 0.5
	xmax = npts + 0.5
	y1 = line

	x = 0.5
	call smw_c2trand (cto, x, y1, w, y2)
	call smw_c2trand (cti, w, y2, x, y1)
	ofb_b = (x < xmin || x > xmax)
	b = max (xmin, min (xmax, x)) + 1
	do i = 1, nw {
	    ofb_a = ofb_b
	    a = b
	    x = i + 0.5
	    call smw_c2trand (cto, x, y1, w, y2)
	    call smw_c2trand (cti, w, y2, x, y1)
	    ofb_b = (x < xmin || x > xmax)
	    b = max (xmin, min (xmax, x)) + 1
	    if (ofb_a && ofb_b)
		out[i] = 0.
	    else if (a <= b) {
		ia = nint (a + 0.5)
		ib = nint (b - 0.5)
		if (abs (a+0.5-ia) < 0.00001 && abs (b-0.5-ib) < 0.00001) {
		    sum = 0.
		    do j = ia, ib
			sum = sum + asieval (asi, real(j))
		    out[i] = sum
		} else
		    out[i] = asigrl (asi, a, b)
		if (!flux)
		    out[i] = out[i] / max (b - a, 1e-4)
	    } else {
		ib = nint (b + 0.5)
		ia = nint (a - 0.5)
		if (abs (a-0.5-ia) < 0.00001 && abs (b+0.5-ib) < 0.00001) {
		    sum = 0.
		    do j = ib, ia
			sum = sum + asieval (asi, real(j))
		    out[i] = sum
		} else
		    out[i] = asigrl (asi, b, a)
		if (!flux)
		    out[i] = out[i] / max (a - b, 1e-4)
	    }
	}

	call asifree (asi)
end
