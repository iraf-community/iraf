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

int	i, j, ia, ib, onedinterp()
real	a, b, sum, asieval(), asigrl()
double	x, xmin, xmax, w, y1, y2
pointer	asi, temp
errchk	onedinterp

begin
	# Get the image buffers fit the interpolation function to the
	# input spectrum.  Extend the interpolation by one pixel at each end.

	call malloc (temp, npts+2, TY_REAL)
	call amovr (in, Memr[temp+1], npts)
	Memr[temp] = in[1]
	Memr[temp+npts+1] = in[npts]

	call asiinit (asi, onedinterp())
	call asifit (asi, Memr[temp], npts+2)

	call mfree (temp, TY_REAL)

	# If flux conserving determine edges of output pixels in input
	# spectrum and integrate using ASIGRL.  If not flux conserving
	# determine the center of each output pixel in the input spectrum
	# and evaluate the interpolation function with ASIEVAL.

	xmin = 0.5
	xmax = npts + 0.5
	y1 = line
	if (flux) {
	    # Determine initial left edge.
	    x = 0.5
	    call mw_c2trand (cto, x, y1, w, y2)
	    call mw_c2trand (cti, w, y2, x, y1)
	    b = max (xmin, min (xmax, x)) + 1
	    do i = 1, nw {
		x = i + 0.5
		call mw_c2trand (cto, x, y1, w, y2)
		call mw_c2trand (cti, w, y2, x, y1)
		a = b
	        b = max (xmin, min (xmax, x)) + 1
		if (a <= b) {
		    ia = nint (a + 0.5)
		    ib = nint (b - 0.5)
		    if (abs (a+0.5-ia) < .001 && abs (b-0.5-ib) < .001) {
			sum = 0.
			do j = ia, ib
			    sum = sum + asieval (asi, real(j))
			out[i] = sum
		    } else
			out[i] = asigrl (asi, a, b)
		} else {
		    ib = nint (b + 0.5)
		    ia = nint (a - 0.5)
		    if (abs (a-0.5-ia) < .001 && abs (b+0.5-ib) < .001) {
			sum = 0.
			do j = ib, ia
			    sum = sum + asieval (asi, real(j))
			out[i] = sum
		    } else
			out[i] = asigrl (asi, b, a)
		}
	    }
	} else {
	    do i = 1, nw {
		x = i
		call mw_c2trand (cto, x, y1, w, y2)
		call mw_c2trand (cti, w, y2, x, y1)

		if ((x >= xmin) && (x <= xmax)) {
		    a = x + 1
	            out[i] = asieval (asi, a)
		} else
	            out[i] = 0.
	    }
	}

	call asifree (asi)
end
