include	<imhdr.h>
include	<math/iminterp.h>

task	zzdebug1, zzdebug2


# ZZDEBUG1 -- Compute function and first two derivatives at some linear sampling

procedure zzdebug1 ()

char	input[SZ_FNAME]		# Input
char	output[SZ_FNAME]	# Output
int	type			# Type of interpolation
real	x1			# Starting value
real	dx			# Sampling interval
int	nout			# Number of output values

int	i, nin, strdic(), clgeti()
real	x, der[3], clgetr()
pointer	in, out, indata, outdata,junkdata, asi, immap(), imgl1r(), imps2r()
real	arieval(), asieval(), asigrl()

begin
	call clgstr ("input", input, SZ_FNAME)
	in = immap (input, READ_ONLY, 0)
	call clgstr ("output", output, SZ_FNAME)
	out = immap (output, NEW_COPY, in)
	call clgstr ("interp", input, SZ_FNAME)
	type = strdic (input, input, SZ_FNAME, II_FUNCTIONS)
	x1 = clgetr ("x1")
	dx = clgetr ("dx")
	nout = clgeti ("nout")

	nin = IM_LEN(in,1)
	IM_NDIM(out) = 2
	IM_LEN(out,1) = nout
	IM_LEN(out,2) = 3

	indata = imgl1r (in)
	outdata = imps2r (out, 1, nout, 1, 3)

	call asiinit (asi, type)
	call asifit (asi, Memr[indata], nin)
	do i = 0, nout-1 {
	    x = x1 + i * dx
	    call asider (asi, x, der, 3)
	    Memr[outdata+i] = der[1]
	    Memr[outdata+i+nout] = der[2]
	    Memr[outdata+i+2*nout] = der[3]
	}

	call printf ("x1=%g x1.5=%g x2.0=%g\n")
	    call pargr (asieval (asi, 1.0))
	    call pargr (asieval (asi, 1.5))
	    call pargr (asieval (asi, 2.0))
	call printf ("xn-1=%g xn-1.5=%g xn=%g\n")
	    call pargr (asieval (asi, real(nin-1)))
	    call pargr (asieval (asi, real (nin-0.5)))
	    call pargr (asieval (asi, real(nin)))

	call printf ("integral=%g\n")
	    call pargr (asigrl (asi, 41.0, real (nin-40+1)))

	call printf ("x1=%g x1.5=%g x2.0=%g\n")
	    call pargr (arieval (1.0, Memr[indata], nin, type))
	    call pargr (arieval (1.5, Memr[indata], nin, type))
	    call pargr (arieval (2.0, Memr[indata], nin, type))

	call printf ("xn-1=%g xn-1.5=%g xn=%g\n")
	    call pargr (arieval (real(nin-1), Memr[indata], nin, type))
	    call pargr (arieval (real(nin-0.5), Memr[indata], nin, type))
	    call pargr (arieval (real (nin), Memr[indata], nin, type))

	call arider (1.0, Memr[indata], nin, der, 3, type)
	call printf ("x=%g der1=%g der2=%g der3=%g\n")
	    call pargr (1.0)
	    call pargr (der[1])
	    call pargr (der[2])
	    call pargr (der[3])
	call arider (1.5, Memr[indata], nin, der, 3, type)
	call printf ("x=%g der1=%g der2=%g der3=%g\n")
	    call pargr (1.5)
	    call pargr (der[1])
	    call pargr (der[2])
	    call pargr (der[3])
	call arider (2.0, Memr[indata], nin, der, 3, type)
	call printf ("x=%g der1=%g der2=%g der3=%g\n")
	    call pargr (2.0)
	    call pargr (der[1])
	    call pargr (der[2])
	    call pargr (der[3])

	call arider (real(nin-1), Memr[indata], nin, der, 3, type)
	call printf ("x=%g der1=%g der2=%g der3=%g\n")
	    call pargr (real(nin-1))
	    call pargr (der[1])
	    call pargr (der[2])
	    call pargr (der[3])
	call arider (real(nin-0.5), Memr[indata], nin, der, 3, type)
	call printf ("x=%g der1=%g der2=%g der3=%g\n")
	    call pargr (real(nin-0.5))
	    call pargr (der[1])
	    call pargr (der[2])
	    call pargr (der[3])
	call arider (real(nin), Memr[indata], nin, der, 3, type)
	call printf ("x=%g der1=%g der2=%g der3=%g\n")
	    call pargr (real(nin))
	    call pargr (der[1])
	    call pargr (der[2])
	    call pargr (der[3])

	call printf ("opix1=%g opix2=%g\n")
	    call pargr (Memr[indata+100-1])
	    call pargr (Memr[indata+200-1])
	Memr[indata+100-1] = INDEF
	Memr[indata+200-1] = INDEF
	call malloc (junkdata, nin, TY_REAL)
	call arbpix (Memr[indata], Memr[junkdata], nin, type, II_BOUNDARYEXT)
	call printf ("pix1=%g pix2=%g\n")
	    call pargr (Memr[junkdata+100-1])
	    call pargr (Memr[junkdata+200-1])
	call mfree (junkdata, TY_REAL)

	call asifree (asi)

	call imunmap (out)
	call imunmap (in)
end


# ZZDEBUG2 -- Shift forward and reverse and compute difference between
# final interpolated line and input data line.

procedure zzdebug2 ()

char	input[SZ_FNAME]		# Input
char	output[SZ_FNAME]	# Output
int	type			# Type of interpolation
real	shift			# Shift

int	i, npts, strdic()
real	clgetr()
pointer	in, out, indata, outdata, x, y, asi, immap(), imgl1r(), impl1r()

begin
	call clgstr ("input", input, SZ_FNAME)
	in = immap (input, READ_ONLY, 0)
	call clgstr ("output", output, SZ_FNAME)
	out = immap (output, NEW_COPY, in)
	call clgstr ("interp", input, SZ_FNAME)
	type = strdic (input, input, SZ_FNAME, II_FUNCTIONS)
	shift = clgetr ("shift")

	npts = IM_LEN(in,1)
	indata = imgl1r (in)
	IM_LEN(out,1) = npts-2
	outdata = impl1r (out)
	call malloc (x, npts, TY_REAL)
	call malloc (y, npts, TY_REAL)

	call asiinit (asi, type)

	do i = 1, npts
	    Memr[x+i-1] = i + shift
	call asifit (asi, Memr[indata], npts)
	call asivector (asi, Memr[x], Memr[y], npts)

	if (shift > 0.) {
	    do i = 1, npts-1
		Memr[x+i-1] = i - shift
	    call asifit (asi, Memr[y], npts-1)
	    call asivector (asi, Memr[x], Memr[y], npts-1)
	} else {
	    do i = 1, npts-1
		Memr[x+i-1] = i - shift
	    call asifit (asi, Memr[y+1], npts-1)
	    call asivector (asi, Memr[x], Memr[y+1], npts-1)
	}

	call asubr (Memr[indata+1], Memr[y+1], Memr[outdata], npts-2)

	call asifree (asi)

	call imunmap (out)
	call imunmap (in)
end
