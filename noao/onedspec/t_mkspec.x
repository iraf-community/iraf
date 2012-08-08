include	<imhdr.h>

# T_MKSPEC -- Make a test artificial spectrum - May be 2 dimensional
#           Options for the form of the spectrum currently include
#              1 - Flat spectrum
#              2 - Ramp
#              3 - Black body - f-lambda

procedure t_mkspec()

char	spec[SZ_FNAME], sname[SZ_IMTITLE]
int	ncols, nlines, func_type, i
real	const1, const2, dconst, const
real	wstart, wend, dw, temp, x, w, fmax
real	c1, c2
pointer	im, buf, sp, row

pointer	immap(), impl1r(), impl2r()
int	clgeti()
real	clgetr()

begin
	# Initialize Black body constants
	c1 = 3.7415e-5
	c2 = 1.4388

	# Get spectrum file name
	call clgstr ("image_name", spec, SZ_FNAME)

	# And title
	call clgstr ("image_title", sname, SZ_IMTITLE)

	# Length
	ncols = clgeti ("ncols")

	# Height
	nlines = clgeti ("nlines")

	# Pixel type

	# Open image
	im = immap (spec, NEW_IMAGE, 0)

	# Load parameters
	IM_LEN(im,1) = ncols
	IM_LEN(im,2) = nlines
	
	# 1 or 2 Dimensional image
	if (nlines > 1)
	    IM_NDIM(im) = 2
	else
	    IM_NDIM(im) = 1

	IM_PIXTYPE(im) = TY_REAL
	call strcpy (sname, IM_TITLE(im), SZ_IMTITLE)
	

	func_type = clgeti ("function")

	# Get additional parameters for functin types
	switch (func_type) {

	    # Flat spectrum
	    case 1:
		const = clgetr ("constant")

	    # Ramp spectrum
	    case 2:
		const1 = clgetr ("start_level")
		const2 = clgetr ("end_level")
		dconst = (const2 - const1) / (ncols - 1)

	    # Black body
	    case 3:
		wstart = clgetr ("start_wave")		# Start wave Angstroms
		wend   = clgetr ("end_wave")		# End wave
		temp   = clgetr ("temperature")		# BB temp deg.K
		dw     = (wend - wstart) / (ncols - 1)
		w      = wstart * 1.0e-8		# Convert to cm.
		fmax   = 1.2865e-4 * temp**5		# Peak f-lambda

	    default:
		call error (1, "Unknown Function type")
	}

	# Allocate space for a row since each row will be duplicated
	# NLINES times
	call smark (sp)
	call salloc (row, ncols, TY_REAL)

	# Fill a row
	do i = 1, ncols {
	    switch (func_type) {
	    case 1:
		Memr[row+i-1] = const
	    case 2:
		Memr[row+i-1] = const1 + (i-1) * dconst
	    case 3:
		x = exp (c2 /w /temp)
		Memr[row+i-1] = (c1 / w**5 / (x-1.0)) / fmax
		w = w + dw * 1.0e-8
	    }
	}

	# Write all lines out
	do i = 1, nlines {

	    # Access either 1 or 2 dimensional line
	    if (nlines > 1)
		buf = impl2r (im,i)
	    else
		buf = impl1r (im)

	    # Copy saved row to output image
	    call amovr (Memr[row], Memr[buf], ncols)
	}

	call sfree (sp)
	call imunmap (im)
end
