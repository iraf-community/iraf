include <mach.h>
include	<imhdr.h>

# T_IMSCALE -- Scale an image.
#
# Compute the image mean between the upper and lower limits and
# scale the image to a new mean.  The output image is of pixel type real.

procedure t_imscale ()

char	input[SZ_FNAME]				# Input image
char	output[SZ_FNAME]			# Output image
real	mean					# Output mean
real	lower					# Lower limit for mean
real	upper					# Upper limit for mean
bool	verbose					# Verbose output?

int	i
long	line_in[IM_MAXDIM], line_out[IM_MAXDIM]
real	mean_in, scale
pointer	in, out, data_in, data_out

int	imgnlr(), impnlr()
real	clgetr(), image_mean()
bool	clgetb()
pointer	immap()

begin
	# Access images and set parameters.
	call clgstr ("input", input, SZ_FNAME)
	in = immap (input, READ_WRITE, 0)
	call clgstr ("output", output, SZ_FNAME)
	out = immap (output, NEW_COPY, in)
	mean = clgetr ("mean")
	lower = clgetr ("lower")
	if (IS_INDEFR (lower))
	    lower = -MAX_REAL
	upper = clgetr ("upper")
	if (IS_INDEFR (upper))
	    upper = MAX_REAL
	verbose = clgetb ("verbose")

	# Set output pixel type to TY_REAL.
	IM_PIXTYPE(out) = TY_REAL

	# Find the image mean and rescaling.
	mean_in = image_mean (in, lower, upper)
	scale = mean / mean_in

	# Create the output image.
	call amovkl (long(1), line_in, IM_MAXDIM)
	call amovkl (long(1), line_out, IM_MAXDIM)

	# Loop through the image lines and rescale.
	while (impnlr (out, data_out, line_out) != EOF) {
	    i = imgnlr (in, data_in, line_in)
	    call amulkr (Memr[data_in], scale, Memr[data_out], IM_LEN(in, 1))
	}

	if (verbose) {
	    call printf ("Task imscale:\n")
	    call printf ("    Lower = %g\n")
		call pargr (lower)
	    call printf ("    Upper = %g\n")
		call pargr (upper)
	    call printf ("    %s:  Mean = %g\n")
		call pargstr (input)
		call pargr (mean_in)
	    call printf ("    Scale = %g\n")
		call pargr (scale)
	    call printf ("    %s:  Mean = %g\n")
		call pargstr (output)
		call pargr (mean)
	}

	# Finish up
	call imunmap (in)
	call imunmap (out)
end


# IMAGE_MEAN -- Determine the mean value of an image between lower and upper.
#
# The algorithm here is a straight image average.  In future this
# should be optimized with subsampling.

real procedure image_mean (im, lower, upper)

pointer	im				# IMIO descriptor
real	lower				# Low cutoff
real	upper				# High cutoff

int	i, npix
long	line[IM_MAXDIM]
real	sum
pointer	data, data_end

int	imgnls(), imgnli(), imgnll(), imgnlr()

begin
	sum = 0.
	npix = 0
	call amovkl (long(1), line, IM_MAXDIM)

	# Loop through the image lines to compute the mean.
	# Optimize IMIO for the image datatype.
	switch (IM_PIXTYPE (im)) {
	case TY_SHORT:
	    while (imgnls (im, data, line) != EOF) {
	        data_end = data + IM_LEN(im, 1) - 1
	        do i = data, data_end {
		    if ((Mems[i] < lower) || (Mems[i] > upper))
		        next
		    sum = sum + Mems[i]
		    npix = npix + 1
	        }
	    }
	case TY_INT:
	    while (imgnli (im, data, line) != EOF) {
	        data_end = data + IM_LEN(im, 1) - 1
	        do i = data, data_end {
		    if ((Memi[i] < lower) || (Memi[i] > upper))
		        next
		    sum = sum + Memi[i]
		    npix = npix + 1
	        }
	    }
	case TY_LONG:
	    while (imgnll (im, data, line) != EOF) {
	        data_end = data + IM_LEN(im, 1) - 1
	        do i = data, data_end {
		    if ((Meml[i] < lower) || (Meml[i] > upper))
		        next
		    sum = sum + Meml[i]
		    npix = npix + 1
	        }
	    }
	default:
	    while (imgnlr (im, data, line) != EOF) {
	        data_end = data + IM_LEN(im, 1) - 1
	        do i = data, data_end {
		    if ((Memr[i] < lower) || (Memr[i] > upper))
		        next
		    sum = sum + Memr[i]
		    npix = npix + 1
	        }
	    }
	}

	return (sum / npix)
end
