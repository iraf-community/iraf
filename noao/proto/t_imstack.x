include	<imhdr.h>

# T_IMSTACK -- Stack images into a single image of higher dimension.

procedure t_imstack ()

int	list				# List of input images
char	output[SZ_FNAME]		# Output image

char	input[SZ_FNAME]
int	i, j, npix
long	line_in[IM_MAXDIM], line_out[IM_MAXDIM]
pointer	in, out, buf_in, buf_out

int	imtopenp(), imtgetim(), imtlen()
int	imgnls(), imgnli(), imgnll(), imgnlr()
int	impnls(), impnli(), impnll(), impnlr()
pointer	immap()

begin
	# Get the input images and the output image.
	list = imtopenp ("images")
	call clgstr ("output", output, SZ_FNAME)

	# Add each input image to the output image.

	i = 0
	while (imtgetim (list, input, SZ_FNAME) != EOF) {
	    i = i + 1
	    in = immap (input, READ_ONLY, 0)

	    # For the first input image map the output image as a copy
	    # and increment the dimension.  Set the output line counter.

	    if (i == 1) {
		out = immap (output, NEW_COPY, in)
		call clnewimage (out)
		IM_NDIM(out) = IM_NDIM(out) + 1
		IM_LEN(out, IM_NDIM(out)) = imtlen (list)
		npix = IM_LEN(out, 1)
	        call amovkl (long(1), line_out, IM_MAXDIM)
	    }

	    # Check next input image for consistency with the output image.
	    if (IM_NDIM(in) != IM_NDIM(out) - 1)
		call error (0, "Input images not consistent")
	    do j = 1, IM_NDIM(in) {
		if (IM_LEN(in, j) != IM_LEN(out, j))
		    call error (0, "Input images not consistent")
	    }

	    # Copy the input lines from the image to the next lines of
	    # the output image.  Switch on the output data type to optimize
	    # IMIO.

	    call amovkl (long(1), line_in, IM_MAXDIM)
	    switch (IM_PIXTYPE (out)) {
	    case TY_SHORT:
	        while (imgnls (in, buf_in, line_in) != EOF) {
		    if (impnls (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovs (Mems[buf_in], Mems[buf_out], npix)
		}
	    case TY_INT:
	        while (imgnli (in, buf_in, line_in) != EOF) {
		    if (impnli (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovi (Memi[buf_in], Memi[buf_out], npix)
		}
	    case TY_LONG:
	        while (imgnll (in, buf_in, line_in) != EOF) {
		    if (impnll (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovl (Meml[buf_in], Meml[buf_out], npix)
		}
	    default:
	        while (imgnlr (in, buf_in, line_in) != EOF) {
		    if (impnlr (out, buf_out, line_out) == EOF)
		        call error (0, "Error writing output image")
		    call amovr (Memr[buf_in], Memr[buf_out], npix)
		}
	    }
	    call imunmap (in)
	}

	# Finish up.
	call imunmap (out)
	call imtclose (list)
end
