# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>


# IC_IMSTACK -- Stack images into a single image of higher dimension.

procedure ic_imstack (images, nimages, output)

char	images[SZ_FNAME-1, nimages]	#I Input images
int	nimages				#I Number of images
char	output				#I Name of output image

int	i, j, npix
long	line_in[IM_MAXDIM], line_out[IM_MAXDIM]
pointer	sp, key, in, out, buf_in, buf_out, ptr

int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
pointer	immap()
errchk	immap

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	iferr {
	    # Add each input image to the output image.
	    out = NULL
	    do i = 1, nimages {
		in = NULL
		ptr = immap (images[1,i], READ_ONLY, 0)
		in = ptr

		# For the first input image map the output image as a copy
		# and increment the dimension.  Set the output line counter.

		if (i == 1) {
		    ptr = immap (output, NEW_COPY, in)
		    out = ptr
		    IM_NDIM(out) = IM_NDIM(out) + 1
		    IM_LEN(out, IM_NDIM(out)) = nimages
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

		call sprintf (Memc[key], SZ_FNAME, "stck%04d")
		    call pargi (i)
		call imastr (out, Memc[key], images[1,i])

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
		case TY_USHORT, TY_LONG:
		    while (imgnll (in, buf_in, line_in) != EOF) {
			if (impnll (out, buf_out, line_out) == EOF)
			    call error (0, "Error writing output image")
			call amovl (Meml[buf_in], Meml[buf_out], npix)
		    }
		case TY_REAL:
		    while (imgnlr (in, buf_in, line_in) != EOF) {
			if (impnlr (out, buf_out, line_out) == EOF)
			    call error (0, "Error writing output image")
			call amovr (Memr[buf_in], Memr[buf_out], npix)
		    }
		case TY_DOUBLE:
		    while (imgnld (in, buf_in, line_in) != EOF) {
			if (impnld (out, buf_out, line_out) == EOF)
			    call error (0, "Error writing output image")
			call amovd (Memd[buf_in], Memd[buf_out], npix)
		    }
		case TY_COMPLEX:
		    while (imgnlx (in, buf_in, line_in) != EOF) {
			if (impnlx (out, buf_out, line_out) == EOF)
			    call error (0, "Error writing output image")
			call amovx (Memx[buf_in], Memx[buf_out], npix)
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
	} then {
	    if (out != NULL) {
		call imunmap (out)
		call imdelete (out)
	    }
	    if (in != NULL)
		call imunmap (in)
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	# Finish up.
	call imunmap (out)
	call sfree (sp)
end
