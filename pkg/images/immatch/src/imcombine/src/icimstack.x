# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>


# IC_IMSTACK -- Stack images into a single image of higher dimension.

procedure ic_imstack (list, output, mask)

int	list		#I List of images
char	output[ARB]	#I Name of output image
char	mask[ARB]	#I Name of output mask

int	i, j, npix
long	line_in[IM_MAXDIM], line_out[IM_MAXDIM], line_outbpm[IM_MAXDIM]
pointer	sp, input, bpmname, key, in, out, inbpm, outbpm, buf_in, buf_out, ptr

int	imtgetim(), imtlen(), errget()
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
pointer	immap(), pm_newmask()
errchk	immap
errchk	imgnls, imgnli, imgnll, imgnlr, imgnld, imgnlx
errchk	impnls, impnli, impnll, impnlr, impnld, impnlx

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (bpmname, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)

	iferr {
	    # Add each input image to the output image.
	    out = NULL; outbpm = NULL
	    i = 0
	    while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {

		i = i + 1
		in = NULL; inbpm = NULL
		ptr = immap (Memc[input], READ_ONLY, 0)
		in = ptr

		# For the first input image map the output image as a copy
		# and increment the dimension.  Set the output line counter.

		if (i == 1) {
		    ptr = immap (output, NEW_COPY, in)
		    out = ptr
		    IM_NDIM(out) = IM_NDIM(out) + 1
		    IM_LEN(out, IM_NDIM(out)) = imtlen (list)
		    npix = IM_LEN(out, 1)
		    call amovkl (long(1), line_out, IM_MAXDIM)

		    if (mask[1] != EOS) {
			ptr = immap (mask, NEW_COPY, in)
			outbpm = ptr
			IM_NDIM(outbpm) = IM_NDIM(outbpm) + 1
			IM_LEN(outbpm, IM_NDIM(outbpm)) = imtlen (list)
			call amovkl (long(1), line_outbpm, IM_MAXDIM)
		    }
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
		call imastr (out, Memc[key], Memc[input])

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

		# Copy mask.
		if (mask[1] != EOS) {
		    iferr (call imgstr (in, "bpm", Memc[bpmname], SZ_FNAME)) {
			Memc[bpmname] = EOS
			ptr = pm_newmask (in, 27)
		    } else
			ptr = immap (Memc[bpmname], READ_ONLY, 0)
		    inbpm = ptr

		    if (IM_NDIM(inbpm) != IM_NDIM(outbpm) - 1)
			call error (0, "Input images not consistent")
		    do j = 1, IM_NDIM(inbpm) {
			if (IM_LEN(inbpm, j) != IM_LEN(outbpm, j))
			    call error (0, "Masks not consistent")
		    }

		    call amovkl (long(1), line_in, IM_MAXDIM)
		    while (imgnli (inbpm, buf_in, line_in) != EOF) {
			if (impnli (outbpm, buf_out, line_outbpm) == EOF)
			    call error (0, "Error writing output mask")
			call amovi (Memi[buf_in], Memi[buf_out], npix)
		    }

		    call sprintf (Memc[key], SZ_FNAME, "bpm%04d")
			call pargi (i)
		    call imastr (out, Memc[key], Memc[bpmname])

		    call imunmap (inbpm)
		}

		call imunmap (in)
	    }
	} then {
	    i = errget (Memc[key], SZ_FNAME)
	    call erract (EA_WARN)
	    if (outbpm != NULL) {
		call imunmap (outbpm)
		iferr (call imdelete (mask))
		    ;
	    }
	    if (out != NULL) {
		call imunmap (out)
		iferr (call imdelete (output))
		    ;
	    }
	    if (inbpm != NULL)
		call imunmap (inbpm)
	    if (in != NULL)
		call imunmap (in)
	    call sfree (sp)
	    call error (i, "Can't make temporary stack images")
	}

	# Finish up.
	if (outbpm != NULL) {
	    call imunmap (outbpm)
	    call imastr (out, "bpm", mask)
	}
	call imunmap (out)
	call sfree (sp)
end
