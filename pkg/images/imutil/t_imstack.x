# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

define	NTYPES	7


# T_IMSTACK -- Stack images into a single image of higher dimension.

procedure t_imstack ()

int	i, j, npix, list
long	line_in[IM_MAXDIM], line_out[IM_MAXDIM]
pointer	sp, input, output, in, out, buf_in, buf_out

pointer	immap()
int	imtopenp(), imtgetim(), imtlen()
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)

	# Get the input images and the output image.
	list = imtopenp ("images")
	call clgstr ("output", Memc[output], SZ_FNAME)

	# Add each input image to the output image.

	i = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    i = i + 1
	    in = immap (Memc[input], READ_ONLY, 0)

	    # For the first input image map the output image as a copy
	    # and increment the dimension.  Set the output line counter.

	    if (i == 1) {
		out = immap (Memc[output], NEW_COPY, in)
		call isk_new_image (out)
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

	# Finish up.
	call imunmap (out)
	call imtclose (list)
end


# ISK_NEW_IMAGE -- Get a new image title and pixel type.
#
# The strings 'default' or '*' are recognized as defaulting to the original
# title or pixel datatype.

procedure isk_new_image (im)

pointer	im				# image descriptor

pointer	sp, lbuf
int	i, type_codes[NTYPES]
bool	strne()
int	stridx()

string	types "suilrdx"
data	type_codes /TY_SHORT,TY_USHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,
	TY_COMPLEX/

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	call clgstr ("title", Memc[lbuf], SZ_LINE)
	if (strne (Memc[lbuf], "default") && strne (Memc[lbuf], "*"))
	    call strcpy (Memc[lbuf], IM_TITLE(im), SZ_IMTITLE) 

	call clgstr ("pixtype", Memc[lbuf], SZ_LINE)
	if (strne (Memc[lbuf], "default") && strne (Memc[lbuf], "*")) {
	    i = stridx (Memc[lbuf], types)
	    if (i != 0)
	        IM_PIXTYPE(im) = type_codes[i]
	}

	call sfree (sp)
end
