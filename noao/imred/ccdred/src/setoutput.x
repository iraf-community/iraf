include	<imhdr.h>

define	PIXTYPES	"|short|real|"
define	SHORT		1
define	REAL		2

# SET_OUTPUT -- Setup the output image.
# The output image is a NEW_COPY of the input image.
# The user may select the pixel datatype.

procedure set_output (in, out, output)

pointer	in			# Input IMIO pointer to copy
pointer	out			# Output IMIO pointer
char	output[SZ_FNAME]	# Output image name

int	type, strdic()
pointer	sp, str, immap()
errchk	immap

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	out = immap (output, NEW_COPY, in)

	call clgstr ("pixeltype", Memc[str], SZ_LINE)
	if (Memc[str] == EOS)
	    IM_PIXTYPE(out) = IM_PIXTYPE(in)
	else {
	    type = strdic (Memc[str], Memc[str], SZ_LINE, PIXTYPES)
	    switch (type) {
	    case SHORT:
	        IM_PIXTYPE(out) = TY_SHORT
	    case REAL:
	        IM_PIXTYPE(out) = TY_REAL
	    default:
		call imunmap (out)
		call error (0, "Unknown pixel type")
	    }
	}

	call sfree (sp)
end
