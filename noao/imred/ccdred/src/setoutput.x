include	<imhdr.h>

# SET_OUTPUT -- Setup the output image.
# The output image is a NEW_COPY of the input image.
# The user may select the pixel datatype.

procedure set_output (in, out, output)

pointer	in			# Input IMIO pointer to copy
pointer	out			# Output IMIO pointer
char	output[SZ_FNAME]	# Output image name

int	clscan(), nscan()
char	type[1]
pointer	immap()
errchk	immap

begin
	out = immap (output, NEW_COPY, in)
	IM_PIXTYPE(out) = TY_REAL
	if (clscan ("pixeltype")  != EOF) {
	    call gargwrd (type, 1)
	    if (nscan() == 1) {
		if (type[1] == 'r')
		    IM_PIXTYPE(out) = TY_REAL
		else if (type[1] == 's')
		    IM_PIXTYPE(out) = TY_SHORT
		else {
		    call imunmap (out)
		    call error (0, "Unknown pixel type")
		}
	    }
	}
end
