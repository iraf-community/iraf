include	<imhdr.h>
include	<imset.h>

# SET_OUTPUT -- Setup the output image.
# The output image is a NEW_COPY of the input image.
# The user may select a pixel datatype with higher precision though not
# lower.

procedure set_output (in, out, output)

pointer	in			# Input IMIO pointer to copy
pointer	out			# Output IMIO pointer
char	output[SZ_FNAME]	# Output image name

int	i, clscan(), nscan()
char	type[1]
pointer	immap()
errchk	immap

begin
	out = immap (output, NEW_COPY, in)
	IM_PIXTYPE(out) = TY_REAL
	if (clscan ("pixeltype")  != EOF) {
	    call gargwrd (type, 1)
	    if (nscan() == 1) {
		i = IM_PIXTYPE(in)
		IM_PIXTYPE(out) = i
		switch (type[1]) {
		case 's':
		    if (i == TY_USHORT)
			IM_PIXTYPE(out) = TY_SHORT
		case 'u':
		    if (i == TY_SHORT)
			IM_PIXTYPE(out) = TY_USHORT
		case 'i':
		    if (i == TY_SHORT || i == TY_USHORT)
			IM_PIXTYPE(out) = TY_INT
		case 'l':
		    if (i == TY_SHORT || i == TY_USHORT || i == TY_INT)
			IM_PIXTYPE(out) = TY_LONG
		case 'r':
		    if (i != TY_DOUBLE)
			IM_PIXTYPE(out) = TY_REAL
		case 'd':
		    IM_PIXTYPE(out) = TY_DOUBLE
		default:
		    call imunmap (out)
		    call error (0, "Unknown pixel type")
		}
	    }
	}
end
