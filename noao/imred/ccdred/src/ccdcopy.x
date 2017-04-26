include	<imhdr.h>

# CCDCOPY -- Copy an image.  This should be done with an IMIO procedure
# but there isn't one yet.

procedure ccdcopy (old, new)

char	old[ARB]		# Image to be copied
char	new[ARB]		# New copy

int	i, nc, nl
pointer	in, out, immap(), imgl2s(), impl2s(), imgl2r(), impl2r()

begin
	in = immap (old, READ_ONLY, 0)
	out = immap (new, NEW_COPY, in)

	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	switch (IM_PIXTYPE(in)) {
	case TY_SHORT:
	    do i = 1, nl
		call amovs (Mems[imgl2s(in,i)], Mems[impl2s(out,i)], nc)
	default:
	    do i = 1, nl
		call amovr (Memr[imgl2r(in,i)], Memr[impl2r(out,i)], nc)
	}

	call imunmap (in)
	call imunmap (out)
end
