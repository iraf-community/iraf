# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imcalc.h"

# X_LOAD -- Load the next line from the input image into the output register.
# Keep the last line read around when the end of the input section is reached.

procedure x_load (in, out)

pointer	in			# imcalc image descriptor
pointer	out			# output register

int	status
pointer	im, lbuf
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld(), imgnlx()

begin
	im = I_IM(in)

	if (I_ATEOF(in) == NO) {
	    switch (I_PIXTYPE(in)) {
	    case TY_SHORT:
		status = imgnls (im, lbuf, I_V(in))
	    case TY_USHORT, TY_LONG:
		status = imgnll (im, lbuf, I_V(in))
	    case TY_INT:
		status = imgnli (im, lbuf, I_V(in))
	    case TY_REAL:
		status = imgnlr (im, lbuf, I_V(in))
	    case TY_DOUBLE:
		status = imgnld (im, lbuf, I_V(in))
	    case TY_COMPLEX:
		status = imgnlx (im, lbuf, I_V(in))
	    default:
		call error (1, "x_load: unknown image pixel datatype")
	    }

	    if (status == EOF) {
		I_ATEOF(in) = YES
		lbuf = I_LBUF(in)
	    }
	}

	R_LBUF(out) = lbuf
end
