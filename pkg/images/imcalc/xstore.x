# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"imcalc.h"

# X_STORE -- Store the input register in the next output line of the output
# image.  Set the global ATEOF flag (for BNEOF) when the end of the section
# is reached.  When we are called the next output line has already been
# computed and is ready to be written out.  The output buffer pointer was
# saved in the last call (or at parse time).  Move the data into the output
# buffer and get a new output buffer for the next call, checking for EOF in
# the process.  A redundant memory to memory copy is involved, but the
# alternative is too complicated to attempt at present.

procedure x_store (in, out)

pointer	in			# input register
pointer	out			# imcalc image descriptor

int	status
pointer	im, lbuf, ibuf, obuf
int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
include	"imcalc.com"

begin
	lbuf = I_LBUF(out)
	im   = I_IM(out)

	if (I_ATEOF(out) == YES) {
	    c_ateof = YES
	    return
	}

	switch (R_DATATYPE(in)) {
	case TY_SHORT:
	    ibuf = (R_LBUF(in) - 1) * SZ_SHORT + 1
	    obuf = (lbuf - 1) * SZ_SHORT + 1
	    call amovc (Memc[ibuf], Memc[obuf], R_LENGTH(in) * SZ_SHORT)
	    status = impnls (imdes, lbuf, I_V(im))
	case TY_USHORT, TY_LONG:
	    ibuf = (R_LBUF(in) - 1) * SZ_LONG + 1
	    obuf = (lbuf - 1) * SZ_LONG + 1
	    call amovc (Memc[ibuf], Memc[obuf], R_LENGTH(in) * SZ_LONG)
	    status = impnll (imdes, lbuf, I_V(im))
	case TY_INT:
	    ibuf = (R_LBUF(in) - 1) * SZ_INT + 1
	    obuf = (lbuf - 1) * SZ_INT + 1
	    call amovc (Memc[ibuf], Memc[obuf], R_LENGTH(in) * SZ_INT)
	    status = impnli (imdes, lbuf, I_V(im))
	case TY_REAL:
	    ibuf = (R_LBUF(in) - 1) * SZ_REAL + 1
	    obuf = (lbuf - 1) * SZ_REAL + 1
	    call amovc (Memc[ibuf], Memc[obuf], R_LENGTH(in) * SZ_REAL)
	    status = impnlr (imdes, lbuf, I_V(im))
	case TY_DOUBLE:
	    ibuf = (R_LBUF(in) - 1) * SZ_DOUBLE + 1
	    obuf = (lbuf - 1) * SZ_DOUBLE + 1
	    call amovc (Memc[ibuf], Memc[obuf], R_LENGTH(in) * SZ_DOUBLE)
	    status = impnld (imdes, lbuf, I_V(im))
	case TY_COMPLEX:
	    ibuf = (R_LBUF(in) - 1) * SZ_COMPLEX + 1
	    obuf = (lbuf - 1) * SZ_COMPLEX + 1
	    call amovc (Memc[ibuf], Memc[obuf], R_LENGTH(in) * SZ_COMPLEX)
	    status = impnlx (imdes, lbuf, I_V(im))
	default:
	    call error (1, "x_store: unknown image pixel datatype")
	}

	if (status == EOF) {
	    I_ATEOF(out) = YES			# on image
	    c_ateof = YES			# for BNEOF
	} else
	    I_LBUF(out) = lbuf
end
