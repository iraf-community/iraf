include	"qpoe.h"

# QP_SAVEWCS -- Save the given WCS in the QPOE header, as a variable length
# binary array (machine independent) in the default WCS parameter QPWCS.

procedure qp_savewcs (qp, mw)

pointer	qp				#I QPOE descriptor
pointer	mw				#I MWCS descriptor

pointer	bp
int	buflen, nchars
int	mw_save(), qp_accessf()
errchk	mw_save, qp_accessf, qp_addf, qp_write
string	s_opaque "opaque"
string	s_qpwcs QPWCS

begin
	bp = NULL
	buflen = 0

	# Encode the WCS as a machine independent binary array.
	nchars = mw_save (mw, bp, buflen)

	# Save it in the QPOE header.
	if (nchars > 0) {
	    if (qp_accessf (qp, s_qpwcs) == NO)
		call qp_addf (qp, s_qpwcs,
		    s_opaque, 0, "World coordinate system", SF_INHERIT)
	    call qp_write (qp, s_qpwcs, Memc[bp], nchars, 1, s_opaque)
	}

	if (bp != NULL)
	    call mfree (bp, TY_CHAR)
end
