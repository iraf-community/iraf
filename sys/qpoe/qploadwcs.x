include	<syserr.h>
include	"qpoe.h"

# QP_LOADWCS -- Load the default WCS, if there is one, from the QPOE image
# header.  A QPOE file can contain any number of WCS, but the default WCS
# should relate the physical coordinate system, e.g., sky coordinates in 
# the range 1024sq, 8192sq, etc., to world coordinates, e.g., the TAN
# projection.  Probably we should provide for multiple physical coordinate
# systems (sky, detector, etc.) each with its own WCS, but at present we
# assume a single WCS.

pointer procedure qp_loadwcs (qp)

pointer	qp				#I QPOE descriptor

int	wcslen
pointer	sp, svwcs, mw
errchk	qp_lenf, syserrs, qp_read, mw_open
int	qp_lenf(), qp_read()
pointer	mw_open()
string	s_qpwcs QPWCS

begin
	# Determine if there is a WCS, and if so, how big the saved version is.
	wcslen = qp_lenf (qp, s_qpwcs)
	if (wcslen <= 0)
	    call syserrs (SYS_QPNOWCS, QP_DFNAME(qp))

	call smark (sp)
	call salloc (svwcs, wcslen, TY_CHAR)

	# Retrieved the saved wcs, and load it into an MWCS descriptor.
	wcslen = qp_read (qp, s_qpwcs, Memc[svwcs], wcslen, 1, "opaque")
	mw = mw_open (svwcs, 0)

	call sfree (sp)
	return (mw)
end
