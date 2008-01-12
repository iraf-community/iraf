# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include "qpoe.h"

# QP_GSTR -- Return the string value of the named parameter.

int procedure qp_gstr (qp, param, outstr, maxch)

pointer qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	outstr[maxch]		#O receives string value
int	maxch			#I max chars out

pointer	sym, fm
int	nchars, fd
pointer	qp_gpsym()
int	fm_getfd(), read()
errchk	qp_bind, qp_gpsym, syserrs, fm_getfd, seek

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)
	fm = QP_FM(qp)

	# Lookup the symbol in the symbol table.
	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)
	else if (!(S_DTYPE(sym) == TY_CHAR || S_DTYPE(sym) == TY_USER))
	    call syserrs (SYS_QPBADCONV, param)

	# Fetch the string value from the lfile where the data is stored.
	fd = fm_getfd (fm, S_LFILE(sym), READ_ONLY, 0)
	call seek (fd, S_OFFSET(sym))

	nchars = max (0, read (fd, outstr, min(S_NELEM(sym),maxch)))
	outstr[nchars+1] = EOS

	call fm_retfd (fm, S_LFILE(sym))
	return (nchars)
end
