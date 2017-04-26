# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include "qpoe.h"

# QP_PSTR -- Update the string value of the named parameter.

procedure qp_pstr (qp, param, strval)

pointer qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	strval[ARB]		#I new string value

pointer	fm, sym
int	fd, nchars

pointer	qp_gpsym()
int	fm_getfd(), strlen()
errchk	qp_bind, qp_gpsym, syserrs, fm_getfd, seek

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	fm = QP_FM(qp)

	# Lookup the symbol in the symbol table.
	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)
	else if (S_DTYPE(sym) != TY_CHAR)
	    call syserrs (SYS_QPBADCONV, param)

	# Update the value of the parameter in the datafile.
	fd = fm_getfd (fm, S_LFILE(sym), READ_WRITE, 0)
	call seek (fd, S_OFFSET(sym))

	nchars = strlen (strval)
	if (S_MAXELEM(sym) > 0)
	    nchars = min (S_MAXELEM(sym), nchars)

	call write (fd, strval, nchars)
	S_NELEM(sym) = nchars
	QP_MODIFIED(qp) = YES

	call fm_retfd (fm, S_LFILE(sym))
end
