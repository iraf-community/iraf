# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_DELETEF -- Delete a header parameter.  It is an error if the named header
# parameter does not exist.  Deletions are permanent once the datafile is
# updated.

procedure qp_deletef (qp, param)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name

pointer	sym
pointer	qp_gpsym()
errchk	qp_gpsym, syserrs

begin
	# Access the named parameter.
	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)
	else if (and (S_FLAGS(sym), SF_DELETED) != 0)
	    return

	# If the parameter value is stored in its own lfile, delete it.
	if (S_LFILE(sym) > LF_STATICPARS)
	    call fm_lfdelete (QP_FM(qp), S_LFILE(sym))

	# Set the delete bit in the symbol descriptor.
	S_FLAGS(sym) = or (S_FLAGS(sym), SF_DELETED)

	QP_MODIFIED(qp) = YES
end
