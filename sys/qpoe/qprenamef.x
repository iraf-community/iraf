# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_RENAMEF -- Rename a header parameter.  It is an error if the named header
# parameter does not exist, or if the new name would redefine another symbol.

procedure qp_renamef (qp, param, newname)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	newname[ARB]		#I new parameter name

pointer	sym, nsym, st
pointer	qp_gpsym(), stenter()
errchk	qp_gpsym, syserrs, stenter

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	st = QP_ST(qp)

	# Access the named parameter.
	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    call syserrs (SYS_QPUKNPAR, param)

	# Check for a parameter redefinition.
	nsym = qp_gpsym (qp, newname)
	if (nsym != NULL)
	    call syserrs (SYS_QPREDEF, newname)

	# Rename the symbol.  We cannot just edit the name, as the hash
	# function would not be able to find it.  We must create a new
	# symstruct and replace the old one by it.  The stenter can cause
	# reallocation of the symbol table, so we need to recompute the
	# symbol pointer.

	nsym = stenter (st, newname, LEN_SYMBOL)
	sym = qp_gpsym (qp, param)

	call amovi (Memi[sym], Memi[nsym], LEN_SYMBOL)
	S_FLAGS(sym) = or (S_FLAGS(sym), SF_DELETED)

	QP_MODIFIED(qp) = YES
end
