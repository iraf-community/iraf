# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_GETB -- Return the boolean value of the named header parameter.  Type
# conversion is not permitted between boolean and the other datatypes.

bool procedure qp_getb (qp, param)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name

pointer	pp
int	qp_getparam()
errchk	qp_getparam, syserrs

begin
	# Lookup the parameter and it's value.
	if (qp_getparam (qp, param, pp) != TY_BOOL)
	    call syserrs (SYS_QPBADCONV, param)
	else if (pp == NULL)
	    call syserrs (SYS_QPNOVAL, param)

	return (Memb[pp])
end
