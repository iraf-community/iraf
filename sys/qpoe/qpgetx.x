# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_GETX -- Return the complex value of the named header parameter.  Type
# conversion is not permitted between complex and the other datatypes.

complex procedure qp_getx (qp, param)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name

pointer	pp
int	qp_getparam()
errchk	qp_getparam, syserrs

begin
	# Lookup the parameter and it's value.
	if (qp_getparam (qp, param, pp) != TY_COMPLEX)
	    call syserrs (SYS_QPBADCONV, param)
	else if (pp == NULL)
	    call syserrs (SYS_QPNOVAL, param)

	return (Memx[pp])
end
