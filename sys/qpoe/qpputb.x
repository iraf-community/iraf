# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_PUTB -- Set the boolean value of the named header parameter.  Type
# conversion is not permitted between boolean and the other data types.

procedure qp_putb (qp, param, value)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
bool	value			#I scalar parameter value

pointer	pp
int	qp_putparam()
errchk	qp_putparam, syserrs

begin
	# Lookup the parameter and get a pointer to the value buffer.
	if (qp_putparam (qp, param, pp) != TY_BOOL)
	    call syserrs (SYS_QPBADCONV, param)
	else if (pp == NULL)
	    call syserrs (SYS_QPNOVAL, param)

	# Pass the new value.
	Memb[pp] = value

	# Update the parameter in the datafile.
	call qp_flushpar (qp)
end
