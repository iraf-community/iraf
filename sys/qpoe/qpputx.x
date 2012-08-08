# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_PUTX -- Set the complex value of the named header parameter.  Type
# conversion is not permitted between complex and the other data types.

procedure qp_putx (qp, param, value)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
complex	value			#I scalar parameter value

pointer	pp
int	qp_putparam()
errchk	qp_putparam, syserrs

begin
	# Lookup the parameter and get a pointer to the value buffer.
	if (qp_putparam (qp, param, pp) != TY_COMPLEX)
	    call syserrs (SYS_QPBADCONV, param)
	else if (pp == NULL)
	    call syserrs (SYS_QPNOVAL, param)

	# Pass the new value.
	Memx[pp] = value

	# Update the parameter in the datafile.
	call qp_flushpar (qp)
end
