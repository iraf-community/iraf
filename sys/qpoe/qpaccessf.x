# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_ACCESSF -- Test whether the named field (header parameter) exists.
# Globally aliased parameters are recursively expanded and must resolve to
# a normal parameter reference.

int procedure qp_accessf (qp, param)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name

pointer	qp_gpsym()
errchk	qp_bind, qp_gpsym

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)
	if (qp_gpsym (qp, param) != NULL)
	    return (YES)
	else
	    return (NO)
end
