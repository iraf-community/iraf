# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_LENF -- Return the length of the named parameter, i.e., the number of
# stored elements in the parameter value.  NULL is returned if there is no
# value, or ERR if the parameter does not exist.

int procedure qp_lenf (qp, param)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name

pointer	sym
pointer	qp_gpsym()

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    return (ERR)
	else
	    return (S_NELEM(sym))
end
