# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# QP_ADD -- Set the value of a parameter, creating the parameter if it does
# not already exist.  (Additional control over the parameter attributes is
# possible if the parameter is created before being set).

procedure qp_addb (qp, param, value)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
bool	value			#I parameter value

int	qp_accessf()
errchk	qp_accessf, qp_addf

begin
	if (qp_accessf (qp, param) == NO)
	    call qp_addf (qp, param, TY_BOOL, 1, "", 0)
	call qp_putb (qp, param, value)
end
