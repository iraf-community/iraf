# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_ASTR -- Set the value of a string parameter, creating the parameter if
# it does not already exist.  (Additional control over the parameter attributes
# is possible if the parameter is created before being set).

procedure qp_astr (qp, param, value)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	value[ARB]		#I parameter value

int	nchars
int	qp_accessf(), strlen()
errchk	qp_accessf, qp_addf

begin
	# By default we allocate a somewhat bigger storage area than needed
	# to store the string, to permit updates of a similar length.	If
	# more control over the maximum string length is needed, QP_ADDF
	# should be called explicitly.

	if (qp_accessf (qp, param) == NO) {
	    nchars = (strlen(value) + INC_STRLEN-1) / INC_STRLEN * INC_STRLEN
	    call qp_addf (qp, param, TY_CHAR, nchars, "", 0)
	}

	call qp_pstr (qp, param, value)
end
