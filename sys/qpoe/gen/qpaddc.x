# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../qpoe.h"

# QP_ADD -- Set the value of a parameter, creating the parameter if it does
# not already exist.  This works for the most common case of simple scalar
# valued header parameters, although any parameter may be written into it it
# already exists.  Additional control over the parameter attributes is possible
# if the parameter is explicitly created with qp_addf before being written into.

procedure qp_addc (qp, param, value, comment)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	value			#I parameter value
char	comment[ARB]		#I comment field, if creating parameter

char	datatype[1]
errchk	qp_accessf, qp_addf
string	dtypes SPPTYPES
int	qp_accessf()

begin
	if (qp_accessf (qp, param) == NO) {
	    datatype[1] = dtypes[TY_CHAR]
	    call qp_addf (qp, param, datatype, 1, comment, 0)
	}
	call qp_putc (qp, param, value)
end
