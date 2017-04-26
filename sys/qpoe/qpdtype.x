# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"qpoe.h"

# QP_DTYPE -- Translate the given symbolic datatype name into an integer
# type code.  The possible type codes are most of the standard SPP TY_type
# codes, plus TY_MACRO, TY_OPAQUE, and TY_USER.  If the symbol type is TY_USER
# (a user defined data structure or domain)), then a pointer to the symbol
# table entry for the named domain is returned as an output argument.  The
# integer type code is returned as the function value.

int procedure qp_dtype (qp, datatype, dsym)

pointer	qp			#I QPOE descriptor
char	datatype[ARB]		#I symbolic datatype name
pointer	dsym			#O pointer to domain symbol, if TY_USER

char	junk[1]
int	dtype, ip
string	types "|bool|char|short|int|long|real|double|complex|macro|opaque|"

pointer	stfind()
int	stridx(), strdic()

begin
	dtype = NULL
	dsym  = NULL
	for (ip=1;  IS_WHITE(datatype[ip]);  ip=ip+1)
	    ;

	# Single character standard dtype code (bcsilrdx)?
	if (datatype[ip+1] == EOS)
	    dtype = stridx (datatype[ip], SPPTYPES)

	# Spelled out dtype name.  Check standard names first.
	if (dtype == NULL) {
	    dtype = strdic (datatype[ip], junk, 1, types)
	    if (dtype == 9)
		dtype = TY_MACRO
	    else if (dtype == 10)
		dtype = TY_OPAQUE
	}

	# Lastly, check the special types.
	if (dtype == 0) {
	    if (datatype[ip] == '{')			# field list
		dtype = TY_USER
	    else {
		dsym = stfind (QP_ST(qp), datatype[ip])
		if (dsym != NULL)
		    dtype = S_DTYPE(dsym)
	    }
	}

	return (dtype)
end
