# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<qpset.h>
include	"qpoe.h"

# QP_QUERYF -- Get information describing the named parameter.  The current
# vector length of the parameter is returned as the function value, or ERR
# if the parameter does not exist.

int procedure qp_queryf (qp, param, datatype, maxelem, comment, flags)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	datatype[SZ_DATATYPE]	#O parameter data type
int	maxelem			#O allocated length of parameter
char	comment[SZ_COMMENT]	#O comment describing parameter
int	flags			#O parameter flag word

int	junk
pointer	sym, dsym, ip, st

int	qp_gstr()
pointer	qp_gpsym(), stname(), strefstab(), strefsbuf()
errchk	qp_gpsym, stname, syserrs

begin
	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	st = QP_ST(qp)

	# Locate the symbol.
	sym = qp_gpsym (qp, param)
	if (sym == NULL)
	    return (ERR)

	flags   = S_FLAGS(sym)
	maxelem = S_MAXELEM(sym)

	# Output the symbolic datatype.
	datatype[2] = EOS
	switch (S_DTYPE(sym)) {
	case TY_BOOL:
	    datatype[1] = 'b'
	case TY_CHAR:
	    datatype[1] = 'c'
	case TY_SHORT:
	    datatype[1] = 's'
	case TY_INT:
	    datatype[1] = 'i'
	case TY_LONG:
	    datatype[1] = 'l'
	case TY_REAL:
	    datatype[1] = 'r'
	case TY_DOUBLE:
	    datatype[1] = 'd'
	case TY_COMPLEX:
	    datatype[1] = 'x'

	case TY_MACRO:
	    call strcpy ("macro", datatype, SZ_DATATYPE)
	case TY_OPAQUE:
	    call strcpy ("opaque", datatype, SZ_DATATYPE)

	case TY_USER:
	    # User defined type: if S_DSYM is NULL, this is the domain entry
	    # itself, else the domain name is the datatype of the parameter.
	    # If this is a primary domain entry, the field list defining the
	    # structure is stored as the string value of the parameter.

	    if (S_DSYM(sym) == NULL)
		junk = qp_gstr (qp, param, datatype, SZ_DATATYPE)
	    else {
		dsym = strefstab (st, S_DSYM(sym))
		call strcpy (Memc[stname(st,dsym)], datatype, SZ_DATATYPE)
	    }

	default:
	    call strcpy ("<unknown>", datatype, SZ_DATATYPE)
	}

	# Output the comment field.
	if (S_COMMENT(sym) != NULL) {
	    ip = strefsbuf (st, S_COMMENT(sym))
	    call strcpy (Memc[ip], comment, SZ_COMMENT)
	} else
	    comment[1] = EOS

	return (S_NELEM(sym))
end
