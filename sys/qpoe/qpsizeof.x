# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_SIZEOF -- Determine the size in chars of a QPOE datatype.  This may
# be one of the special datatypes (user defined record types), or a primitive
# type.  In the case of a special type, the REFTYPE flag specifies whether
# the size of the value of the type variable itself (always SZ_CHAR) is to be
# returned, or the size of an *instance* of the special type.

int procedure qp_sizeof (qp, dtype, dsym, reftype)

pointer	qp			#I QPOE descriptor
int	dtype			#I datatype code
pointer	dsym			#I domain descriptor, if type TY_USER
int	reftype			#I IMMEDIATE (domain itself) or INSTANCEOF

pointer	sym
int	sizeof()
pointer	strefstab()

begin
	switch (dtype) {
	case TY_MACRO, TY_OPAQUE:
	    return (SZ_CHAR)

	case TY_USER:
	    # Size of a user defined structure (or the element size of the
	    # struct definition entry itself).

	    if (dsym == NULL) {			# {...}
		return (SZ_CHAR)
	    } else if (S_DSYM(dsym) == NULL) {	# reference is to
		if (reftype == IMMEDIATE)	# primary domain entry
		    return (SZ_CHAR)
		else
		    return (S_SZELEM(dsym))
	    } else {				# instance of domain
		sym = strefstab (QP_ST(qp), S_DSYM(dsym))
		return (S_SZELEM(sym))
	    }

	default:
	    return (sizeof (dtype))
	}
end
