# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# COERCE -- Coerce a pointer from one datatype to another, choosing the
# next larger element for t2 in the event that t1 is not aligned with t2.

pointer procedure coerce (ptr, type1, type2)

pointer	ptr				# input pointer
int	type1, type2			# from, to data types
int	n
pointer	p
include	<szdtype.inc>

begin
	p = ptr - 1
	if (type1 == TY_CHAR)
	    return (p / ty_size[type2] + 1)
	else if (type2 == TY_CHAR)
	    return (p * ty_size[type1] + 1)
	else {
	    p = p * ty_size[type1]				# ptr to char
	    n = ty_size[type2]
	    return (((p + n-1) / n) + 1)
	}
end
