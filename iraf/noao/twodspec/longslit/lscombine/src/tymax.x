# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>


# TY_MAX -- Return the datatype of highest precedence.

int procedure ty_max (type1, type2)

int	type1, type2		# Datatypes

int	i, j, type, order[8]
data	order/TY_SHORT,TY_USHORT,TY_INT,TY_LONG,TY_REAL,TY_DOUBLE,TY_COMPLEX,TY_REAL/

begin
	for (i=1; (i<=7) && (type1!=order[i]); i=i+1)
	    ;
	for (j=1; (j<=7) && (type2!=order[j]); j=j+1)
	    ;
	type = order[max(i,j)]

	# Special case of mixing short and unsigned short.
	if (type == TY_USHORT && type1 != type2)
	    type = TY_INT

	return (type)
end
