# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# QP_ELEMENTSIZE -- Determine the size in chars of a QPOE datatype.  This may
# be one of the special datatypes (user defined record types), or a primitive
# type.

int procedure qp_elementsize (qp, datatype, reftype)

pointer	qp			#I QPOE descriptor
char	datatype[ARB]		#I symbolic datatype name
int	reftype			#I type of reference (immediate or instanceof)

pointer	dsym
int	dtype
int	qp_sizeof(), qp_dtype()

begin
	dtype = qp_dtype (qp, datatype, dsym)
	return (qp_sizeof (qp, dtype, dsym, reftype))
end
