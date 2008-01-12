include "cqdef.h"


# CQ_GQPAR -- Get the default value, units, and format for a query parameter
# by name.

int procedure cq_gqpar (cq, name, pname, max_name, value, max_val, units,
	max_units, format, max_format)

pointer	cq				#I the catalog descriptor
char	name[ARB]			#I the input query parameter name
char	pname[ARB]			#I the output query parameter name
int	max_name			#I the max size of the parameter name
char	value[ARB]			#O the default value size
int	max_val				#I the max size of the parameter value
char	units[ARB]			#O the units string
int	max_units			#I the max size of the parameter units
char	format[ARB]			#O the format string
int	max_format			#I the max size of the parameter format

pointer	cc
int	parno
int	strdic(), cq_wrdstr()

begin
	# Check that the current catalog is defined.
	if (CQ_CAT(cq) == NULL)
	    return (0)
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    return (0)
	cc = CQ_CAT(cq)

	parno = strdic (name, pname, max_name, Memc[CQ_PQPNAMES(cc)])
	if (parno <= 0)
	    return (0)

	parno = cq_wrdstr (parno, value, max_val, Memc[CQ_PQPDVALUES(cc)])
	if (parno <= 0)
	    return (0)

	parno = cq_wrdstr (parno, units, max_units, Memc[CQ_PQPUNITS(cc)])
	if (parno <= 0)
	    return (0)

	parno = cq_wrdstr (parno, format, max_format, Memc[CQ_PQPFMTS(cc)])
	if (parno <= 0)
	    return (0)

	return (parno)
end


# CQ_GQPARN -- Get the default value, units, and format for a query parameter
# by number.

int procedure cq_gqparn (cq, parno, pname, max_name, value, max_val, units,
	max_units, format, max_format)

pointer	cq				#I the catalog descriptor
int	parno				#I the parameter number
char	pname[ARB]			#I the output query parameter name
int	max_name			#I the max size of the parameter name
char	value[ARB]			#O the default value size
int	max_val				#I the max size of the parameter value
char	units[ARB]			#O the units string
int	max_units			#I the max size of the parameter units
char	format[ARB]			#O the format string
int	max_format			#I the max size of the parameter format

pointer	cc
int	pnum
int	cq_wrdstr()

begin
	# Check that the current catalog is defined.
	if (CQ_CAT(cq) == NULL)
	    return (0)
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    return (0)
	cc = CQ_CAT(cq)

	pnum = cq_wrdstr (parno, pname, max_name, Memc[CQ_PQPNAMES(cc)])
	if (pnum <= 0)
	    return (0)

	pnum = cq_wrdstr (parno, value, max_val, Memc[CQ_PQPDVALUES(cc)])
	if (pnum <= 0)
	    return (0)

	pnum = cq_wrdstr (parno, units, max_units, Memc[CQ_PQPUNITS(cc)])
	if (pnum <= 0)
	    return (0)

	pnum = cq_wrdstr (parno, format, max_format, Memc[CQ_PQPFMTS(cc)])
	if (pnum <= 0)
	    return (0)

	return (pnum)
end
