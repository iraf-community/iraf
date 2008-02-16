# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLOPSET -- Open a named pset.

pointer procedure clopset (pset)

char	pset[ARB]		# pset name (name of CL pset parameter)
pointer	pp
errchk	malloc

begin
	call malloc (pp, LEN_PSETDES, TY_STRUCT)
	call strcpy (pset, PS_PSETNAME(pp), SZ_PSPSETNAME)

	return (pp)
end
