# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETS -- Get the short integer value of a pset parameter.

short procedure clgpsets (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
short	clgets()

begin
	return (clgets (PARNAME(pp,parname)))
end
