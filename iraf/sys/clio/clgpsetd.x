# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETD -- Get the double value of a pset parameter.

double procedure clgpsetd (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
double	clgetd()

begin
	return (clgetd (PARNAME(pp,parname)))
end
