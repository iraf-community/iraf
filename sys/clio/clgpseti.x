# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETI -- Get the int value of a pset parameter.

int procedure clgpseti (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
int	clgeti()

begin
	return (clgeti (PARNAME(pp,parname)))
end
