# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETL -- Get the long integer value of a pset parameter.

long procedure clgpsetl (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
long	clgetl()

begin
	return (clgetl (PARNAME(pp,parname)))
end
