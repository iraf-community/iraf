# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETR -- Get the real value of a pset parameter.

real procedure clgpsetr (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
real	clgetr()

begin
	return (clgetr (PARNAME(pp,parname)))
end
