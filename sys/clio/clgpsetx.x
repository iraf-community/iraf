# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETX -- Get the complex value of a pset parameter.

complex procedure clgpsetx (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
complex	clgetx()

begin
	return (clgetx (PARNAME(pp,parname)))
end
