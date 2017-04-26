# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETB -- Get the boolean value of a pset parameter.

bool procedure clgpsetb (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
bool	clgetb()

begin
	return (clgetb (PARNAME(pp,parname)))
end
