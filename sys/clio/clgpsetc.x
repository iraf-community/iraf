# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLGPSETC -- Get the char value of a pset parameter.

char procedure clgpsetc (pp, parname)

pointer	pp			# pset descriptor
char	parname[ARB]		# parameter name

pointer	clpset_parname()
char	clgetc()

begin
	return (clgetc (PARNAME(pp,parname)))
end
