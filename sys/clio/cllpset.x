# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"clpset.h"

# CLLPSET -- List a pset.  Each param,value pair is written to the output
# file using the caller supplied format, e.g., "set %s = \"%s\"\n".

procedure cllpset (pp, fd, format)

pointer	pp			#I pset descriptor
int	fd			#I output file
char	format[ARB]		#I format, one %s each for param, value

begin
	call clc_compress()
	call clc_list (fd, PS_PSETNAME(pp), format)
end
