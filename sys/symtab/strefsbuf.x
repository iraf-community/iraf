# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STREFSBUF -- Convert an offset into SBUF into a pointer to char.

pointer procedure strefsbuf (stp, offset)

pointer	stp			# symtab descriptor
int	offset			# offset into SBUF

begin
	return (ST_SBUFP(stp) + offset)
end
