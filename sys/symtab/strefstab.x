# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"symtab.h"

# STREFSTAB -- Convert an offset into STAB into a pointer to int.

pointer procedure strefstab (stp, offset)

pointer	stp			# symtab descriptor
int	offset			# offset into STAB

begin
	return (ST_STABP(stp) + offset)
end
