# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"icfit.h"

# IC_SHOW -- Show the values of the parameters.

procedure ic_show (ic, file, gt)

pointer	ic			# ICFIT pointer
char	file[ARB]		# Output file
pointer	gt			# GTOOLS pointer

int	fd, open()
errchk	open, ic_fshow

begin
	fd = open (file, APPEND, TEXT_FILE)
	IC_GT(ic) = gt
	call ic_fshow (ic, fd)
	call close (fd)
end
