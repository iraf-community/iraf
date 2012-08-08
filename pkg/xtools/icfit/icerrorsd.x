# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"names.h"

# IC_ERRORS -- Compute and error diagnositic information.

procedure ic_errorsd (ic, file, cv, x, y, wts, npts)

pointer	ic		# ICFIT pointer
char	file[ARB]	# Output file
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points

int	fd, open()
errchk	open, ic_ferrorsd

begin
	fd = open (file, APPEND, TEXT_FILE)
	call ic_ferrorsd (ic, cv, x, y, wts, npts, fd)
	call close (fd)
end
