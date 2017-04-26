# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"icfit.h"
include	"names.h"

# IC_LIST -- List X, Y, FIT, W.

procedure ic_listd (ic, cv, x, y, wts, npts, file)

pointer	ic		# ICFIT pointer
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points
char	file[ARB]	# Output file

int	i, fd, open()
double	dcveval()
errchk	open()

begin
	# Open the output file.
	fd = open (file, APPEND, TEXT_FILE)

	if (npts == IC_NFIT(ic)) {
	    do i = 1, npts {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargd (x[i])
		    call pargd (y[i])
		    call pargd (dcveval (cv, x[i]))
		    call pargd (wts[i])
	    }
	} else {
	    do i = 1, IC_NFIT(ic) {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargd (Memd[IC_XFIT(ic)+i-1])
		    call pargd (Memd[IC_YFIT(ic)+i-1])
		    call pargd (dcveval (cv, Memd[IC_XFIT(ic)+i-1]))
		    call pargd (Memd[IC_WTSFIT(ic)+i-1])
	    }
	}

	call close (fd)
end
