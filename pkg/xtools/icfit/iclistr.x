# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"icfit.h"
include	"names.h"

# IC_LIST -- List X, Y, FIT, W.

procedure ic_listr (ic, cv, x, y, wts, npts, file)

pointer	ic		# ICFIT pointer
pointer	cv		# Curfit pointer
real	x[ARB]		# Ordinates
real	y[ARB]		# Abscissas
real	wts[ARB]	# Weights
int	npts		# Number of data points
char	file[ARB]	# Output file

int	i, fd, open()
real	rcveval()
errchk	open()

begin
	# Open the output file.
	fd = open (file, APPEND, TEXT_FILE)

	if (npts == IC_NFIT(ic)) {
	    do i = 1, npts {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargr (x[i])
		    call pargr (y[i])
		    call pargr (rcveval (cv, x[i]))
		    call pargr (wts[i])
	    }
	} else {
	    do i = 1, IC_NFIT(ic) {
		call fprintf (fd, "%8g %8g %8g %8g\n")
		    call pargr (Memr[IC_XFIT(ic)+i-1])
		    call pargr (Memr[IC_YFIT(ic)+i-1])
		    call pargr (rcveval (cv, Memr[IC_XFIT(ic)+i-1]))
		    call pargr (Memr[IC_WTSFIT(ic)+i-1])
	    }
	}

	call close (fd)
end
