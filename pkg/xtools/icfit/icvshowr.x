# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"icfit.h"

# IC_VSHOW -- Show fit parameters in verbose mode.

procedure ic_vshowr (ic, file, cv, x, y, wts, npts, gt)

pointer	ic		# ICFIT pointer
char	file[ARB]	# Output file
pointer	cv		# Curfit pointer
real	x[ARB]		# Ordinates
real	y[ARB]		# Abscissas
real	wts[ARB]	# Weights
int	npts		# Number of data points
pointer	gt		# Graphics tools pointer

int	fd, open()
errchk	open, ic_fvshowr

begin
	fd = open (file, APPEND, TEXT_FILE)
	IC_GT(ic) = gt
	call ic_fvshowr (ic, cv, x, y, wts, npts, fd)
	call close (fd)
end


# IC_XYSHOW -- List data as x, y, fit, weight lines on output.

procedure ic_xyshowr (ic, file, cv, x, y, w, npts)

pointer	ic		# ICFIT pointer
char	file[ARB]		# Output file
pointer	cv			# Pointer to curfit structure
real	x[npts]			# Array of x data values
real	y[npts]			# Array of y data values
real	w[npts]			# Array of weight data values
int	npts			# Number of data values

int	fd, open()
errchk	open, ic_fxyshowr

begin
	fd = open (file, APPEND, TEXT_FILE)
	call ic_fxyshowr (ic, cv, x, y, w, npts, fd)
	call close (fd)
end
