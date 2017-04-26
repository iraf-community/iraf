# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"icfit.h"

# IC_VSHOW -- Show fit parameters in verbose mode.

procedure ic_vshowd (ic, file, cv, x, y, wts, npts, gt)

pointer	ic		# ICFIT pointer
char	file[ARB]	# Output file
pointer	cv		# Curfit pointer
double	x[ARB]		# Ordinates
double	y[ARB]		# Abscissas
double	wts[ARB]	# Weights
int	npts		# Number of data points
pointer	gt		# Graphics tools pointer

int	fd, open()
errchk	open, ic_fvshowd

begin
	fd = open (file, APPEND, TEXT_FILE)
	IC_GT(ic) = gt
	call ic_fvshowd (ic, cv, x, y, wts, npts, fd)
	call close (fd)
end


# IC_XYSHOW -- List data as x, y, fit, weight lines on output.

procedure ic_xyshowd (ic, file, cv, x, y, w, npts)

pointer	ic		# ICFIT pointer
char	file[ARB]		# Output file
pointer	cv			# Pointer to curfit structure
double	x[npts]			# Array of x data values
double	y[npts]			# Array of y data values
double	w[npts]			# Array of weight data values
int	npts			# Number of data values

int	fd, open()
errchk	open, ic_fxyshowd

begin
	fd = open (file, APPEND, TEXT_FILE)
	call ic_fxyshowd (ic, cv, x, y, w, npts, fd)
	call close (fd)
end
