# TRANSFORM -- Common task parameters.

int	itype			# Interpolation type
real	u1, v1			# Starting coordinates
real	u2, v2			# Ending coordinates
real	du, dv			# Coordinate intervals
int	nu, nv			# Number of pixels
bool	ulog, vlog		# Logrithmic coordinates?
bool	flux			# Conserve flux per pixel?
bool	usewcs			# Use WCS?
real	blank			# Blank value

common	/trcom/ u1, v1, u2, v2, du, dv, nu, nv, itype, ulog, vlog,
		flux, usewcs, blank
