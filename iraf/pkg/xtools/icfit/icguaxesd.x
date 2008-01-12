# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ICG_UAXES -- Set user axis

procedure icg_uaxesd (key, cv, x, y, z, npts, label, units, maxchars)

int	key				# Key for axes
pointer	cv				# CURFIT pointer
double	x[npts]				# Independent variable
double	y[npts]				# Dependent variable
double	z[npts]				# Output values
int	npts				# Number of points
char	label[maxchars]			# Axis label
char	units[maxchars]			# Axis units
int	maxchars			# Maximum chars in label

begin
end
