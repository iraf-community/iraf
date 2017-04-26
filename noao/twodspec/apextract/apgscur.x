include	"apertures.h"

# AP_GSCUR -- Set the graphics cursor to the aperture given by the index.
# It computes the position of the cursor for the specified dispersion line.

procedure ap_gscur (index, gp, line, aps, y)

int	index			# Index of aperture
pointer	gp			# GIO pointer
int	line			# Dispersion line
pointer	aps[ARB]		# Apertures
real	y			# Y cursor coordinate

int	apaxis
real	x
pointer	ap

real	ap_cveval()

begin
	if (index < 1 || IS_INDEF (y))
	    return

	ap = aps[index]
	apaxis = AP_AXIS(ap)
	x = AP_CEN(ap, apaxis) + ap_cveval (AP_CV(ap), real (line))
	call gscur (gp, x, y)
end
