# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# GSCHUP -- Set character up vector.

procedure gschup (chux, chuy)

real	chux, chuy		# Character up vector, in world coordinates
int	char_up
bool	fp_equalr()

begin
	# Find the angle normal to the text baseline.  The angle is stored
	# in degrees between -180 and +180.

	if (fp_equalr (chux, 0.0))
	    char_up = 90
	else
	    char_up = nint (atan2 (chuy, chux) * 180. / 3.1415926)

	call gsawi (G_TXUP, char_up)
end
