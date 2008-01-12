include	<math.h>

# The sun's velocity with respect to the local standard of rest is:

define	RAVSUN	18.	# RA of sun's velocity
define	DECVSUN	30.	# DEC of sun's velocity
define	VSUN	20.	# VLSR of sun
define	EPOCH	1900.	# Epoch of sun's velocity

# AST_VSUN -- Projection of the sun's velocity along the given direction.

procedure ast_vsun (ra, dec, epoch, v) 

double	ra		# Reference right ascension (hours)
double	dec		# Reference declination (degrees)
double	epoch		# Epoch (years)
double	v		# VLSR of sun along reference direction

double	ravsun, decvsun, vx, vy, vz, cc, cs, s

begin
	# Precess VLSR direction to current date.
	call ast_precess (double (RAVSUN), double (DECVSUN), double (EPOCH),
	    ravsun, decvsun, epoch)

	# Cartisian velocity components of the sun's velocity.
	vx = VSUN * cos (DEGTORAD (15. * ravsun)) * cos (DEGTORAD (decvsun))
	vy = VSUN * sin (DEGTORAD (15. * ravsun)) * cos (DEGTORAD (decvsun))
	vz = VSUN * sin (DEGTORAD (decvsun))

	# Direction cosines along the reference direction.
	cc = cos (DEGTORAD (dec)) * cos (DEGTORAD (15. * ra))
	cs = cos (DEGTORAD (dec)) * sin (DEGTORAD (15. * ra))
	s  = sin (DEGTORAD (dec))

	# Project sun's motion along the reference direction.
	v = -(vx * cc + vy * cs + vz * s)
end
