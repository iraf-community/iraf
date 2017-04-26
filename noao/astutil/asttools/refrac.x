include	<math.h>


# REFRAC -- Compute observed place from apparent place.
#
# This is a placeholder routine.  I am not completely sure this is
# done correctly though the SLALIB routines are accurate.
# This uses the quick (less precise) SLALIB routines for the
# calculation.

procedure refrac (ara, adec, aha, lat, w, t, p, h, ora, odec)

double	ara, adec			# Apparent ra (hr) and dec (deg)
double	aha				# Apparent hour angle (hr)
double	lat				# Latitude (deg)
double	w				# Effective wavelength (A)
double	t				# Temperature (C)
double	p				# Pressure (mbar)
double	h				# Humidity (frac 0-1)
double	ora, odec			# Observed ra (hr) and dec (deg)

double	oha, refa, refb, az, el, vu[3], vr[3]

begin
	# Determine refraction coefficients.
	call slRFCQ (t+273.15D0, p, h, w/10000D0, refa, refb)

	# Convert (aha,adec) to (az,el).
	call slDE2H (DEGTORAD(aha*15D0), DEGTORAD(adec), DEGTORAD(lat), az, el)

	# Convert (az,el) to (x,y,z).
	call slDS2C (az, el, vu)

	# Apply refraction correction.
	call slREFV (vu, refa, refb, vr)

	# Convert (x,y,z) to (az,el)
	call slDC2S (vr, az, el)

	# Convert (az,el) to (ha,dec).
	call slDH2E (az, el, DEGTORAD(lat), oha, odec)

	# Convert (oha,odec) to (ora,odec).
	oha = RADTODEG(oha) / 15D0
	odec = RADTODEG(odec)
	ora = ara + aha - oha
	if (ara - ora < -12)
	    ora = ora + 24
	else if (ara - ora > 12)
	    ora = ora - 24
end
