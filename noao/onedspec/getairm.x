# GET_AIRM -- Derive airmass value from telescope data if possible
#             Otherwise return INDEF
#
# Note that HA must be negative to the East.
# If HA is not reasonable, then ST-RA is used

procedure get_airm (ra, dec, ha, st, latitude, airm)

real	ra, dec, ha, st, latitude, airm

begin
	# Verify realistic value for HA

	if (IS_INDEF (ha)) {
	    if (IS_INDEF (st) || IS_INDEF (ra))
		call error (0, "Can't determine airmass")

	    ha = st - ra
	}

	# Now verify DEC
	if (IS_INDEF (dec))
	    call error (0, "Can't determine airmass")

	# Everything should be just fine now
	# Compute airmass using method of John Ball

	call airmass (dec, ha, latitude, airm)
end

# AIRMASS -- Compute airmass from RA, DEC, and HA
#
# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133.
# and John Ball's book on Algorithms for the HP-45

procedure airmass (dec, ha, latitude, airm)

real	dec, ha, latitude, airm

real	scale, rads, cos_zd, sin_elev
real	x

data	rads /57.29577951/		# Degrees per radian
data	scale/750.0      /		# Atmospheric scale height approx

begin
	cos_zd = sin (latitude/rads) * sin (dec/rads) +
	    cos (latitude/rads) * cos (dec/rads) * cos (ha*15/rads)

	sin_elev = cos_zd		# SIN of elev = cos of Zenith dist

	x  = scale * sin_elev
	airm = sqrt (x**2 + 2*scale + 1) - x
end
