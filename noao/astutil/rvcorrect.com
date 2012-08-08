double	latitude, longitude, altitude	# Location of observation
double	vs				# Solar velocity
double	ras, decs, eps			# Coordinate of solar velocity

common	/rvc_com/ latitude, longitude, altitude, vs, ras, decs, eps
