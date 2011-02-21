# Aperture data structure

define	LEN_AP	($1*20)				# Length of DC data structure
define	DC_PL	Memi[$1+($2-1)*20+1]		# Physical line number
define	DC_AP	Memi[$1+($2-1)*20+2]		# Aperture number
define	DC_BM	Memi[$1+($2-1)*20+3]		# Beam number
define	DC_DT	Memi[$1+($2-1)*20+4]		# Dispersion type
define	DC_NW	Memi[$1+($2-1)*20+5]		# Number of pixels in spectrum
define	DC_W1	Memd[P2D($1+($2-1)*20+6)]	# Wavelength of first pixel
define	DC_W2	Memd[P2D($1+($2-1)*20+8)]	# Wavelength of last pixel
define	DC_DW	Memd[P2D($1+($2-1)*20+10)]	# Wavelength interval per pixel
define	DC_Z	Memd[P2D($1+($2-1)*20+12)]	# Redshift
define	DC_LW	Memr[P2R($1+($2-1)*20+14)]	# Aperture lower limit (2)
define	DC_UP	Memr[P2R($1+($2-1)*20+16)]	# Aperture upper limit (2)
define	DC_CF	Memi[$1+($2-1)*20+18]		# Pointer to coefficients
define	DC_UN	Memi[$1+($2-1)*20+19]		# Units
