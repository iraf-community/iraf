# Aperture data structure

define	LEN_AP	1+($1*16)			# Length of data structure
define	PL	Memi[$1+($2-1)*16+1]		# Physical line number
define	AP	Memi[$1+($2-1)*16+2]		# Aperture number
define	BM	Memi[$1+($2-1)*16+3]		# Beam number
define	DT	Memi[$1+($2-1)*16+4]		# Dispersion type
define	NW	Memi[$1+($2-1)*16+5]		# Number of pixels in spectrum
define	W1	Memd[P2D($1+($2-1)*16+6)]	# Wavelength of first pixel
define	W2	Memd[P2D($1+($2-1)*16+8)]	# Wavelength of last pixel
define	DW	Memd[P2D($1+($2-1)*16+10)]	# Wavelength interval per pixel
define	Z	Memd[P2D($1+($2-1)*16+12)]	# Redshift
define	LW	Memd[P2D($1+($2-1)*16+14)]	# Aperture lower limit
define	UP	Memd[P2D($1+($2-1)*16+16)]	# Aperture upper limit
