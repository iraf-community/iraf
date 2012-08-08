# Wavelength table structure
define	TBL_LEN		14
define	TBL_W1		Memd[P2D($1)]	# Starting wavelength
define	TBL_W2		Memd[P2D($1+2)]	# Ending wavelength
define	TBL_DW		Memd[P2D($1+4)]	# Wavelength interval
define	TBL_WMIN	Memd[P2D($1+6)]	# Minimum wavelength for global
define	TBL_WMAX	Memd[P2D($1+8)]	# Maximum wavelength for global
define	TBL_AP		Memi[$1+10]	# Aperture
define	TBL_NW		Memi[$1+11]	# Number of points
define	TBL_NWMAX	Memi[$1+12]	# Maximum number of points for global
define	TBL_CONFIRM	Memi[$1+13]	# Confirm?
