# Wavelength table structure
define	TBL_LEN		9
define	TBL_AP		Memi[$1]	# Aperture
define	TBL_W1		Memr[$1+1]	# Starting wavelength
define	TBL_W2		Memr[$1+2]	# Ending wavelength
define	TBL_DW		Memr[$1+3]	# Wavelength interval
define	TBL_NW		Memi[$1+4]	# Number of points
define	TBL_WMIN	Memr[$1+5]	# Minimum wavelength for global
define	TBL_WMAX	Memr[$1+6]	# Maximum wavelength for global
define	TBL_NWMAX	Memi[$1+7]	# Maximum number of points for global
define	TBL_CONFIRM	Memi[$1+8]	# Confirm?
