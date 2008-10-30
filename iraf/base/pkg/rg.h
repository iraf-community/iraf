# Definitions for the RANGES structure.

define	LEN_RG		2

define	RG_NPTS		Memz[P2Z($1)]		# Number of points in ranges
define	RG_NRGS		Memz[P2Z($1+1)]		# Number of range intervals
define	RG_X1		Meml[P2L($1+2)+2*($2-1)]	# Start of interval $2
define	RG_X2		Meml[P2L($1+2)+2*($2-1)+1]	# End of interval $2
