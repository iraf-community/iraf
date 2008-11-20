# XT_FIXPIX data structure.
define	FP_LEN		13		# Length of FP structure
define	FP_PM		Memp[$1]	# Pixel mask pointer
define	FP_LVAL		Memi[P2I($1+1)]	# Mask value for line interpolation
define	FP_CVAL		Memi[P2I($1+2)]	# Mask value for column interpolation
define	FP_NCOLS	Memz[P2Z($1+3)]	# Number of columns to interpolate
define	FP_PCOL		Memp[$1+4]	# Pointer to columns
define	FP_PL1		Memp[$1+5]	# Pointer to start lines
define	FP_PL2		Memp[$1+6]	# Pointer to end lines
define	FP_PV1		Memp[$1+7]	# Pointer to start values	
define	FP_PV2		Memp[$1+8]	# Pointer to end values	
define	FP_LMIN		Meml[P2L($1+9)]	# Minimum line
define	FP_LMAX		Meml[P2L($1+10)]	# Maximum line
define	FP_PIXTYPE	Memi[P2I($1+11)]	# Pixel type for values
define	FP_DATA		Memp[$1+12]	# Data values

define	FP_COL		Meml[FP_PCOL($1)+$2-1]
define	FP_L1		Meml[FP_PL1($1)+$2-1]
define	FP_L2		Meml[FP_PL2($1)+$2-1]
define	FP_V1		(FP_PV1($1)+$2-1)
define	FP_V2		(FP_PV2($1)+$2-1)

define	FP_LDEF		1		# Default line interpolation code
define	FP_CDEF		2		# Default column interpolation code
