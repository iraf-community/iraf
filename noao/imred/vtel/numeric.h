# Structure for argument list to subroutine 'numeric'.

define	VT_LENNUMSTRUCT	8		# Length of VT num structure

define	VT_DLODX	Memr[$1]	# deriv longitude wrt x
define	VT_DLATDY	Memr[$1+1]	# deriv latitude wrt y
define	VT_LATTOP	Memr[$1+2]	# latitude of top of output pixel
define	VT_LATBOT	Memr[$1+3]	# latitude of bottom of output pixel
define	VT_LOLEFT	Memr[$1+4]	# longitude of left side of out pixel
define	VT_LORITE	Memr[$1+5]	# longitude of right side of out pixel
define	VT_LATMID	Memr[$1+6]	# latitude of middle of output pixel
define	VT_LOMID	Memr[$1+7]	# longitude of middle of output pixel
