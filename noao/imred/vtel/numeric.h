# Structure for argument list to subroutine 'numeric'.

define	VT_LENNUMSTRUCT	8		# Length of VT num structure

define	VT_DLODX	Memr[P2R($1)]	# deriv longitude wrt x
define	VT_DLATDY	Memr[P2R($1+1)]	# deriv latitude wrt y
define	VT_LATTOP	Memr[P2R($1+2)]	# latitude of top of output pixel
define	VT_LATBOT	Memr[P2R($1+3)]	# latitude of bottom of output pixel
define	VT_LOLEFT	Memr[P2R($1+4)]	# longitude of left side of out pixel
define	VT_LORITE	Memr[P2R($1+5)]	# longitude of right side of out pixel
define	VT_LATMID	Memr[P2R($1+6)]	# latitude of middle of output pixel
define	VT_LOMID	Memr[P2R($1+7)]	# longitude of middle of output pixel
