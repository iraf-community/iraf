define	KMAX		20		# maximum order spline permitted

# Spline descriptor structure -- stored at beginning of BSPLN array.
# All of the information needed to describe the spline is collected
# in one place.  Space is left in the header for expansion.
# Space required:  REAL BSPLN [2*N+30]

define	NCOEF		bspln[1]	# number of coeff in the spline
define	ORDER		bspln[2]	# order of spline (cubic = 4)
define	XMIN		bspln[3]	# minimum x-value
define	XMAX		bspln[4]	# maximum x-value
define	KINDEX		bspln[5]	# position during evaluation (SEVAL)
define	KNOT1		NCOEF+10	# offset to the first knot
define	COEF1		10		# offset to the first coefficient
