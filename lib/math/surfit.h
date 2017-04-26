# SURFIT.H -- Global defines for the surface fitting package.

# Permitted types of curves.

define	SF_FUNCTIONS	"|legendre|chebyshev|spline3|spline1|"
define	SF_LEGENDRE	1
define	SF_CHEBYSHEV	2
define	SF_SPLINE3	3
define	SF_SPLINE1	4
define	SF_NTYPES	4

# Weighting flags

define	SF_WEIGHTS	"|user|uniform|"
define	SF_USER		1	# user enters weights
define	SF_UNIFORM	2	# equal weights, weight 1.0

# Error conditions

define	SINGULAR	1	# matrix is singular
define	NO_DEG_FREEDOM	2	# insufficient number of data points
