# definitions for the curfit package

# define the permitted types of curves

define	CV_FUNCTIONS	"|chebyshev|legendre|spline3|spline1|"
define	CHEBYSHEV	1
define	LEGENDRE	2
define	SPLINE3		3
define	SPLINE1		4
define	USERFNC		5
define	NTYPES		5

# define the weighting flags

define	CV_WEIGHTS	"|user|uniform|spacing|chisq|"
define	WTS_USER	1	# user enters weights
define	WTS_UNIFORM	2	# equal weights
define	WTS_SPACING	3	# weight proportional to spacing of data points
define	WTS_CHISQ	4	# chi-squared weights (input data in photons)

# error conditions

define	SINGULAR	1
define	NO_DEG_FREEDOM	2

# definitions for cvstat

define	CVTYPE		1	# curve type
define	CVORDER		2	# order
define	CVNCOEFF	3	# Number of coefficients
define	CVNSAVE		4	# Length of save buffer
define	CVXMIN		5	# minimum ordinate
define	CVXMAX		6	# maximum ordinate

