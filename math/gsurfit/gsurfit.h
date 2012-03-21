# definitions for the gsurfit package

# define the permitted types of curves

define	GS_FUNCTIONS	"|chebyshev|legendre|polynomial|"
define	GS_CHEBYSHEV	1	# chebyshev polynomials
define	GS_LEGENDRE	2	# legendre polynomials
define	GS_POLYNOMIAL	3	# power series polynomials
define	NTYPES		3

# define the xterms flags

define	GS_XTYPES	"|none|full|half|"
define	GS_XNONE	0	# no x-terms (old NO)
define	GS_XFULL	1	# full x-terms (new YES)
define	GS_XHALF	2	# half x-terms (new)

# define the weighting flags

define	GS_WEIGHTS	"|user|uniform|spacing|"
define	WTS_USER	1	# user enters weights
define	WTS_UNIFORM	2	# equal weights
define	WTS_SPACING	3	# weight proportional to spacing of data points

# error conditions

define	SINGULAR	1
define	NO_DEG_FREEDOM	2

# gsstat/gsset definitions

define	GSTYPE		1
define	GSXORDER	2
define	GSYORDER	3
define	GSXTERMS	4
define	GSNXCOEFF	5
define	GSNYCOEFF	6
define	GSNCOEFF	7
define	GSNSAVE		8
define	GSXMIN		9
define	GSXMAX		10
define	GSYMIN		11
define	GSYMAX		12
define	GSXREF		13
define	GSYREF		14
define	GSZREF		15

define	GS_SAVECOEFF	8
