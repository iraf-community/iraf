# MATH.H -- Definitions of various mathematical constants.
# Values are given to 20 decimal places.
# From Abramowitz & Stegun, Handbook of Mathematical Functions, Ch. 1.
# LN	denotes	natural logarithm
# LOG	denote	base 10 logarithm


# Real precision definitions.

define	SQRTOF2		1.4142135623730950488

define	BASE_E		2.7182818284590452353
define	EXP_PI		23.140692632779269006

define	LN_2		.69314718055994530942
define	LN_10		2.3025850929940456840
define	LN_PI		1.1447298858494001741
define	LOG_E		.43429448190325182765

define	PI		3.1415926535897932385
define	TWOPI		6.2831853071795864769
define	FOURPI		12.566370614359172953
define	HALFPI		1.5707963267948966192
define	SQRTOFPI	1.7724538509055160273

define	RADIAN		57.295779513082320877
define	RADTODEG	(($1)*RADIAN)
define	DEGTORAD	(($1)/RADIAN)

define	GAMMA		.57721566490153286061		# Euler's constant
define	LN_GAMMA	(-.54953931298164482234)
define	EXP_GAMMA	1.7810724179901979852


# Double precision definitions.

define	DSQRTOF2	1.4142135623730950488d0

define	DBASE_E		2.7182818284590452353d0
define	DEXP_PI		23.140692632779269006d0

define	DLN_2		.69314718055994530942d0
define	DLN_10		2.3025850929940456840d0
define	DLN_PI		1.1447298858494001741d0
define	DLOG_E		.43429448190325182765d0

define	DPI		3.1415926535897932385d0
define	DTWOPI		6.2831853071795864769d0
define	DFOURPI		12.566370614359172953d0
define	DHALFPI		1.5707963267948966192d0
define	DSQRTOFPI	1.7724538509055160273d0

define	DRADIAN		57.295779513082320877d0
define	DRADTODEG	(($1)*DRADIAN)
define	DDEGTORAD	(($1)/DRADIAN)

define	DGAMMA		.57721566490153286061d0		# Euler's constant
define	DLN_GAMMA	(-.54953931298164482234d0)
define	DEXP_GAMMA	1.7810724179901979852d0
