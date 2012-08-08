# The definitions file for the least squares fitting routines.

define		MAX_NFITPARS	7	# number of parameters following

define		YINCPT		$1[1]	# y-intercept
define		EYINCPT		$1[2]	# error in y-intercept
define		SLOPE		$1[3]	# slope of fit
define		ESLOPE		$1[4]	# error in slope
define		CHI		$1[5]	# mean error of unit weight
define		RMS		$1[6]	# mean error of unit weight

#define		ME1		$1[1]	# mean error of unit weight
#define		OFFSET		$1[2]	# intercept
#define		EOFFSET		$1[3]	# error in intercept
#define		SLOPE1		$1[4]	# slope of fit to first variable
#define		ESLOPE1		$1[5]	# error in slope1
#define		SLOPE2		$1[6]	# slope of fit to second variable
#define		ESLOPE2		$1[7]	# error in slope2
