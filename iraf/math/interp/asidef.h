# defines for use with interpolator package
# intended for internal consumption only
# used to set up storage in coeff array

define	TYPEI		coeff[1]	# code for interpolator type
define	NPTS		coeff[2]	# no. of data points

define	ITYPEI		int(coeff[1])
define	INPTS		int(coeff[2])

define ASITYPEERR	1
define ASIFITERR	2

define COFF		10		# offset into coeff array
					# beware coeff[COFF - 2] is first
					# location used !!
