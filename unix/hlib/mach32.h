# Machine Parameters

define	SZB_CHAR	2		# machine bytes per char
define	SZB_ADDR	1		# machine bytes per address increment
define	SZ_VMPAGE	256		# page size (1 if no virtual mem.)
define	MAX_DIGITS 	25		# max digits in a number
define	NDIGITS_RP	7		# number of digits of real precision
define	NDIGITS_DP 	16		# number of digits of precision (double)
define	MAX_EXPONENT	38		# max exponent, base 10
define	MAX_EXPONENTR	38		# IEEE single
define	MAX_EXPONENTD	308		# IEEE double

define	MAX_SHORT	32767		# largest numbers
define	MAX_INT		2147483647
define	MAX_LONG	2147483647
define	MAX_REAL	0.99e37		# anything larger is INDEF
define	MAX_DOUBLE	0.99d307
define	NBITS_BYTE	8		# nbits in a machine byte
define	NBITS_SHORT	16		# nbits in a short	
define	NBITS_INT	32		# nbits in an integer	
define	NBITS_LONG	32		# nbits in a long	
define	EPSILONR	(1.192e-7)	# smallest E such that 1.0 + E > 1.0
define	EPSILOND	(2.220d-16)	# double precision epsilon
define	EPSILON		EPSILONR
define	IEEE_USED	YES

include <swap.h>
