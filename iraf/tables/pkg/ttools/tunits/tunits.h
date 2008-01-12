# TUNITS.H -- Structure and constants used by tunits

define	MAXUNIT		8
define	LEN_UNIT	15

define	LEN_TUNSTRUCT	(SZ_DOUBLE+2*MAXUNIT)

define	TUN_FACTOR	Memd[P2D($1)]			# conversion factor
define	TUN_UNPTR	Memi[$1+SZ_DOUBLE+$2]		# ptr to units string
define	TUN_POWER	Memi[$1+SZ_DOUBLE+MAXUNIT+$2]	# units power

define	TUN_UNITS	Memc[TUN_UNPTR($1,$2)]		# units string

define	FINALS		"m,kg,s,rad,hz"
