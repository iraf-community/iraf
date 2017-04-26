# Definition for funits package

define	FUN_LEN		124			# Length of funits structure
define	SZ_UNITS	79			# Length of funits strings

define	FUN_TYPE	Memi[$1]		# Unit type code
define	FUN_CLASS	Memi[$1+1]		# Unit class code
define	FUN_MOD		Memi[$1+2]		# Modifier code
define	FUN_SCALE	Memr[P2R($1+3)]		# Scale factor
define	FUN_LABEL	Memc[P2C($1+4)]		# Unit label
define	FUN_UNITS	Memc[P2C($1+44)]	# Units string
define	FUN_USER	Memc[P2C($1+84)]	# User units string 

# Unit classes
define	FUN_UNKNOWN	0			# Unknown
define	FUN_FREQ	1			# Frequency
define	FUN_WAVE	2			# Wavelength

# Unit modifiers
define	FUN_LOG		1			# Log10
define	FUN_MAG		2			# Mag

# Unit types
define	FUN_NUNITS	4
define	FUN_DIC	"|jansky|fu|erg/cm2/s/hz|erg/cm2/s/a|log|mag|"

# Unit scales: Conversions from Jansky
define	FUN_J		1.0		# Jansky (W/m2/s/Hz)
define	FUN_FU		1E26		# Flux units (W/m2/s/Hz*1E26)
define	FUN_CGSH	1e3		# CGS units (erg/cm2/s/Hz)
define	FUN_CGSA	1e3		# CGS units (erg/cm2/s/A)

define	FUN_VLIGHT	2.997925e18	# V light A/s
