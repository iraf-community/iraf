# Definition for units package

define	UN_LEN		126			# Length of units structure
define	SZ_UNITS	79			# Length of units strings

define	UN_TYPE		Memi[$1]		# Unit type code
define	UN_CLASS	Memi[$1+1]		# Unit class code
define	UN_LOG		Memi[$1+2]		# Log code
define	UN_INV		Memi[$1+3]		# Inverse code
define	UN_SCALE	Memr[P2R($1+4)]		# Scale factor
define	UN_VREF		Memr[P2R($1+5)]		# Ref lambda (ang) for velocity
define	UN_LABEL	Memc[P2C($1+6)]		# Unit label
define	UN_UNITS	Memc[P2C($1+46)]	# Units string
define	UN_USER		Memc[P2C($1+86)]	# User units string 

# Unit classes
define	UN_UNKNOWN	0			# Unknown
define	UN_WAVE		1			# Wavelength
define	UN_FREQ		2			# Frequency
define	UN_VEL		3			# Velocity
define	UN_ENERGY	4			# Energy
define	UN_DOP		5			# Doppler shift

# Unit types
define	UN_NUNITS	17
define	UN_DIC	"|angstroms|nanometers|millimicrons|microns|millimeters\
		 |centimeters|meters|hertz|kilohertz|megahertz|gigahertz\
		 |m/s|km/s|ev|kev|mev|z|log|inverse|"

# Unit scales: Conversions from Angstroms
define	UN_ANG		1.0		# angstroms
define	UN_NM		0.1		# nanometers
define	UN_MMIC		0.1		# millimicrons
define	UN_MIC		1E-4		# microns
define	UN_MM		1E-7		# millimeters
define	UN_CM		1E-8		# centimeter
define	UN_M		1E-10		# meters
define	UN_HZ		2.9979E18	# hertz
define	UN_KHZ		2.9979E15	# kilohertz
define	UN_MHZ		2.9979E12	# megahertz
define	UN_GHZ		2.9979E9	# gigaertz
define	UN_MPS		2.9979E8	# m/s
define	UN_KPS		2.9979E5	# km/s
define	UN_EV		1.2396E4	# ev
define	UN_KEV		1.2396E1	# kev
define	UN_MEV		1.2396E-2	# mev
define	UN_Z		1.0		# doppler shift

# Unit abbreviations
define	ABBREVIATIONS	"noao$lib/units.dat"
