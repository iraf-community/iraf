# SHDR - Spectrum header data structure

define	LEN_SHDR	202
define	LEN_SHDRS	79			# Length of strings

define	SPECTRUM	Memc[P2C($1)]		# Spectrum image name
define	TITLE		Memc[P2C($1+40)]	# Title
define	LABEL		Memc[P2C($1+80)]	# Dispersion label
define	UNITS		Memc[P2C($1+120)]	# Dispersion units
define	IM		Memi[$1+160]		# IMIO pointer
define	MW		Memi[$1+161]		# MWCS pointer
define	CTLW		Memi[$1+162]		# MWCS logical -> world
define	CTWL		Memi[$1+163]		# MWCS world -> logical
define	CTLW1		Memi[$1+164]		# MWCS logical -> world
define	CTWL1		Memi[$1+165]		# MWCS world -> logical
define	UN		Memi[$1+166]		# UNITS pointer for SX
define	MWUN		Memi[$1+167]		# UNITS pointer of MWCS
define	FORMAT		Memi[$1+168]		# Spectrum format
define	NSUM		Memi[$1+169]		# Sum for TWODSPEC format
define	TYPE		Memi[$1+170]		# Spectrum type
define	AAXIS		Memi[$1+171]		# Aperture axis
define	DAXIS		Memi[$1+172]		# Logical dispersion axis
define	DAXISP		Memi[$1+173]		# Physical dispersion axis
define	NDIM		Memi[$1+174]		# Logical image dimension
define	INDEX1		Memi[$1+175]		# Logical image index
define	INDEX2		Memi[$1+176]		# Logical image index
define	PNDIM		Memi[$1+177]		# Physical (WCS) dimension
define	PINDEX1		Memi[$1+178]		# Physical image index
define	PINDEX2		Memi[$1+179]		# Physical image index
define	AP		Memi[$1+180]		# Aperture ID
define	BEAM		Memi[$1+181]		# Beam ID
define	APLOW		Memr[$1+182]		# Aperture lower limit
define	APHIGH		Memr[$1+183]		# Aperture lower limit
define	IT		Memr[$1+184]		# Integ. time
define	RA		Memr[$1+185]		# Right ascension
define	DEC		Memr[$1+186]		# Declination
define	UT		Memr[$1+187]		# Universal time
define	ST		Memr[$1+188]		# Siderial time
define	HA		Memr[$1+189]		# Hour angle
define	AM		Memr[$1+190]		# Airmass
define	W0		Memr[$1+191]		# Starting wavelength
define	W1		Memr[$1+192]		# Ending wavelength
define	WP		Memr[$1+193]		# Wavelength increment per pixel
define	DC		Memi[$1+194]		# Dispersion correction
define	EC		Memi[$1+195]		# Extinction correction
define	FC		Memi[$1+196]		# Flux calibration

define	SX		Memi[$1+197]		# Pointer to X values
define	SY		Memi[$1+198]		# Pointer to Y values
define	SN		Memi[$1+199]		# Number of pixels
define	NP1		Memi[$1+200]		# First logical pixel
define	NP2		Memi[$1+201]		# Last logical pixel


# Access modes for SHDR_OPEN
define	SHDATA		1	# Get header and pixel data
define	SHHDR		2	# Get header only

# Formats
define	MULTISPEC	1
define	ONEDSPEC	2
define	TWODSPEC	3

# Types
define	SKY		0
define	OBJECT		1
define	ARC		2
define	FLAT		3

# Dispersion correction
define	DCNO		-1
define	DCLINEAR	0
define	DCLOG		1
define	DCFUNC		2

# Extinction correction
define	ECNO		-1
define	ECYES		0

# Flux calibration
define	FCNO		-1
define	FCYES		0
