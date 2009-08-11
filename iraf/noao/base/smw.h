# SMW definitions.
# The SMW package (the SMW structure and routines) provide an interface
# between the MWCS and the ONEDSPEC packages.  It provides translation
# between different image formats and a standard interface for the spectral
# tasks.  The types of formats supported are MULTISPEC, EQUISPEC, and 1-3D
# simple linear WCS as well as some older formats.
# 
# The SMW package also provides separating the MULTISPEC MWCS into groups to
# avoid limitations in the number of attributes allowed by MWCS.  This is
# primarily required for dispersion correction.

define	SMW_NSPLIT	500			# Number of spectra per WCS

define	SMW_LEN		(33 + $1)		# Length of SMW structure

define	SMW_FORMAT	Memi[P2I($1)]		# Spectrum format
define	SMW_NSPEC	Meml[P2L($1+1)]		# Number of spectra
define  SMW_NBANDS      Memi[P2I($1+2)]		# Number of associated bands

define	SMW_TRANS	Memi[P2I($1+3)]		# Transposed image?
define	SMW_PDIM	Memi[P2I($1+4)]		# Physical dimension
define	SMW_PAXIS	Memi[P2I($1+5)+$2-1]	# Physical axes [3]

define	SMW_LDIM	Memi[P2I($1+8)]		# Logical dimension
define	SMW_LAXIS	Memi[P2I($1+9)+$2-1]	# Logical axes [3]
define	SMW_LLEN	Meml[P2L($1+12)+$2-1]	# Logical axes lengths [3]
define	SMW_NSUM	Memi[P2I($1+15)+$2-1]	# Logical summing factors [2]

define	SMW_DTYPE	Memi[P2I($1+17)]	# Dispersion type
define	SMW_W1		Memd[P2D($1+18)]	# Coord of first phys pixel
define	SMW_DW		Memd[P2D($1+20)]	# Coord interval per phys pixel
define	SMW_Z		Memd[P2D($1+22)]	# Doppler factor
define	SMW_NW		Meml[P2L($1+24)]	# Number of dispersion pixels
define	SMW_APS		Memp[$1+25]		# Pointer to apertures
define	SMW_BEAMS	Memp[$1+26]		# Pointer to beams
define	SMW_APLOW	Memp[$1+27]		# Pointer to aplows
define	SMW_APHIGH	Memp[$1+28]		# Pointer to aphighs
define	SMW_APID	Memp[$1+29]		# Pointer to default apid
define	SMW_APIDS	Memp[$1+30]		# Pointer to apids

define	SMW_CTLP	Memp[$1+31]		# CT logical -> physical
define	SMW_NMW		Memi[P2I($1+32)]	# Number of MWCS pointers
define	SMW_MW		Memp[$1+33+$2]		# MWCS pointer(s)

# Spectrum formats
define	SMW_FORMATS	"|equispec|multispec|"
define	SMW_ND		0			# N-dimensional linear WCS
define	SMW_ES		1			# Equispec WCS
define	SMW_MS		2			# Multispec WCS

# Coordinate transformation structure.
define	SMW_CTLEN	(6 + $1)		# Length of SMW CT structure

define	SMW_SMW		Memp[$1]		# SMW pointer
define	SMW_CTTYPE	Memi[P2I($1+1)]		# Transformation type
define	SMW_DAXIS	Memi[P2I($1+2)]		# Dispersion axis
define	SMW_AAXIS	Memi[P2I($1+3)]		# Aperture axis
define	SMW_CTL		Memp[$1+4]		# Logical/physical pointer
define	SMW_NCT		Memi[P2I($1+5)]		# Number of CT pointers	
define	SMW_CT		Memp[$1+6+$2]		# Physical/world pointer

# Transformation types
define	SMW_CTTYPES	"|logical|physical|"
define	SMW_WL		12			# World to logical
define	SMW_WP		13			# World to physical
define	SMW_LW		21			# Logical to world
define	SMW_LP		23			# Logical to physical
define	SMW_PW		31			# Physical to world
define	SMW_PL		32			# Physical to logical


# SHDR - Spectrum header data structure

define	LEN_SHDR	378
define	LEN_SHDRS	79			# Length of strings

define	IMNAME		Memc[P2C($1)]		# Spectrum image name
define	IMSEC		Memc[P2C($1+40)]	# Spectrum image section
define	TITLE		Memc[P2C($1+80)]	# Title
define	LABEL		Memc[P2C($1+120)]	# Dispersion label
define	UNITS		Memc[P2C($1+160)]	# Dispersion units
define	FLABEL		Memc[P2C($1+200)]	# Flux label
define	FUNITS		Memc[P2C($1+240)]	# Flux units
define	IM		Memp[$1+280]		# IMIO pointer
define	MW		Memp[$1+281]		# SMW pointer
define	CTLW		Memp[$1+282]		# SMW logical -> world
define	CTWL		Memp[$1+283]		# SMW world -> logical
define	CTLW1		Memp[$1+284]		# SMW logical -> world
define	CTWL1		Memp[$1+285]		# SMW world -> logical
define	UN		Memp[$1+286]		# UNITS pointer for SX
define	MWUN		Memp[$1+287]		# UNITS pointer of SMW
define	FUN		Memp[$1+288]		# Flux units pointer
define	FUNIM		Memp[$1+289]		# Flux units pointer for image
define	LINDEX		Meml[P2L($1+290)+$2-1]	# Logical image index [2]
define	PINDEX		Meml[P2L($1+292)+$2-1]	# Physical image index [2]
define	APINDEX		Meml[P2L($1+294)]	# Aperture index

define	AP		Meml[P2L($1+295)]	# Aperture ID
define	APLOW		Memr[P2R($1+296)+$2-1]	# Aperture lower limit [2]
define	APHIGH		Memr[P2R($1+298)+$2-1]	# Aperture lower limit [2]
define	BEAM		Memi[P2I($1+300)]	# Beam ID
define	OFLAG		Memi[P2I($1+301)]	# Spectrum object type flag
define	IT		Memr[P2R($1+302)]	# Integ. time
define	RA		Memr[P2R($1+303)]	# Right ascension
define	DEC		Memr[P2R($1+304)]	# Declination
define	UT		Memr[P2R($1+305)]	# Universal time
define	ST		Memr[P2R($1+306)]	# Siderial time
define	HA		Memr[P2R($1+307)]	# Hour angle
define	AM		Memr[P2R($1+308)]	# Airmass
define	W0		Memr[P2R($1+309)]	# Starting wavelength
define	W1		Memr[P2R($1+310)]	# Ending wavelength
define	WP		Memr[P2R($1+311)]	# Wavelength increment per pixel
define	DC		Memi[P2I($1+312)]	# Dispersion correction
define	EC		Memi[P2I($1+313)]	# Extinction correction
define	FC		Memi[P2I($1+314)]	# Flux calibration
define	RC		Memc[P2C($1+315)]	# Reddening correction

define	NP1		Meml[P2L($1+355)]	# First logical pixel
define	NP2		Meml[P2L($1+356)]	# Last logical pixel
define	SN		Meml[P2L($1+357)]	# Number of pixels
define	SPEC		Memp[$1+358+$2-1]	# Pointers to spectra
define	SID		Memp[$1+365+$2-1]	# Pointers to spectra ID strings
define	STYPE		Memi[P2I($1+372)+$2-1]	# Spectrum type

# Spectrum types and access modes.

define	STYPES	"|coordinates|spectrum|raw|background|sigma|continuum|"

define	SH_NTYPES	6	# Number of spectrum types
define	SHHDR		0	# Get header only
define	SHX		1	# Get coordinates
define	SHDATA		2	# Get spectrum
define	SHRAW		3	# Get raw spectrum
define	SHSKY		4	# Get sky
define	SHSIG		5	# Get sigma
define	SHCONT		6	# Get continuum

# Shorthand for pointers to spectra.
define	SX		SPEC($1,SHX)		# Spectrum coordinates
define	SY		SPEC($1,SHDATA)		# Spectrum pointer
define	SR		SPEC($1,SHRAW)		# Raw spectrum pointer
define	SS		SPEC($1,SHSKY)		# Sky pointer
define	SE		SPEC($1,SHSIG)		# Sigma pointer
define	SC		SPEC($1,SHCONT)		# Continuum pointer

# Object flag values
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
