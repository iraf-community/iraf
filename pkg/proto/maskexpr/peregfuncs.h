# PEREGFUNCS.H -- Structure definitions.

# Circle definitions.

define	LEN_CIRCLEDES	5
define	C_PL		Memi[$1]	# reference mask
define	C_XCEN		Memr[P2R($1+1)]	# X center of circle
define	C_YCEN		Memr[P2R($1+2)]	# Y center of circle
define	C_RADIUS	Memr[P2R($1+3)]	# radius of circle
define	C_PV		Memi[$1+4]	# pixel value


# Ellipse definitions.

define	LEN_ELLDES	10
define	E_PL		Memi[$1]	# reference mask
define	E_XCEN		Memr[P2R($1+1)]	# X center of ellipse
define	E_YCEN		Memr[P2R($1+2)]	# Y center of ellipse
define	E_AA		Memr[P2R($1+3)]	# aa parameter
define	E_BB		Memr[P2R($1+4)]	# bb parameter
define	E_CC		Memr[P2R($1+5)]	# cc parameter
define	E_FF		Memr[P2R($1+6)]	# ff paramater
define	E_DXMAX		Memr[P2R($1+7)]	# the maximum x offset
define	E_DYMAX		Memr[P2R($1+8)]	# the maximum x offset
define	E_PV		Memi[$1+9]	# pixel value


# Box definitions.

define	LEN_BOXDES	6
define	B_PL		Memi[$1]	# reference mask
define	B_X1		Memr[P2R($1+1)]	# X1 lower left corner of box
define	B_Y1		Memr[P2R($1+2)]	# Y1 lower left corner of box
define	B_X2		Memr[P2R($1+3)]	# X2 upper right corner of box
define	B_Y2		Memr[P2R($1+4)]	# Y2 upper right corner of box
define	B_PV		Memi[$1+5]	# pixel value


# Polygon definitions.

define	TOL		0.0001		# pixel units
define	swapi		{tempi=$2;$2=$1;$1=tempi}
define	swapr		{tempr=$2;$2=$1;$1=tempr}
define	equal		(abs($1-$2)<TOL)

define	LEN_PGONDES	7
define	P_PL		Memi[$1]	# pointer to X vector
define	P_XP		Memi[$1+1]	# pointer to X vector
define	P_YP		Memi[$1+2]	# pointer to Y vector
define	P_OO		Memi[$1+3]	# pointer to previous range list
define	P_OY		Memi[$1+4]	# y value of previous range list
define	P_NS		Memi[$1+5]	# number of line segments
define	P_PV		Memi[$1+6]	# pixel value


# Circular annulus definitions.

define	LEN_CANNDES	6
define	CA_PL		Memi[$1]	# reference mask
define	CA_XCEN		Memr[P2R($1+1)]	# x center of circle
define	CA_YCEN		Memr[P2R($1+2)]	# y center of circle
define	CA_RADIUS1	Memr[P2R($1+3)]	# inner radius of annulus
define	CA_RADIUS2	Memr[P2R($1+4)]	# outer radius of annulus
define	CA_PV		Memi[$1+5]	# pixel value


# Elliptical annulus defintiions.

define	LEN_EANNDES	16
define	EA_PL		Memi[$1]	 # reference mask
define	EA_XCEN		Memr[P2R($1+1)]	 # x center of ellipse
define	EA_YCEN		Memr[P2R($1+2)]	 # y center of ellipse
define	EA_AA1		Memr[P2R($1+3)]	 # aa parameter for inner ellipse
define	EA_BB1		Memr[P2R($1+4)]	 # bb parameter for inner ellipse
define	EA_CC1		Memr[P2R($1+5)]	 # cc parameter for inner ellipse
define	EA_FF1		Memr[P2R($1+6)]	 # ff parameter for inner ellipse
define	EA_DXMAX1	Memr[P2R($1+7)]  # max dx value for inner ellipse
define	EA_DYMAX1	Memr[P2R($1+8)]  # max dy value for inner ellipse
define	EA_AA2		Memr[P2R($1+9)]	 # aa parameter for outer ellipse
define	EA_BB2		Memr[P2R($1+10)] # bb parameter for outer ellipse
define	EA_CC2		Memr[P2R($1+11)] # cc parameter for outer ellipse
define	EA_FF2		Memr[P2R($1+12)] # ff parameter for outer ellipse
define	EA_DXMAX2	Memr[P2R($1+13)] # max dx value for outer ellipse
define	EA_DYMAX2	Memr[P2R($1+14)] # max dy value for outer ellipse
define	EA_PV		Memi[$1+15]	 # pixel value


# Rasterop annulus definitions.

define	LEN_RANNDES	7
define	RA_PL		Memi[$1]	# the mask descriptor
define	RA_IXP		Memi[$1+1]	# pointer to inner polygon X vector
define	RA_IYP		Memi[$1+2]	# pointer to inner Y polygon vector
define	RA_OXP		Memi[$1+3]	# pointer to outer X polygon vector
define	RA_OYP		Memi[$1+4]	# pointer to outer Y polygon vector
define	RA_NVER		Memi[$1+5]	# number of vertices
define	RA_PV		Memi[$1+6]	# mask pixel value


# Polygon annulus definitions.

define	LEN_PAGONDES	7
define	PA_PL		Memi[$1]	# the mask descriptor
define	PA_IXP		Memi[$1+1]	# pointer to inner polygon X vector
define	PA_IYP		Memi[$1+2]	# pointer to inner Y polygon vector
define	PA_OXP		Memi[$1+3]	# pointer to outer X polygon vector
define	PA_OYP		Memi[$1+4]	# pointer to outer Y polygon vector
define	PA_NVER		Memi[$1+5]	# number of vertices
define	PA_PV		Memi[$1+6]	# mask pixel value


# Column definitions.

define	LEN_COLSDES	4
define	L_PL		Memi[$1]	# reference mask
define	L_RANGES	Memi[$1+1]	# pointer to the ranges	
define	L_NRANGES	Memi[$1+2]	# the number of ranges
define	L_XS		Memi[$1+3]	# the starting x coordinate value
define	L_NPIX		Memi[$1+4]	# the number of pixels value
define	L_PV		Memi[$1+5]	# pixel value


# Line definitions.

define	LEN_LINESDES	3
define	L_PL		Memi[$1]	# reference mask
define	L_RANGES	Memi[$1+1]	# pointer to the ranges	
define	L_PV		Memi[$1+2]	# pixel value

define	MAX_NRANGES	100
define	SMALL_NUMBER	1.0e-24
