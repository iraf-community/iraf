# IMMARK Task Header File

# define IMMARK structure

define	LEN_MARKSTRUCT  (40 + 10 * SZ_FNAME + SZ_LINE + 11)

define	MK_AUTOLOG	Memi[$1]	# Enable auto logging
define	MK_NUMBER	Memi[$1+1]	# Number coordinate list entries
define	MK_LABEL	Memi[$1+2]	# Label coordinate list entries
define	MK_GRAYLEVEL	Memi[$1+3]	# Gray level of marks
define	MK_SIZE		Memi[$1+4]	# Size of numbers and text
define	MK_FRAME	Memi[$1+5]	# Frame number for display
define	MK_NCIRCLES	Memi[$1+6]	# Number of circles
define	MK_NELLIPSES	Memi[$1+7]	# Number of ellipses
define	MK_NSQUARES	Memi[$1+8]	# Number of squares
define	MK_NRECTANGLES	Memi[$1+9]	# Number of rectangles
define	MK_MKTYPE	Memi[$1+10]	# Type of mark
define	MK_SZPOINT	Memi[$1+11]	# Size of point
define	MK_NXOFFSET	Memi[$1+12]	# X offset of number
define	MK_NYOFFSET	Memi[$1+13]	# X offset of number

define	MK_RADII	Memi[$1+14]	# Pointer to list of radii
define	MK_AXES		Memi[$1+15]	# Pointer to list of semi-major axes
define	MK_SLENGTHS	Memi[$1+16]	# Pointer to list of square lengths
define	MK_RLENGTHS	Memi[$1+17]	# Pointer to list of rectangle lengths

define	MK_RATIO	Memr[P2R($1+18)] # Ratio of width/length rectangles
define	MK_ELLIPTICITY	Memr[P2R($1+19)] # Ellipticity of ellipses
define	MK_RTHETA	Memr[P2R($1+20)] # Position angle of rectangle
define	MK_ETHETA	Memr[P2R($1+21)] # Position angle of ellipse

define	MK_X1		Memi[$1+22]	# LL corner x coord
define	MK_Y1		Memi[$1+23]	# LL corner y coord
define	MK_X2		Memi[$1+24]	# UR corner x coord
define	MK_Y2		Memi[$1+25]	# UR corner y coord

define	MK_TOLERANCE	Memr[P2R($1+26)] # Tolerance for deleting objects

define	MK_IMAGE	Memc[P2C($1+40)]		# Image name
define	MK_OUTIMAGE	Memc[P2C($1+40+SZ_FNAME+1)]	# Output image
define	MK_COORDS	Memc[P2C($1+40+2*SZ_FNAME+2)]	# Coordinate file
define	MK_DELETIONS	Memc[P2C($1+40+3*SZ_FNAME+3)]	# Deletions files
define	MK_LOGFILE	Memc[P2C($1+40+4*SZ_FNAME+4)]	# Log file
define	MK_FONT		Memc[P2C($1+40+5*SZ_FNAME+5)]	# Font
define	MK_MARK		Memc[P2C($1+40+6*SZ_FNAME+6)]	# Default mark
define	MK_CSTRING	Memc[P2C($1+40+7*SZ_FNAME+7)]	# Default circles
define	MK_RSTRING	Memc[P2C($1+40+8*SZ_FNAME+8)]	# Default rectangles

# define IMMARK ID's

define	AUTOLOG		1
define	NUMBER		2
define	GRAYLEVEL	3
define	SIZE		4
define	FRAME		5
define	NCIRCLES	6
define	NELLIPSES	7
define	NSQUARES	8
define	NRECTANGLES	9
define	MKTYPE		10
define	RADII		11
define	AXES		12
define	SLENGTHS	13
define	RLENGTHS	14
define	RATIO		15
define	ELLIPTICITY	16
define	RTHETA		17
define	ETHETA		18
define	IMAGE		19
define	OUTIMAGE	20
define	COORDS		21
define	LOGFILE		22
define	FONT		23
define	MARK		25
define	CSTRING		26
define	RSTRING		27
define	SZPOINT		28
define	X1		29
define	Y1		30
define	X2		31
define	Y2		32
define	NXOFFSET	33
define	NYOFFSET	34
define	LABEL		35
define	TOLERANCE	36
define	DELETIONS	37

# define mark types

define	MKTYPELIST	"|point|circle|rectangle|line|plus|cross|none|"

define	MK_POINT	1
define	MK_CIRCLE	2
define	MK_RECTANGLE	3
define	MK_LINE		4
define	MK_PLUS		5
define	MK_CROSS	6
define	MK_NONE		7

# define the fonts

define	MKFONTLIST	"|raster|"

# define IMMARK commands

define	MKCMD_IMAGE		1
define	MKCMD_OUTIMAGE		2
define	MKCMD_COORDS		3
define	MKCMD_LOGFILE		4
define	MKCMD_AUTOLOG		5
define	MKCMD_FRAME		6
define	MKCMD_FONT		7
define	MKCMD_NUMBER		8
define	MKCMD_GRAYLEVEL		9
define	MKCMD_SIZE		10
define	MKCMD_SZPOINT		11
define	MKCMD_MARK		12
define	MKCMD_CIRCLES		13
define	MKCMD_RECTANGLES	14
define	MKCMD_SHOW		15
define	MKCMD_SNAP		16
define	MKCMD_NXOFFSET		17
define	MKCMD_NYOFFSET		18
define	MKCMD_SAVE		19
define	MKCMD_RESTORE		20
define	MKCMD_LABEL		21
define	MKCMD_TOLERANCE		22
define	MKCMD_DELETIONS		23

define	MKCMD2_WTEXT		1
define	MKCMD2_MOVE		2
define	MKCMD2_NEXT		3


# define IMMARK keywords

define	KY_IMAGE		"image"
define	KY_OUTIMAGE		"outimage"
define	KY_COORDS		"coords"
define	KY_LOGFILE		"logfile"
define	KY_AUTOLOG		"autolog"
define	KY_FRAME		"frame"
define	KY_FONT			"font"
define	KY_NUMBER		"number"
define	KY_GRAYLEVEL		"color"
define	KY_SIZE			"txsize"
define	KY_SZPOINT		"pointsize"
define	KY_MARK			"mark"
define	KY_CIRCLES		"radii"
define	KY_RECTANGLE		"lengths"
define	KY_NXOFFSET		"nxoffset"
define	KY_NYOFFSET		"nyoffset"
define	KY_RATIO		"ratio"
define	KY_LABEL		"label"
define	KY_TOLERANCE		"tolerance"
define	KY_DELETIONS		"deletions"


define	MKCMDS	"|junk|outimage|coords|logfile|autolog|frame|font|number|color|txsize|pointsize|mark|radii|lengths|show|write|nxoffset|nyoffset|save|restore|label|tolerance|deletions|"

define	MKCMDS2 "|text|move|next|"

# miscellaneous

define	MAX_NMARKS		100
