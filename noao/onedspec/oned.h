# Include file for ONEDSPEC definitions

define	MAX_NR_BEAMS	100	# Max number of instrument apertures
define	MAX_ELEM	100	# Max number of elements in STD file
define	MEMP		Memi	# De-reference pointer as integer
define	LEN_TOK_CHK	5	# Length of calib file verif token

# STD file structure

define	LEN_STD		(9 + SZ_FNAME + SZ_LINE + 2)

define	WSTART		Memr[$1]
define	WEND		Memr[$1+1]
define	STD_AIRMASS	Memr[$1+2]
define	BEAM_NR		Memi[$1+3]
define	SP_LENGTH	Memi[$1+4]
define	NR_WAVES	Memi[$1+5]
define	P_WAVES		MEMP[$1+6]
define	P_SENS		MEMP[$1+7]
define	P_COUNTS	MEMP[$1+8]
define	STD_IMAGE	Memc[P2C($1+9)]
define	STD_TITLE	Memc[P2C($1+9+SZ_FNAME+1)]

# SENSFUNC Interactive graphics definitions

define	EXTRA_PTS	20	# Number of extra points to allow during insert
define	EXTRA_HT	0.1	# Additional extent in y-direction scaling
define	SZ_XBOX		0.02	# Box size in viewport coords x-dim
define	SZ_YBOX		0.04	# Box size in viewport coords y-dim
define	MAX_DISTANCE	0.05	# Max distance from cursor (in NDC)

# Coincidence correction options

define	CC_PHOTO_MODE	1	# Photometer style correction
define	CC_IIDS_MODE	2	# IIDS style
define	CC_POWER_MODE	3	# Power law correction
define	CC_USER_MODE	4	# User supplies a function
