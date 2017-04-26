
# MULTISPEC Definitions

define	SZ_MS_IMAGE	79	# Size of image filename string
define	SZ_MS_TITLE	79	# Size of the image title string
define	SZ_MS_COMMENTS	1024	# Size of MULTISPEC comment block
define	SZ_MS_KEY	20	# Size of the database reference strings

define	MS_DB_ENTRIES	20	# Max number of database entries
define	MS_MAX_DES	1	# Max number of MULTISPEC descriptors
define	MAX_RANGES	30	# Maximum range dimension.

define	MS_ERROR	1000	# General MULTISPEC error code

# MULTISPEC I/O Descriptor

define	LEN_MS_DES	2 + MS_DB_ENTRIES

define	MS_DB		Memi[$1]	# DBIO descriptor
define	MS_NAMES	Memi[$1+1]	# Pointer to database names array
define	MS_DATA		Memi[$1+1+$2]	# Pointers to data from database

# MULTISPEC Header stored in database.

define	LEN_MS_HDR	84			# Length of MULTISPEC Header

define	MS_IMAGE	Memi[MS_DATA($1,HDR)]		# Image filename
define	MS_TITLE	Memi[MS_DATA($1,HDR)+40]	# Title from the image
define	MS_NSPECTRA	Memi[MS_DATA($1,HDR)+80]	# Number of spectra
define	MS_LEN		Memi[MS_DATA($1,HDR)+($2-1)+81] # Image dimensions
define	MS_NSAMPLES	Memi[MS_DATA($1,HDR)+83]	# Number of sample lines

# User callable macros

define	NAME		Memc[MS_NAMES($1)+($2-1)*(SZ_MS_KEY+1)]
define	HEADER		Memi[MS_DATA($1,HDR)]
define	COMMENT		Memc[MS_DATA($1,COMMENTS)+($2-1)]
define	LINE		Memi[MS_DATA($1,SAMPLE)+($2-1)]
define	PARAMETER	Memr[MS_DATA($1,$2)+($3-1)]
define	CV		Memi[MS_DATA($1,$2)+($3-1)]

# Ranges

define	LEN_RANGES	2

define	X_START		1	# Start of profile in image pixel coordinates
define	DX_START	2	# Start of profile relative to spectra center

# MULTISPEC parameter identifiers

define	HDR		1	# MULTISPEC header
define	COMMENTS	2	# MULTISPEC comments
define	SAMPLE		3	# Sample line array
define	I0		4	# Profile scale parameter
define	X0		5	# Profile position parameter
define	X0_FIT		6	# Spectra position fit

define	S0		7	# GAUSS5 shape parameter
define	S1		8	# GAUSS5 shape parameter
define	S2		9	# GAUSS5 shape parameter
define	S0_FIT		10	# GAUSS5 shape paramter fit
define	S1_FIT		11	# GAUSS5 shape paramter fit
define	S2_FIT		12	# GAUSS5 shape paramter fit


# Models
define	NONE		0	# No model
define	GAUSS5		1	# Five parameter Gaussian model
define	SMOOTH		2	# Data profile smoothing

# Five parameter Gaussian model -- GAUSS5
define	MS_NGAUSS5	5	# Number of GAUSS5 model parameters
define	I0_INDEX	1	# Index values for parameter arrays
define	X0_INDEX	2
define	S0_INDEX	3
define	S1_INDEX	4
define	S2_INDEX	5
