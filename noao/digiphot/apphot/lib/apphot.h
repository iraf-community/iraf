# APPHOT header file

# APPHOT parameters (# 1 - 20)

define	FWHMPSF		1	# Full width half maximum of the PSF
define	WX		3	# Previous X cursor position
define	WY		4	# Previous Y cursor position
define	IMNAME		5	# Image name
define	CWX		6	# Current X cursor position
define	CWY		7	# Current Y cursor position
define	CLNAME		8	# Coordinates file name
define	PLOTFILE	9	# Name of the plotfile
define	POSITIVE	10	# Emission or absorption feature
define	ITIME		11	# Exposure time
define	EXPOSURE	12	# Exposure time keyword
define	DATAMIN		13	# Minimum good data value
define	DATAMAX		14	# Maximum good data value
define	OUTNAME		15	# Output file name
define	SCALE		16	# Scale in pixels / unit
define	AIRMASS		17	# Airmass keyword
define	XAIRMASS	18	# Airmass value
define	FILTER		19	# Filter keyword
define	FILTERID	20	# Filter id

# define APPHOT keywords

define	KY_FWHMPSF	"fwhmpsf"
define	KY_IMNAME	"image"
define	KY_POSITIVE	"emission"
define	KY_ITIME	"itime"
define	KY_EXPOSURE	"exposure"
define	KY_DATAMIN	"datamin"
define	KY_DATAMAX	"datamax"
define	KY_OUTNAME	"output"
define	KY_CLNAME	"coords"
define	KY_SCALE	"scale"
define	KY_AIRMASS	"airmass"
define	KY_XAIRMASS	"xairmass"
define	KY_FILTER	"filter"
define	KY_FILTERID	"ifilter"

# define APPHOT units strings

define	UN_FWHMPSF	"scaleunit"
define	UN_POSITIVE	"switch"
define	UN_ITIME	"timeunit"
define	UN_EXPOSURE	"keyword"
define	UN_DATAMIN	"counts"
define	UN_DATAMAX	"counts"
define	UN_SCALE	"units"
define	UN_AIRMASS	"keyword"
define	UN_XAIRMASS	"number"
define	UN_FILTER	"keyword"
define	UN_FILTERID	"name"

# APPHOT string commands

define	APCMDS	"|fwhmpsf|emission|exposure|itime|datamin|datamax|image|coords|output|scale|airmass|xairmass|filter|ifilter|"

define	APCMD_FWHMPSF	1
define	APCMD_EMISSION	2
define	APCMD_EXPOSURE	3
define	APCMD_ITIME	4
define	APCMD_DATAMIN	5
define	APCMD_DATAMAX	6
define	APCMD_IMAGE	7
define	APCMD_COORDS	8
define	APCMD_OUTPUT	9
define	APCMD_SCALE	10
define	APCMD_AIRMASS	11
define	APCMD_XAIRMASS	12
define	APCMD_FILTER	13
define	APCMD_FILTERID	14

# Miscellaneous commands

define	MISC	"|show|radplots|"
define	MISC1	"|show|"

define	ACMD_SHOW	1
define	ACMD_RADPLOTS	2
