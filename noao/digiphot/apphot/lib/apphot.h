# APPHOT header file

# APPHOT parameters (# 1 - 100)

define	FWHMPSF		1	# Full width half maximum of the PSF
define	WX		2	# Previous X cursor position
define	WY		3	# Previous Y cursor position
define	IMNAME		4	# Image name
define	CWX		5	# Current X cursor position
define	CWY		6	# Current Y cursor position
define	CLNAME		7	# Coordinates file name
define	PLOTFILE	8	# Name of the plotfile
define	POSITIVE	9	# Emission or absorption feature
define	ITIME		10	# Exposure time
define	EXPOSURE	11	# Exposure time keyword
define	DATAMIN		12	# Minimum good data value
define	DATAMAX		13	# Maximum good data value
define	OUTNAME		14	# Output file name
define	SCALE		15	# Scale in pixels / unit
define	AIRMASS		16	# Airmass keyword
define	XAIRMASS	17	# Airmass value
define	FILTER		18	# Filter keyword
define	FILTERID	19	# Filter id
define	OBSTIME		20	# Time of observation keyword
define	OTIME		21	# Time stamp

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
define	KY_OBSTIME	"obstime"
define	KY_OTIME	"otime"

# define APPHOT units strings

#define	UN_FWHMPSF	"scaleunit"
#define	UN_POSITIVE	"switch"
#define	UN_ITIME	"timeunit"
#define	UN_EXPOSURE	"keyword"
#define	UN_DATAMIN	"counts"
#define	UN_DATAMAX	"counts"
#define	UN_SCALE	"units"
#define	UN_AIRMASS	"keyword"
#define	UN_XAIRMASS	"number"
#define	UN_FILTER	"keyword"
#define	UN_FILTERID	"name"
#define	UN_OBSTIME	"keyword"
#define	UN_OTIME	"timeunit"

define	UN_ASCALEUNIT	"scaleunit"
define	UN_ASWITCH	"switch"
define	UN_ACOUNTS	"counts"
define	UN_AUNITS	"units"
define	UN_ATIMEUNIT	"timeunit"
define	UN_AKEYWORD	"keyword"
define	UN_ANAME	"name"
define	UN_ANUMBER	"number"

# APPHOT string commands

define	APCMDS	"|fwhmpsf|emission|exposure|itime|datamin|datamax|image|coords|output|scale|airmass|xairmass|filter|ifilter|obstime|otime|"

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
define	APCMD_OBSTIME	15
define	APCMD_OTIME	16

# Miscellaneous commands

define	MISC	"|show|radplots|"
define	MISC1	"|show|"

define	ACMD_SHOW	1
define	ACMD_RADPLOTS	2
