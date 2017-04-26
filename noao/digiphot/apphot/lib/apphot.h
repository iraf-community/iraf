# APPHOT header file

# APPHOT parameters (# 1 - 100)

define	IMNAME		1	# Image name
define	IMROOT		2	# Image name
define	CLNAME		3	# Coordinates file name
define	CLROOT		4	# Coordinates file name
define	PLOTFILE	5	# Name of the plotfile
define	OUTNAME		6	# Output file name

define	WCSIN		7	# Input WCS
define	WCSOUT		8	# Input WCS
define	MW		9	# MWCS descriptor
define	CTIN		10	# Input WCS transformation
define	CTOUT		11	# Output WCS transformation

define	SCALE		12	# Scale in pixels / unit
define	FWHMPSF		13	# Full width half maximum of the PSF
define	POSITIVE	14	# Emission or absorption feature
define	DATAMIN		15	# Minimum good data value
define	DATAMAX		16	# Maximum good data value

define	ITIME		17	# Exposure time
define	AIRMASS		18	# Airmass keyword
define	FILTER		19	# Filter keyword
define	OBSTIME		20	# Time of observation keyword

define	XAIRMASS	21	# Airmass value
define	FILTERID	22	# Filter id
define	OTIME		23	# Time stamp
define	EXPOSURE	24	# Exposure time keyword

define	WX		25	# Previous X cursor position
define	WY		26	# Previous Y cursor position
define	CWX		27	# Current X cursor position
define	CWY		28	# Current Y cursor position

# Define the WCS types

define	WCSINSTR		"|logical|tv|physical|world|"
define	WCSOUTSTR		"|logical|tv|physical|"
define	WCS_LOGICAL		1
define	WCS_TV			2
define	WCS_PHYSICAL		3
define	WCS_WORLD		4

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


# Define the memory cacheing fudge factor

define	MEMFUDGE	1.05

# Miscellaneous commands

define	MISC	"|show|radplots|"
define	MISC1	"|show|"

define	ACMD_SHOW	1
define	ACMD_RADPLOTS	2
