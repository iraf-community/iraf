# SENSFUNC definitions.

define	SF_NGRAPHS	4		# Number of graphs per frame
define	SF_INCLUDE	1		# Include observation
define	SF_EXCLUDE	2		# Exclude observation
define	SF_DELETE	3		# Delete observation

# SENSFUNC Standard Star Data Structure.

define	SZ_STDIMAGE	63		# Length of standard image name
define	SZ_STDTITLE	63		# Length of standard title

define	LEN_STD		115		# Length of standard obs. structure

define	STD_IMAGE	Memc[P2C($1)]	# Standard image name
define	STD_SKY		Memc[P2C($1+32)] # Standard image sky
define	STD_TITLE	Memc[P2C($1+64)] # Standard title
define	STD_FLAG	Memi[$1+96]	# Flag
define	STD_BEAM	Memi[$1+97]	# Beam number of spectrum
define	STD_NPTS	Memi[$1+98]	# Number of points in spectrum
define	STD_EXPTIME	Memr[$1+99]	# Exposure time
define	STD_AIRMASS	Memr[$1+100]	# Airmass of spectrum
define	STD_WSTART	Memr[$1+101]	# Starting wavelength of spectrum
define	STD_WEND	Memr[$1+102]	# Ending wavelength of spectrum
define	STD_SHIFT	Memr[$1+103]	# Added shift
define	STD_NWAVES	Memi[$1+104]	# Number of calibration wavelengths
define	STD_WAVES	Memi[$1+105]	# Pointer to wavelengths
define	STD_FLUXES	Memi[$1+106]	# Pointer to standard flux values
define	STD_DWAVES	Memi[$1+107]	# Pointer to flux bandwidths
define	STD_COUNTS	Memi[$1+108]	# Pointer to counts
define	STD_SENS	Memi[$1+109]	# Pointer to sensitivities
define	STD_FIT		Memi[$1+110]	# Pointer to fitted sensitivities
define	STD_WTS		Memi[$1+111]	# Pointer to weights
define	STD_IWTS	Memi[$1+112]	# Pointer to weights
define	STD_X		Memi[$1+114]	# Pointer to plotted x values
define	STD_Y		Memi[$1+115]	# Pointer to plotted y values

# Graphics structure

define	GP_SZTITLE	79			# Size of title string

define	LEN_GP		65			# Length of structure

define	GP_GIO		Memi[$1]		# GIO pointer
define	GP_TITLE	Memc[P2C($1+1)]		# General title
define	GP_GRAPHS	Memc[P2C($1+41)+$2-1]	# Graphs
define	GP_IMAGES	Memi[$1+44+$2-1]	# Pointer to image names
define	GP_SKYS		Memi[$1+48+$2-1]	# Pointer to sky names
define	GP_MARK		Memi[$1+52]		# Mark type
define	GP_SZMARK	Memr[$1+53]		# Mark size
define	GP_MDEL		Memi[$1+54]		# Deleted mark
define	GP_SZMDEL	Memr[$1+55]		# Size of deleted mark
define	GP_MADD		Memi[$1+56]		# Mark type
define	GP_WSTART	Memr[$1+57]		# Starting wavelength for plots
define	GP_WEND		Memr[$1+58]		# Ending wavelength for plots
define	GP_IMIO		Memi[$1+58+$2]		# IMIO pointers
define	GP_AIRMASS	Memr[$1+62+$2]		# Airmass range of plots
