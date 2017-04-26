# APPHOT Structure

define	LEN_APSTRUCT	(36 + 12 * SZ_FNAME + 12)

# apphot definitions

define	AP_VERSION	Memi[$1]	# Package version number
define	AP_CWX		Memr[P2R($1+1)]	# Current x cursor coordinate
define	AP_CWY		Memr[P2R($1+2)]	# Current y cursor coordinate
define	AP_WX		Memr[P2R($1+3)]	# Previous x cursor coordinate
define	AP_WY		Memr[P2R($1+4)]	# Previous y cursor coordinate
define	AP_FWHMPSF	Memr[P2R($1+5)]	# FWHM of the PSF
define	AP_SCALE	Memr[P2R($1+6)]	# Scale in pixels / unit (internal)
define	AP_POSITIVE	Memi[$1+7]	# Emission feature ?
define	AP_DATAMIN	Memr[P2R($1+8)]	# Mininum good data value
define	AP_DATAMAX	Memr[P2R($1+9)]	# Maximum good data value
define	AP_ITIME	Memr[P2R($1+10)] # Exposure time
define	AP_XAIRMASS	Memr[P2R($1+11)] # Air mass

# pointer to the apphot strucutures

define	AP_NOISE	Memi[$1+12]	# Pointer to noise structure
define	AP_PCENTER	Memi[$1+13]	# Pointer to center structure
define	AP_PSKY		Memi[$1+14]	# Pointer to sky structure
define	AP_PPSF		Memi[$1+15]	# Pointer to psf structure
define  AP_PPHOT        Memi[$1+16]	# Pointer to phot structure
define	AP_PDISPLAY	Memi[$1+17]	# Pointer to display structure
define	AP_POLY		Memi[$1+18]	# Pointer to polyphot structure
define	AP_RPROF	Memi[$1+19]	# Pointer to radprof strucuture
define	AP_PFIND	Memi[$1+20]	# Pointer to the find structure

# pointer to sequential access buffer (not used currently)

define	AP_SEQUENTIAL	Memi[$1+21]	# Sequential or random access
define	AP_IMBUF	Memi[$1+22]	# Pointer to internal buffer
define	AP_HWIDTH	Memi[$1+23]	# Half-width of buffer in image lines
define	AP_IMX1		Memi[$1+24]	# Lower column limit of buffer
define	AP_IMX2		Memi[$1+25]	# Upper column limit of buffer
define	AP_IMY1		Memi[$1+26]	# Lower line limit of buffer
define	AP_IMY2		Memi[$1+27]	# Upper line limit of buffer

# pointer to the wcs info


define	AP_WCSIN	Memi[$1+28]	# the input wcs
define	AP_WCSOUT	Memi[$1+29]	# the output wcs
define	AP_MW		Memi[$1+30]	# the mwcs pointer
define	AP_CTIN	        Memi[$1+31]	# the input transformation pointer
define	AP_CTOUT        Memi[$1+32]	# the output transformation pointer

# image, file and keyword names

define  AP_IMNAME	Memc[P2C($1+36)]                # IRAF image name
define  AP_IMROOT	Memc[P2C($1+36+SZ_FNAME+1)]     # IRAF image root name
define	AP_CLNAME	Memc[P2C($1+36+2*SZ_FNAME+2)]   # Coordinate list
define	AP_CLROOT	Memc[P2C($1+36+3*SZ_FNAME+3)]   # Coordinate list root
define	AP_PLOTFILE	Memc[P2C($1+36+4*SZ_FNAME+4)]	# Plotfile
define	AP_OUTNAME	Memc[P2C($1+36+5*SZ_FNAME+5)]	# Output
define	AP_EXPOSURE	Memc[P2C($1+36+6*SZ_FNAME+6)]	# Exposure keyword
define	AP_AIRMASS	Memc[P2C($1+36+7*SZ_FNAME+7)]	# Airmass keyword
define	AP_FILTER	Memc[P2C($1+36+8*SZ_FNAME+8)]	# Filter keyword
define	AP_FILTERID	Memc[P2C($1+36+9*SZ_FNAME+9)]	# Filter id
define	AP_OBSTIME	Memc[P2C($1+36+10*SZ_FNAME+10)]	# Obstime keyword
define	AP_OTIME	Memc[P2C($1+36+11*SZ_FNAME+11)]	# Time of observation

# default definitions

define	DEF_SCALE	1.0000
define	DEF_POSITIVE	YES
define	DEF_DATAMIN	INDEFR
define	DEF_DATAMAX	INDEFR

define	DEF_EXPOSURE	""
define	DEF_AIRMASS	""
define	DEF_FILTER	""
define	DEF_OBSTIME	""
define	DEF_ITIME	INDEFR
define	DEF_XAIRMASS	INDEFR
define	DEF_FILTERID	"INDEF"
define	DEF_OTIME	"INDEF"
