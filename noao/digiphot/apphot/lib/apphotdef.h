# APHOT Structure

define	LEN_APSTRUCT	(30 + 8 * SZ_FNAME + 8)

# apphot definitions

define	AP_VERSION		Memi[$1]	# Package version number
define	AP_CWX			Memr[$1+1]	# Current x cursor coordinate
define	AP_CWY			Memr[$1+2]	# Current y cursor coordinate
define	AP_WX			Memr[$1+3]	# Previous x cursor coordinate
define	AP_WY			Memr[$1+4]	# Previous y cursor coordinate
define	AP_FWHMPSF		Memr[$1+5]	# FWHM of the PSF
define	AP_SCALE		Memr[$1+6]	# Scale in pixels/unit (int)
define	AP_POSITIVE		Memi[$1+7]	# Emission feature
define	AP_DATAMIN		Memr[$1+8]	# Mininum good data value
define	AP_DATAMAX		Memr[$1+9]	# Maximum good data value
define	AP_ITIME		Memr[$1+10]	# Exposure time
define	AP_XAIRMASS		Memr[$1+11]	# Air mass

define	AP_NOISE		Memi[$1+12]	# Pointer to noise structure
define	AP_PCENTER		Memi[$1+13]	# Pointer to center structure
define	AP_PSKY			Memi[$1+14]	# Pointer to sky structure
define	AP_PPSF			Memi[$1+15]	# Pointer to psf structure
define  AP_PPHOT                Memi[$1+16]	# Pointer to phot structure
define	AP_PDISPLAY		Memi[$1+17]	# Pointer to display structure
define	AP_POLY			Memi[$1+18]	# Pointer to polyphot structure
define	AP_RPROF		Memi[$1+19]	# Pointer to radprof strucuture
define	AP_PFIND		Memi[$1+20]	# Pointer to the find structure

define  AP_IMNAME		Memc[P2C($1+22)]  # IRAF image name
define	AP_CLNAME		Memc[P2C($1+22+SZ_FNAME+1)]  # Coordinate list
define	AP_PLOTFILE		Memc[P2C($1+22+2*SZ_FNAME+2)]	# Plotfile
define	AP_EXPOSURE		Memc[P2C($1+22+3*SZ_FNAME+3)]	# Exposure
define	AP_OUTNAME		Memc[P2C($1+22+4*SZ_FNAME+4)]	# Output
define	AP_AIRMASS		Memc[P2C($1+22+5*SZ_FNAME+5)]	# Airmass
define	AP_FILTER		Memc[P2C($1+22+6*SZ_FNAME+6)]	# Filter
define	AP_FILTERID		Memc[P2C($1+22+7*SZ_FNAME+7)]	# Filter id

# default definitions

define	DEF_SCALE	1.0000
define	DEF_POSITIVE	YES
define	DEF_ITIME	INDEFR
define	DEF_EXPOSURE	""
define	DEF_DATAMIN	INDEFR
define	DEF_DATAMAX	INDEFR
define	DEF_AIRMASS	""
define	DEF_XAIRMASS	INDEFR
define	DEF_FILTER	""
define	DEF_FILTERID	""
