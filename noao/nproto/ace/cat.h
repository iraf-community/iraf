# Catalog structure.
define	CAT_SZSTR	99		# Length of catalog string
define	CAT_LEN		160		# Length of catalog structure
define	CAT_OBJS	Memi[$1]	# Array of objects (ptr)
define	CAT_APFLUX	Memi[$1+1]	# Array of aperture fluxes (ptr)
define	CAT_NOBJS	Memi[$1+2]	# Number of objects
define	CAT_NUMMAX	Memi[$1+3]	# Maximum object number
define	CAT_FLAGS	Memi[$1+4]	# Catalog flags
define	CAT_HDR		Memi[$1+5]	# Header structure
define	CAT_INTBL	Memi[$1+6]	# Input table structure
define	CAT_OUTTBL	Memi[$1+7]	# Output table structure
define	CAT_MAGZERO	Memr[P2R($1+8)]	# Magnitude zero
define	CAT_CATALOG	Memc[P2C($1+10)]	# Catalog name
define	CAT_OBJID	Memc[P2C($1+60)]	# Default ID
define	CAT_STRPTR	P2C($1+110)		# Working string buffer
define	CAT_STR		Memc[CAT_STRPTR($1)]	# Working string buffer

# Table structure.
define	TBL_LEN		2
define	TBL_TP		Memi[$1]	# Table pointer
define	TBL_STP		Memi[$1+1]	# Symbol table of entries

# Entry structure.
define	ENTRY_ULEN	19			# Length of units string
define	ENTRY_FLEN	19			# Length of format string
define	ENTRY_DLEN	99			# Length of description string
define	ENTRY_LEN	95			# Length of entry structure
define	ENTRY_CDEF	Memi[$1]		# Column descriptor
define	ENTRY_ID	Memi[$1+1]		# Entry id
define	ENTRY_TYPE	Memi[$1+2]		# Datatype in object record
define	ENTRY_CTYPE	Memi[$1+3]		# Datatype in catalog
define	ENTRY_FUNC	Memi[$1+4]		# Entry function
define	ENTRY_RAP	Memr[P2R($1+5)]		# Entry aperture radius
define	ENTRY_UNITS	Memc[P2C($1+6)]		# Entry units (19)
define	ENTRY_FORMAT	Memc[P2C($1+26)]	# Entry format (19)
define	ENTRY_DESC	Memc[P2C($1+46)]	# Entry description (99)

define	FUNCS		"|MAG|"
define	FUNC_MAG	1		# Magnitude

# Catalog extensions.
define	CATEXTNS	"|fits|tab|"

# Catalog Parameters.
define	CATPARAMS	"|image|mask|objid|catalog|nobjects|magzero|"
