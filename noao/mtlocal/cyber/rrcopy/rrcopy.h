
# Definitions for the Cyber RCOPY tape reader

define 	NBITS_CHAR      (NBITS_BYTE * SZB_CHAR)	# Number of bits per char
define 	NBITS_CYBER_WORD	60		# Number of bits per Cyber word
define 	LEN_PRU			64		# Number of words per Cyber pru
define	NBITS_PRU		3840		# Number of bits per Cyber pru
define	NCHARS_NOISE	 (48 / NBITS_CHAR)	# Nchars in a Cyber noise record
define 	NBITS_EOR_MARK		48		# Number of bits per eor marker
define 	SZ_HEADER      ((64 * 60) / NBITS_CHAR)	# Size in chars of IPPS header
define 	SZ_TAPE_BLK   ((512 * 60) / NBITS_CHAR) # Size in chars of tape block
define	SZ_BUFFER	(SZ_TAPE_BLK + 100)	# Size of tape buffer for read
define  SZ_IPPS_ID		127		# Max number of characters in ID
define	MAX_RANGES		100	
define	NOT_SET			0		# Flag for data_type not set
define	BLANK			0.0		# Temporary value for blanks

# Bit-offsets to IPPS header words 

define	DATA_TYPE_OFFSET	(16 * 60 + 1)	# Offset to data_type (nbpp)
define	NCOLS_OFFSET		(17 * 60 + 1)	# Offset to ncols (nppr)
define	NWORDS_OFFSET		(18 * 60 + 1)	# Offet to nwords_per_row
define	NROWS_OFFSET		(20 * 60 + 1)	# Offset to nrows
define	FIRST_PRU_OFFSET	(21 * 60 + 1)	# Offset to 1st pru of raster
define	MIN_OFFSET		(31 * 60 + 1)	# Offset to data min
define	MAX_OFFSET		(32 * 60 + 1)	# Offset to data max
define	EOR_OFFSET		(44 * 60 + 1)	# Offset to terminating pru

# The IPPS raster descriptor structure RP:

define 	LEN_RP		10 + SZ_IPPS_ID + 1

define	BITS_PIXEL	Memi[$1]
define	PRU_EOR         Memi[$1+1]
define	WRDS_PER_ROW    Memi[$1+2]
define	PRU_ROW_ONE     Memi[$1+3]
define	NCOLS		Memi[$1+4]
define	NROWS		Memi[$1+5]
define	DATA_MIN	Memr[P2R($1+6)]
define	DATA_MAX	Memr[P2R($1+7)]
define	IPPS_ID		Memc[P2C($1+10)]
