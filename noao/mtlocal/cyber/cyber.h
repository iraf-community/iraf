# Fix length of user area
define	LEN_USER_AREA	2880

# Offsets to elements of the IPPS raster header (RDUMPF)

define	DATA_TYPE_OFFSET	33	# Offset to data_type (nbpp)
define	NCOLS_OFFSET		35	# Offset to ncols (nppr)
define	NWORDS_OFFSET		37	# Offet to nwords_per_row
define	NROWS_OFFSET		41	# Offset to nrows
define	FIRST_PRU_OFFSET	43	# Offset to 1st pru of raster
define	MIN_OFFSET		63	# Offset to data min
define	MAX_OFFSET		65	# Offset to data max
define	EOR_OFFSET		89	# Offset to terminating pru

# Bit-offsets to fields in the Permanent File Table (LDUMPF)

define	PFN_OFFSET	(27 * 60 + 1 - 6)	# Name: Left justified
define	PF_ID_OFFSET	(26 * 60 + 1 - 6)	# ID: Right justified
define	CY_OFFSET	(31 * 60 + 1 - 12)	# Cycle number
define	CREATE_OFFSET  	(32 * 60 + 1 - 18)	# Creation date
define	ATTACH_OFFSET	(33 * 60 + 1 - 18)	# Date of last attach
define	ALTER_OFFSET 	(34 * 60 + 1 - 18)	# Date of last alteration
define	NCHARS_OFFSET	(8 * 60 + 7)		# Nchars in PF name

# The IPPS raster descriptor structure DT (RDUMPF):

define 	LEN_DT		10 + SZ_IPPS_ID + 1

define	BITS_PIXEL	Memi[$1]
define	PRU_EOR         Memi[$1+1]
define	WRDS_PER_ROW    Memi[$1+2]
define	NPRU_ROW	Memi[$1+3]
define	PRU_ROW_ONE     Memi[$1+4]
define	NCOLS		Memi[$1+5]
define	NROWS		Memi[$1+6]
define	DATA_MIN	Memr[P2R($1+7)]
define	DATA_MAX	Memr[P2R($1+8)]
define	IPPS_ID		Memc[P2C($1+10)]


# The DUMPF tape descriptor structure DMP (LDUMPF):

define 	LEN_DMP		15 + SZ_PFN + SZ_PF_ID + 2

define	CY		Memi[$1]
define	M_CREATE	Memi[$1+1]
define	D_CREATE	Memi[$1+2]
define	Y_CREATE	Memi[$1+3]
define	M_ALTER		Memi[$1+4]
define	D_ALTER		Memi[$1+5]
define	Y_ALTER		Memi[$1+6]
define	M_ATTACH	Memi[$1+7]
define	D_ATTACH	Memi[$1+8]
define	Y_ATTACH	Memi[$1+9]
define	NCHARS_PFN	Memi[$1+10]
define	PFN		Memc[P2C($1+15)]
define	ID		Memc[P2C($1+50)]

# Bit-offsets to various IDSFILE header words are defined:

define	TAPE_OFFSET     ((512 + (15 - 1)) * 64 + 1)
define	SCAN_OFFSET	((512 + (1 - 1)) * 64 + 1)
define	ITM_OFFSET	((512 + (2 - 1)) * 64 + 1)
define	NP1_OFFSET	((512 + (5 - 1)) * 64 + 1)
define	NP2_OFFSET	((512 + (6 - 1)) * 64 + 1)
define	BEAM_OFFSET	((512 + (7 - 1)) * 64 + 1)
define	COMPANION_OFFSET	((512 + (64 - 1)) * 64 + 1)
define	OLD_OFFSET	((512 + (64 - 1)) * 64 + 1)
define	SMODE_OFFSET	((512 + (10 - 1)) * 64 + 1)
define	UT_OFFSET	((512 + (11 - 1)) * 64 + 1)
define	ST_OFFSET	((512 + (12 - 1)) * 64 + 1)
define	DF_OFFSET	((512 + (16 - 1)) * 64 + 1)
define	SM_OFFSET	((512 + (17 - 1)) * 64 + 1)
define	QF_OFFSET	((512 + (18 - 1)) * 64 + 1)
define	DC_OFFSET	((512 + (19 - 1)) * 64 + 1)
define	QD_OFFSET	((512 + (20 - 1)) * 64 + 1)
define	EX_OFFSET	((512 + (21 - 1)) * 64 + 1)
define	BS_OFFSET	((512 + (22 - 1)) * 64 + 1)
define	CA_OFFSET	((512 + (23 - 1)) * 64 + 1)
define	CO_OFFSET	((512 + (24 - 1)) * 64 + 1)
define	HA_OFFSET	((512 + 26) * 64 + 1)
define	AIR_OFFSET	((512 + 27) * 64 + 1)
define	RA_OFFSET	((512 + 12) * 64 + 1)
define	DEC_OFFSET	((512 + 13) * 64 + 1)
define	LAM_OFFSET	((512 + 2) * 64 + 1)
define	DEL_OFFSET	((512 + 3) * 64 + 1)
define	OFLAG_OFFSET	((512 + (26 - 1)) * 64 + 1)
define	ALPHA1_OFFSET	((512 + (25 - 1)) * 64 + 55)
define	ALPHA2_OFFSET	(ALPHA1_OFFSET - 6)
define	ALPHA3_OFFSET	(ALPHA2_OFFSET - 6)
define	IDS_ID_OFFSET	(512 + 28)
define	COEFF_OFFSET	(512 + 37) 
define	IDSO_REC_OFF	1
define	IDSO_TAPE_OFF	961

# Definition of the control parameter descriptor structure (RIDSFILE, RIDSOUT)

define	LEN_CP			5 + SZ_FNAME + SZ_LINE + 2

define	MAKE_IMAGE		Memi[$1]
define	PRINT_PIXELS		Memi[$1+1]
define	LONG_HEADER		Memi[$1+2]
define	DATA_TYPE		Memi[$1+3]
define	IRAF_FILE		Memc[P2C($1+5)]
define	REC_NUMBERS		Memc[P2C($1+5+SZ_FNAME)]

# The IDSFILE descriptor structure IDS (RIDSOUT, RIDSFILE):

define	LEN_IDS			35 + 5 + SZ_IDS_ID + 1

define	HA			Memd[P2D($1)]
define	AIRMASS			Memd[P2D($1+2)]
define	RA			Memd[P2D($1+4)]
define	DEC			Memd[P2D($1+6)]
define	LAMBDA0			Memd[P2D($1+8)]
define	DELTA_LAMBDA		Memd[P2D($1+10)]
define	RECORD_NUMBER		Memi[$1+12]
define	ITM			Memi[$1+13]
define	NP1			Memi[$1+14]
define	NP2			Memi[$1+15]
define	BEAM_NUMBER		Memi[$1+16]
define	COMPANION_RECORD	Memi[$1+17]
define	SMODE			Memi[$1+18]
define	UT			Memi[$1+19]
define	ST			Memi[$1+20]
define	DF_FLAG			Memi[$1+21]
define	SM_FLAG			Memi[$1+22]
define	QF_FLAG			Memi[$1+23]
define	DC_FLAG			Memi[$1+24]
define	QD_FLAG			Memi[$1+25]
define	EX_FLAG			Memi[$1+26]
define	BS_FLAG			Memi[$1+27]
define	CA_FLAG			Memi[$1+28]
define	CO_FLAG			Memi[$1+29]
define	OFLAG			Memi[$1+30]
define	COEFF			Memi[$1+31]	
define	ALPHA_ID		Memc[P2C($1+35)]
define	LABEL			Memc[P2C($1+40)]


# Definitions for the Cyber DUMPF tape reading programs RDUMPF and LDUMPF

define 	NBITS_CHAR      (NBITS_BYTE * SZB_CHAR)	# Number of bits per char
define 	NBITS_CYBER_WORD	60		# Number of bits per Cyber word
define 	LEN_PRU			64		# Number of words per Cyber pru
define	NCHARS_PRU	(64 * 60 / NBITS_CHAR)	# Nchars per PRU
define	NBITS_PRU		3840		# Number of bits per Cyber pru
define	NCHARS_NOISE	 (48 / NBITS_CHAR)	# Nchars in a Cyber noise record
define 	NBITS_EOR_MARK		48		# Number of bits per eor marker
define	LEN_HEADER		64		# Number of words per header
define	SZ_HEADER	((64 * 60) / NBITS_CHAR)	# Nchars in IPPS header
define 	SZ_TAPE_BLK   ((512 * 60) / NBITS_CHAR) # Size in chars of tape block
define	SZ_TAPE_BUFFER	(SZ_TAPE_BLK + 60)	# Size of tape buffer for read
define	LEN_PFT			48		# Size of Permanent File Table
define  SZ_IPPS_ID		127		# Max number of characters in ID
define	MAX_RANGES		100	
define	NOT_SET			0		# Flag for data_type not set
define	BLANK			0.0		# Temporary value for blanks
define	NBYTES_WORD		5		# 5 12-bit bytes per Cyber word
define	NINT_CYBER_WRD		2
define	LEN_CYBER_READ	(4 * 65)	# Number of Cyber words read at once
define	SZ_PFT	      ((48 * 60) / NBITS_CHAR)	# Chars in Perm file table
define	SZ_PFN			40		# Max characters in PF Name 
define	SZ_PF_ID 		9		# Max characters in PF ID
define	NBITS_DATE		18		# Dates are written in 18 bits
define	NBITS_CY		12		# Cycle # written in 12 bits
define	NBITS_DC		6 		# Nbits display code character
define	NCHARS_WORD		10		# Number of display code 
						# characters per cyber word

define	LEN_INDEX	(5 * LEN_PRU)
define	LEN_USER_INDEX	(2 * LEN_PRU)
define	LEN_IDS_RECORD	(9 * LEN_PRU)
define	NPIX_IDS_RECORD	1024
define	SZ_IDS_ID	64
define	NCHAR_ALPHA	3
define	START_OF_IDSFILE	6	# First PRU of IDSFILE after index
define	NBITS_LRN	18		
define	NBITS_HRN	18	
define	NBITS_NPRU	24
define	LRN_OFFSET	19		# Offset in index to lrn
define	HRN_OFFSET	1		# Offset in index to hrn
define	NPRU_OFFSET	37		# Offset in index to pru ordinal
define	NBITS_DC	6		# Number of bits in a display code char
define	MAX_COEFF	28		# Maximum n_coeff if DF is set
define	NPIX_LINE	8		# Npixels per line of text (IDSOUT)
define	NLINES_PIXELS	128		# Nlines of text containing pixels
