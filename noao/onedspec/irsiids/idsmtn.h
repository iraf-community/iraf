# Definitions for the Mountain format IDS tape reader:

define	MAX_RANGES	100
define	DUMMY		3	# Value returned if DUMMY IDS record is read

define	NBITS_CHAR		(SZB_CHAR * NBITS_BYTE)
define	SZ_IDS_RECORD		(2108 * 16 / NBITS_CHAR)
define	NPIX_IDS_REC		1024
define	LEN_USER_AREA		2880
define	SZ_IDS_ID		64
define	NBITS_VN_3WRD_FP	48
define	NBITS_VN_2WRD_FP	32
define	NBITS_VN_LONG_INT	32
define	NBITS_VN_INT		16
define	NBITS_FORTH_CHAR	8
define	DATA_BYTE		9			# First byte of data
define	NBYTES_DATA	(1024 * 32 / NBITS_BYTE)	# Number of data bytes
define	NBYTES_INT	(NBITS_INT / NBITS_BYTE)
define	NBYTES_VN_3WRD_FP	6
define	NBYTES_VN_2WRD_FP	4
define	NBITS_2WRD_HIGH		8
define	WRD2_EXP_OFFSET		25
define	NBITS_2WRD_EXP		6
define	WRD2_MANT_SIGN		24
define	WRD2_EXP_SIGN		31
define	NSIG_VN_BITS		15
define	VN_LONG_SIGN		31
define	WRD3_MANT_SIGN		31
define	WRD3_EXP_SIGN		47
define	MAX_NCOEFF		25


# The control parameter structure is defined below:

define	LEN_CP		10 + SZ_FNAME + 1

define	IS_REDUCED	Memi[$1]
define	LONG_HEADER	Memi[$1+1]
define	PRINT_PIXELS	Memi[$1+2]
define	MAKE_IMAGE	Memi[$1+3]
define	OFFSET		Memi[$1+4]
define	DATA_TYPE	Memi[$1+5]
define	IRAF_FILE	Memc[P2C($1+10)]


# The header structure is defined below:

define	LEN_IDS			40 + SZ_IDS_ID + 1

define	HA			Memr[P2R($1)]
define	AIRMASS			Memr[P2R($1+1)]
define	RA			Memr[P2R($1+2)]
define	DEC			Memr[P2R($1+3)]
define	W0			Memr[P2R($1+4)]
define	WPC			Memr[P2R($1+5)]
define	LINE			Memi[$1+6]
define	NP1			Memi[$1+7]
define	NP2			Memi[$1+8]
define	ITM			Memr[P2R($1+9)]
define	BEAM			Memi[$1+10]
define	W			Memi[$1+11]
define	UT			Memr[P2R($1+13)]
define	ST			Memr[P2R($1+14)]
define	DF_FLAG			Memi[$1+15]
define	SM_FLAG			Memi[$1+16]
define	QF_FLAG			Memi[$1+17]
define	DC_FLAG			Memi[$1+18]
define	QD_FLAG			Memi[$1+19]
define	EX_FLAG			Memi[$1+20]
define	BS_FLAG			Memi[$1+21]
define	CA_FLAG			Memi[$1+22]
define	CO_FLAG			Memi[$1+23]
define	OFLAG			Memi[$1+24]
define	POINT			Memi[$1+25]	
define	DRA			Memi[$1+26]
define	DDEC			Memi[$1+27]
define	ALPHA_ID		Memc[P2C($1+35)]
define	LABEL			Memc[P2C($1+40)]


# Bit offsets to various IDS header words are defined below:

define	NREC_OFFSET	((0 * 16) + 1)
define	RFLAGS_OFFSET	((1 * 16) + 1)
define	ITM_OFFSET	((2 * 16) + 1)
define	DATA_OFFSET	((4 * 16) + 1)
define	W0_OFFSET	((2052 * 16) + 1)
define	WPC_OFFSET	((2055 * 16) + 1)
define	NP1_OFFSET	((2058 * 16) + 1)
define	NP2_OFFSET	((2059 * 16) + 1)
define	OFLAG_OFFSET	((2060 * 16) + 1)
define	SMODE_OFFSET	((2061 * 16) + 1)
define	UT_OFFSET	((2062 * 16) + 1)
define	ST_OFFSET	((2064 * 16) + 1)
define	BEAM_OFFSET	((2066 * 16) + 1)
define	HA_OFFSET	((2067 * 16) + 1)
define	RA_OFFSET	((2070 * 16) + 1)
define	DEC_OFFSET	((2073 * 16) + 1)
define	DRA_OFFSET	((2076 * 16) + 1)
define	DDEC_OFFSET	((2077 * 16) + 1)
define	LABEL_OFFSET	((2078 * 16) + 1)
