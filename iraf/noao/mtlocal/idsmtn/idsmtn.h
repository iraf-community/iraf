# Definitions for the Mountain format IDS tape reader:

define	MAX_RANGES	100
define	DUMMY		3	# Value returned if DUMMY IDS record is read

define	SZB_IDS_RECORD		4216
define	NPIX_IDS_REC		1024
define	DATA_BYTE		9			# First byte of data
define	MAX_NCOEFF		25
define	SZ_IDS_ID		64
define	LEN_USER_AREA		2880

# The control parameter structure is defined below:

define	LEN_CP		(10 + SZ_FNAME + 1)

define	IS_REDUCED	Memi[P2I($1)]
define	LONG_HEADER	Memi[P2I($1+1)]
define	PRINT_PIXELS	Memi[P2I($1+2)]
define	MAKE_IMAGE	Memi[P2I($1+3)]
define	OFFSET		Memi[P2I($1+4)]
define	DATA_TYPE	Memi[P2I($1+5)]
define	IRAF_FILE	Memc[P2C($1+10)]


# The header structure is defined below:

define	LEN_IDS			(40 + SZ_IDS_ID + 1)

define	HA			Memd[P2D($1)]
define	AIRMASS			Memd[P2D($1+2)]
define	RA			Memd[P2D($1+4)]
define	DEC			Memd[P2D($1+6)]
define	W0			Memd[P2D($1+8)]
define	WPC			Memd[P2D($1+10)]
define	NREC			Memi[P2I($1+12)]
define	NP1			Memi[P2I($1+13)]
define	NP2			Memi[P2I($1+14)]
define	ITM			Memi[P2I($1+15)]
define	BEAM			Memi[P2I($1+16)]
define	COMPANION_RECORD	Memi[P2I($1+17)]
define	SMODE			Memi[P2I($1+18)]
define	UT			Memi[P2I($1+19)]
define	ST			Memi[P2I($1+20)]
define	DF_FLAG			Memi[P2I($1+21)]
define	SM_FLAG			Memi[P2I($1+22)]
define	QF_FLAG			Memi[P2I($1+23)]
define	DC_FLAG			Memi[P2I($1+24)]
define	QD_FLAG			Memi[P2I($1+25)]
define	EX_FLAG			Memi[P2I($1+26)]
define	BS_FLAG			Memi[P2I($1+27)]
define	CA_FLAG			Memi[P2I($1+28)]
define	CO_FLAG			Memi[P2I($1+29)]
define	OFLAG			Memi[P2I($1+30)]
define	COEFF			Memi[P2I($1+31)]	
define	DRA			Memi[P2I($1+32)]
define	DDEC			Memi[P2I($1+33)]
define	ALPHA_ID		Memc[P2C($1+35)]
define	LABEL			Memc[P2C($1+40)]


# BYTE offsets to various IDS header words are defined below.  These become
# word offsets once each byte is unpacked per element of an integer array.

define	NREC_OFFSET	((1 * 2) - 1)
define	ITM_OFFSET	((3 * 2) - 1)
define	DATA_OFFSET	((5 * 2) - 1)
define	W0_OFFSET	((2053 * 2) - 1)
define	WPC_OFFSET	((2056 * 2) - 1)
define	NP1_OFFSET	((2059 * 2) - 1)
define	NP2_OFFSET	((2060 * 2) - 1)
define	OFLAG_OFFSET	((2061 * 2) - 1)
define	SMODE_OFFSET	((2062 * 2) - 1)
define	UT_OFFSET	((2063 * 2) - 1)
define	ST_OFFSET	((2065 * 2) - 1)
define	BEAM_OFFSET	((2067 * 2) - 1)
define	HA_OFFSET	((2068 * 2) - 1)
define	RA_OFFSET	((2071 * 2) - 1)
define	DEC_OFFSET	((2074 * 2) - 1)
define	DRA_OFFSET	((2077 * 2) - 1)
define	DDEC_OFFSET	((2078 * 2) - 1)
define	LABEL_OFFSET	((2079 * 2) - 1)
