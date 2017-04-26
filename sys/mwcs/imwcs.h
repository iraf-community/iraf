# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMWCS.H -- Definitions used by MW_SAVEIM and MW_LOADIM to encode and
# decode the FITS (image header) version of a MWCS.

define	DEF_MAXCARDS	128		# initial number of card descriptors
define	INC_MAXCARDS	128		# increment if overflow occurs
define	IDB_STARTVALUE	10		# column at which data field begins
define	MAX_FITSCOLS	68		# max chars of data per FITS card
define	DEF_BIGBUF	680		# initial size of "big" FITS buffer
define	INC_BIGBUF	680		# initial size of "big" FITS buffer
define	SZ_KWNAME	8		# size of FITS keyword
define	SZ_VALSTR	21		# size of FITS value string
define	SZ_SBUF		163840		# string buffer size (2048 WCS cards)
define	SZ_OBUF		680		# biggest "attribute = value" string
define	SZ_CARD		80		# card width, chars
define	SZ_BIGSTR	MAX_FITSCOLS	# max size FITS string (one card)

# WCS FITS main descriptor.
define	LEN_IMWCS	310
define	IW_IM		Memi[$1]	# image descriptor
define	IW_NDIM		Memi[$1+1]	# image dimension
define	IW_NCARDS	Memi[$1+2]	# number of WCS cards
define	IW_CBUF		Memi[$1+3]	# card descriptors
define	IW_MAXCARDS	Memi[$1+4]	# CBUF allocated length, cards
define	IW_SBUF		Memi[$1+5]	# string buffer
define	IW_SBUFLEN	Memi[$1+6]	# SBUF allocated length, chars
define	IW_SBUFOP	Memi[$1+7]	# current offset in sbuf
define	IW_CARD		(IW_CBUF($1)+(($2)-1)*LEN_CDES)
	# (avail)
define	IW_CROTA	Memr[P2R($1+9)]				# obsolete
define	IW_CTYPE	Memi[$1+10+($2)-1]			# axtype (strp)
define	IW_CRPIX	Memd[P2D($1+20)+($2)-1]			# CRPIXi
define	IW_CRVAL	Memd[P2D($1+40)+($2)-1]			# CRVALi
define	IW_CDELT	Memd[P2D($1+60)+($2)-1]			# CDELTi
define	IW_CD		Memd[P2D($1+80)+(($3)-1)*7+($2)-1]	# CDi_j
define	IW_LTV		Memd[P2D($1+180)+($2)-1]		# LTVi
define	IW_LTM		Memd[P2D($1+200)+(($3)-1)*7+($2)-1]	# LTMi_j
define	IW_WSVLEN	Memi[$1+300+($2)-1]			# WSVi_LEN

# WCS FITS card descriptor.
define	LEN_CDES	6
define	C_TYPE		Memi[$1]	# card type
define	C_AXIS		Memi[$1+1]	# wcs axis
define	C_INDEX		Memi[$1+2]	# card number on axis
define	C_CARDNO	Memi[$1+3]	# card number in header
define	C_UPDATED	Memi[$1+4]	# card has been updated
define	C_RP		Memi[$1+5]	# pointer to card

# Card types.
define	TY_CTYPE	1
define	TY_CDELT	2
define	TY_CROTA	3
define	TY_CRPIX	4
define	TY_CRVAL	5
define	TY_CD		6
define	TY_LTV		7
define	TY_LTM		8
define	TY_WATDATA	9
define	TY_WSVLEN	10
define	TY_WSVDATA	11
define	TY_WCSDIM	12
define	TY_WAXMAP	13

# IW_RFITS definitions.
define	RF_REFERENCE	0		# reference directly into header
define	RF_COPY		1		# reference copies of header cards
