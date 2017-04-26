# STF.H -- IKI/STF internal definitions.

define	HDR_TEMPLATE	"dev$pix.hhh"	# used by fmkcopy to create new header
define	MAX_LENEXTN	3		# max length imagefile extension
define	STF_HDRPATTERN	"^??h"		# class of legal header extensions
define	STF_DEFHDREXTN	"hhh"		# default header file extension
define	STF_DEFPIXEXTN	"hhd"		# default pixel file extension
define	ENV_DEFIMTYPE	"imtype"	# name of environment variable
define	STF_MAXDIM	7		# max NAXIS
define	MAX_CACHE	5		# max cached header files
define	DEF_CACHE	3		# default size of header file cache
define	ENV_STFCACHE	"stfcache"	# environment variable for cache size
define	MAX_PCOUNT	99		# max param descriptors
define	SZ_DATATYPE	16		# e.g., `REAL*4'
define	SZ_KEYWORD	8		# size of a FITS keyword
define	SZ_PTYPE	8		# e.g., `CRPIX1'
define	SZ_PDTYPE	16		# e.g., `CHAR*8'
define	SZ_COMMENT	FITS_SZCOMMENT	# comment string for GPB card
define	SZ_EXTRASPACE	(81*32)		# extra space for new cards in header

define	FITS_RECLEN	80		# length of a FITS record (card)
define	FITS_STARTVALUE	10		# first column of value field
define	FITS_ENDVALUE	30		# last  column of value field
define	FITS_SZVALSTR	21		# nchars in value string
define	FITS_SZCOMMENT	50		# max chars in comment, incl. /

# STF image descriptor, used internally by the STF interface.  The required
# header parameters are maintained in this descriptor, everything else is
# simply copied into the user area of the IMIO descriptor.

define	LEN_STFDES	(LEN_STFBASE+MAX_PCOUNT*LEN_PDES)
define	STF_CACHE	STF_BITPIX	# cache descriptor starting here
define	STF_CACHELEN	(33+STF_PCOUNT($1)*LEN_PDES)
define	LEN_STFBASE	43

define	STF_ACMODE	Memi[$1]	# image access mode
define	STF_NEWIMAGE	Memi[$1+1]	# creating entire new STF format image?
define	STF_GROUP	Memi[$1+2]	# group to be accessed
define	STF_SZGROUP	Memi[$1+3]	# size of image+hdr in pixfile, chars
define	STF_PFD		Memi[$1+4]	# pixfile file descriptor
define	STF_GRARG	Memi[$1+5]	# group index given in image name
			# (extra space)
define	STF_BITPIX	Memi[$1+10]	# bits per pixel
define	STF_NAXIS	Memi[$1+11]	# number of axes in image
define	STF_GROUPS	Memi[$1+12]	# group format?
define	STF_GCOUNT	Memi[$1+13]	# number of groups in STF image
define	STF_PSIZE	Memi[$1+14]	# size of GPB, bits
define	STF_PCOUNT	Memi[$1+15]	# number of parameters in GPB
define	STF_DATATYPE	Memc[P2C($1+16)]# datatype string
define	STF_LENAXIS	Memi[$1+35+$2-1]# 35:41 = [7] max
define	STF_PDES	(($1)+43+((($2)-1)*LEN_PDES))

# GPB Parameter descriptor.
define	LEN_PDES	81
define	P_OFFSET	Memi[$1]	# struct offset of parameter
define	P_SPPTYPE	Memi[$1+1]	# SPP datatype of parameter
define	P_LEN		Memi[$1+2]	# number of elements
define	P_PSIZE		Memi[$1+3]	# field size, bits
define	P_PTYPEP	(P2C($1+4))	# pointer to parameter name
define	P_PTYPE		Memc[P2C($1+4)]	# parameter name
define	P_PDTYPE	Memc[P2C($1+13)]# datatype string
define	P_COMMENT	Memc[P2C($1+30)]# comment string

# Reserved FITS keywords known to this code.
define	KW_BITPIX	1
define	KW_DATATYPE	2
define	KW_END		3
define	KW_GCOUNT	4
define	KW_GROUPS	5
define	KW_NAXIS	6
define	KW_NAXISN	7
define	KW_PCOUNT	8
define	KW_PDTYPE	9
define	KW_PSIZE	10
define	KW_PSIZEN	11
define	KW_PTYPE	12
define	KW_SIMPLE	13
