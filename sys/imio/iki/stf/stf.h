# STF.H -- IKI/STF internal definitions.

define	HDR_TEMPLATE	"dev$pix.hhh"	# used by fmkcopy to create new header
define	MAX_LENEXTN	3		# max length imagefile extension
define	STF_HDRPATTERN	"^??h"		# class of legal header extensions
define	STF_DEFHDREXTN	"hhh"		# default header file extension
define	STF_DEFPIXEXTN	"hhd"		# default pixel file extension
define	ENV_DEFIMTYPE	"imtype"	# name of environment variable
define	STF_MAXDIM	7		# max NAXIS
define	MAX_PCOUNT	50		# max param descriptors
define	SZ_DATATYPE	16		# e.g., `REAL*4'
define	SZ_PTYPE	8		# e.g., `CRPIX1'
define	SZ_PDTYPE	16		# e.g., `CHAR*8'
define	SZ_EXTRASPACE	(81*32)		# extra space for new cards in header

define	FITS_RECLEN	80		# length of a FITS record (card)
define	FITS_STARTVALUE	10		# first column of value field
define	FITS_ENDVALUE	30		# last  column of value field
define	FITS_SZVALSTR	21		# nchars in value string

# STF image descriptor, used internally by the STF interface.  The required
# header parameters are maintained in this descriptor, everything else is
# simply copied into the user area of the IMIO descriptor.

define	LEN_STFDES	(43+MAX_PCOUNT*LEN_PDES)
define	STF_ACMODE	Memi[$1]	# image access mode
define	STF_NEWIMAGE	Memi[$1+1]	# creating entire new STF format image?
define	STF_GROUP	Memi[$1+2]	# group to be accessed
define	STF_SZGROUP	Memi[$1+3]	# size of image+hdr in pixfile, chars
define	STF_SZGPBHDR	Memi[$1+4]	# size of GPB part of header
define	STF_PFD		Memi[$1+5]	# pixfile file descriptor
			# (extra space)
define	STF_BITPIX	Memi[$1+11]	# Bits per pixel
define	STF_NAXIS	Memi[$1+12]	# number of axes in image
define	STF_GROUPS	Memi[$1+13]	# group format?
define	STF_GCOUNT	Memi[$1+14]	# number of groups in STF image
define	STF_PSIZE	Memi[$1+15]	# size of GPB, bits
define	STF_PCOUNT	Memi[$1+16]	# number of parameters in GPB
define	STF_DATATYPE	Memc[P2C($1+17)]# Datatype string
define	STF_LENAXIS	Memi[$1+35+$2-1]# 35:41 = [7] max
define	STF_PDES	(($1)+43+((($2)-1)*LEN_PDES))

# GPB Parameter descriptor.
define	LEN_PDES	29
define	P_OFFSET	Memi[$1]	# struct offset of parameter
define	P_SPPTYPE	Memi[$1+1]	# SPP datatype of parameter
define	P_LEN		Memi[$1+2]	# number of elements
define	P_PSIZE		Memi[$1+3]	# field size, bits
define	P_PTYPE		Memc[P2C($1+4)]	# parameter name
define	P_PDTYPE	Memc[P2C($1+13)]# datatype string
