# OIF.H -- IKI/OIF internal definitions.

define	MAX_LENEXTN	3		# max length imagefile extension
define	OIF_HDREXTN	"imh"		# image header filename extension
define	OIF_PIXEXTN	"pix"		# image pixfile extension
define	LEN_PIXHDR	512		# max length of PIXHDR structure
define	COMPRESS	NO		# disable alignment of image lines?
define	DEF_VERSION	2		# default file version

define	ENV_OIFVER	"oifversion"	# default format for new images
define	HDR		"HDR$"		# stands for header directory
define	STRLEN_HDR	4

define	TY_IMHDR	1		# main imagefile header
define	TY_PIXHDR	2		# pixel file header
