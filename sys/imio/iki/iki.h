# IKI.H -- Image Kernel Interface global definitions.

define	MAX_KERNELS	10		# max loaded IKI kernels
define	MAX_LENEXTN	4		# max length header filename extension
define	MIN_LENEXTN	2		# min length header filename extension
define	MAX_IMEXTN	64		# max image extension patterns
define	SZ_IKISBUF	512		# string buffer for IKI common
define	SZ_KNAME	4		# internal kernel name "oif,fxf,.."

# IMTYPE specifies the default type for new images.
define	ENV_IMTYPE	"imtype"
define	DEF_IMTYPE	"oif,noinherit"

# IMEXTN specifies the mapping between image types and file extensions.
define	ENV_IMEXTN	"imextn"
define	DEF_IMEXTN	"oif:imh fxf:fits,fit plf:pl qpf:qp stf:hhh,??h"

# The standard test image.
define	STD_TESTIMAGE	"dev$pix"
define	DEF_TESTIMAGE	"dev$pix.imh"

define	LEN_KERNEL	9		# length of a kernel entry in k_table
define	IKI_KNAME	k_kname[1,$1]	# image kernel name
define	IKI_OPEN	k_table[1,$1]	# open/create image
define	IKI_CLOSE	k_table[2,$1]	# close image
define	IKI_OPIX	k_table[3,$1]	# open/create pixel file
define	IKI_UPDHDR	k_table[4,$1]	# update image header
define	IKI_ACCESS	k_table[5,$1]	# test existence or legal type
define	IKI_COPY	k_table[6,$1]	# fast copy of entire image
define	IKI_DELETE	k_table[7,$1]	# delete image
define	IKI_RENAME	k_table[8,$1]	# rename image
define	IKI_FLAGS	k_table[9,$1]	# driver flags

# IKI driver flags.
define	IKF_NOCREATE	1		# kernel cannot create new images
