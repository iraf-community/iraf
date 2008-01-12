define	NUMSTART		11	# First object number

# Header structure.
define	HDR_SZFNAME	99		# Length of filename strings.
define	HDR_LEN		101
define	HDR_MAGZERO	Memr[P2R($1)]		# Magnitude zero point
define	HDR_IMAGE	Memc[P2C($1+1)]		# Image name
define	HDR_MASK	Memc[P2C($1+51)]	# Object mask name

# Mask Flags.
define	MASK_NUM	0077777777B	# Mask number
define	MASK_BNDRY	0100000000B	# Boundary flag
define	MASK_SPLIT	0200000000B	# Split flag
define	MASK_DARK	0400000000B	# Dark flag

define	MSETFLAG	ori($1,$2)
define	MUNSETFLAG	andi($1,noti($2))

define	MNUM		(andi($1,MASK_NUM))
define	MNOTDARK	(andi($1,MASK_DARK)==0)
define	MDARK		(andi($1,MASK_DARK)!=0)
define	MNOTSPLIT	(andi($1,MASK_SPLIT)==0)
define	MSPLIT		(andi($1,MASK_SPLIT)!=0)
define	MNOTBNDRY	(andi($1,MASK_BNDRY)==0)
define	MBNDRY		(andi($1,MASK_BNDRY)!=0)

# Output object masks types.
define	OM_TYPES		"|boolean|numbers|colors|all|"
define	OM_BOOL		1	# Boolean (0=sky, 1=object+bad)
define	OM_ONUM		2	# Object number only
define	OM_COLORS	3	# Bad=1, Objects=2-9
define	OM_ALL		4	# All values
