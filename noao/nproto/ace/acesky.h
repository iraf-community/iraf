# Sky parameter structure.
define	SKY_LEN		5		# Length of parameter structure

define	SKY_TYPE	Memi[$1]	# Type of sky algorithm
define	SKY_NEWSKY	Memi[$1+1]	# Determine a new sky sigma?
define	SKY_NEWSIG	Memi[$1+2]	# Determine a new sky sigma?
define	SKY_SURPARS	Memi[$1+3]	# Pointer to parameters for surface fit
define	SKY_BLKPARS	Memi[$1+4]	# Pointer to parameters for block stat

define	SKY_TYPES	"|surface|block|"
define	SKY_SURFACE	1		# Surface fitting
define	SKY_BLOCK	2		# Block statistics

define	SKY_SURPARSLEN	7		# Length of parameter structure
define	SKY_NSKYLINES	Memi[$1]	# Number of sky lines to sample
define	SKY_SKYBLK1D	Memi[$1+1]	# Sky block size for 1D averages
define	SKY_SKYHCLIP	Memr[P2R($1+2)]	# Sky fitting high sigma clip
define	SKY_SKYLCLIP	Memr[P2R($1+3)]	# Sky fitting low sigma clip
define	SKY_SKYXORDER	Memi[$1+4]	# Sky fitting x order
define	SKY_SKYYORDER	Memi[$1+5]	# Sky fitting y order
define	SKY_SKYXTERMS	Memi[$1+6]	# Sky fitting cross terms
