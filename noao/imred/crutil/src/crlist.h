define	CR_ALLOC	100		# Allocation block size
define	CR_LENSTRUCT	9		# Length of structure

define	CR_NCR		Memi[$1]	# Number of cosmic rays
define	CR_NALLOC	Memi[$1+1]	# Length of cosmic ray list
define	CR_COL		Memi[$1+2]	# Pointer to columns
define	CR_LINE		Memi[$1+3]	# Pointer to lines
define	CR_FLUX		Memi[$1+4]	# Pointer to fluxes
define	CR_RATIO	Memi[$1+5]	# Pointer to flux ratios
define	CR_WT		Memi[$1+6]	# Pointer to training weights
define	CR_REPLACE	Memi[$1+7]	# Pointer to replacement values
define	CR_FLAG		Memi[$1+8]	# Pointer to rejection flag

define	ALWAYSNO	3
define	ALWAYSYES	4

define	CR_RMAX		3.		# Maximum radius for matching
