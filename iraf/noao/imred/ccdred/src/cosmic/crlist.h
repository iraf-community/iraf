define	CR_ALLOC	100		# Allocation block size
define	CR_LENSTRUCT	9		# Length of structure

define	CR_NCR		Memi[P2I($1)]	# Number of cosmic rays
define	CR_NALLOC	Memi[P2I($1+1)]	# Length of cosmic ray list
define	CR_COL		Memi[P2I($1+2)]	# Pointer to columns
define	CR_LINE		Memi[P2I($1+3)]	# Pointer to lines
define	CR_FLUX		Memi[P2I($1+4)]	# Pointer to fluxes
define	CR_RATIO	Memi[P2I($1+5)]	# Pointer to flux ratios
define	CR_WT		Memi[P2I($1+6)]	# Pointer to training weights
define	CR_REPLACE	Memi[P2I($1+7)]	# Pointer to replacement values
define	CR_FLAG		Memi[P2I($1+8)]	# Pointer to rejection flag

define	ALWAYSNO	3
define	ALWAYSYES	4

define	CR_RMAX		3.		# Maximum radius for matching
