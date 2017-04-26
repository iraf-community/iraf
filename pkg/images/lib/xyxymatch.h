# The definitions file for the LINXYMATCH task

# Define the matching algorithms

define	RG_MATCHSTR	"|tolerance|triangles|"
define	RG_TOLERANCE	1	# Match by tolerance only
define	RG_TRIANGLES	2	# Match by triangles

# Define the reference and input files types

define	RG_REFFILE	1	# The input reference coordinate file
define	RG_INFILE	2	# The input coordinate file

# Define some useful constants

define	MAX_NTIE	3	# Maximum number of tie points
define	MAX_NCOEFF	6       # Maximum number of coefficients
define	DEF_BUFSIZE	1000	# The default buffer size
define	SZ_TRIINDEX	6	# Number of triangle indices to save.
define	SZ_TRIPAR	5	# Number of triangle parameters

# Define the structure of the internal arrays used by the trangles algorithm

define	RG_INDEX	1	# Sort index
define	RG_X1		2	# Vertex 1
define	RG_X2		3	# Vertex 2
define	RG_X3		4	# Vertex 3
define	RG_CC		5	# Counterclockwise ?
define	RG_MATCH	6	# Match index

define	RG_LOGP		1	# Log of the perimeter
define	RG_RATIO	2	# Ratio of longest to shortest side
define	RG_COS1		3	# Cos of angle at vertex 1
define	RG_TOLR		4	# Tolerance in the ratio
define	RG_TOLC		5	# Tolerance in the cosine
