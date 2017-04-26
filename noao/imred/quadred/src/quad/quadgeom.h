# QUADGEOM - Structure definitions and macros for quadgeom structure.

define QG_LENSTRUCT    28		# Length of structure.
define QG_MAXAMPS	4		# Maximum possible number of readouts.
# The various arrays are dimensioned as QG_MAXAMPS+1.
# QG_AAAA(0) contains quantities refering to the entire image
# QG_AAAA(z) contains quantities refering to the zth sub-image

define QG_NAMPS		Memi[$1]	# Total number of active readouts.
define QG_NAMPSX	Memi[$1+1]	# Number of active readouts in X.
define QG_NAMPSY	Memi[$1+2]	# Number of active readouts in Y.

# Array of pointers to names of active readouts.
define QG_AMPIDPTR	Memi[$1+3]			# --> ampid  array.
define QG_AMPID		Memi[QG_AMPIDPTR($1)+$2]	# ampid  array.

# Array of pointers to names of active readouts.
define QG_AMPTYPTR	Memi[$1+4]			# --> amptype  array.
define QG_AMPTYPE	Memi[QG_AMPTYPTR($1)+$2]	# amptype  array.

# Dimensions of image from each readout.
define QG_NXPTR		Memi[$1+5]			# --> X dimension array.
define QG_NX		Memi[QG_NXPTR($1)+$2]		# X dimension.
define QG_NYPTR		Memi[$1+6]			# --> Y dimension array.
define QG_NY		Memi[QG_NYPTR($1)+$2]		# Y dimension.

# datasec = "[dx1:dx2,dy1:dy2]"
define QG_DOFF		7
define QG_DX1PTR	Memi[$1+QG_DOFF]		# --> dx1 array.
define QG_DX2PTR	Memi[$1+QG_DOFF+1]		# --> dx2 array.
define QG_DY1PTR	Memi[$1+QG_DOFF+2]		# --> dy1 array.
define QG_DY2PTR	Memi[$1+QG_DOFF+3]		# --> dy2 array.
define QG_DX1		Memi[QG_DX1PTR($1)+$2]		# dx1.
define QG_DX2		Memi[QG_DX2PTR($1)+$2]		# dx2.
define QG_DY1		Memi[QG_DY1PTR($1)+$2]		# dy1..
define QG_DY2		Memi[QG_DY2PTR($1)+$2]		# dy2.

# trimsec = "[tx1:tx2,ty1:ty2]"
define QG_TOFF		11				# QG_DOFF+4.
define QG_TX1PTR	Memi[$1+QG_TOFF]		# --> tx1 array.
define QG_TX2PTR	Memi[$1+QG_TOFF+1]		# --> tx2 array.
define QG_TY1PTR	Memi[$1+QG_TOFF+2]		# --> ty1 array.
define QG_TY2PTR	Memi[$1+QG_TOFF+3]		# --> ty2 array.
define QG_TX1		Memi[QG_TX1PTR($1)+$2]		# tx1.
define QG_TX2		Memi[QG_TX2PTR($1)+$2]		# tx2.
define QG_TY1		Memi[QG_TY1PTR($1)+$2]		# ty1.
define QG_TY2		Memi[QG_TY2PTR($1)+$2]		# ty2.

# biassec = "[bx1:bx2,by1:by2]"
define QG_BOFF		15				# QG_TOFF+4.
define QG_BX1PTR	Memi[$1+QG_BOFF]		# --> bx1 array.
define QG_BX2PTR	Memi[$1+QG_BOFF+1]		# --> bx2 array.
define QG_BY1PTR	Memi[$1+QG_BOFF+2]		# --> by1 array.
define QG_BY2PTR	Memi[$1+QG_BOFF+3]		# --> by2 array.
define QG_BX1		Memi[QG_BX1PTR($1)+$2]		# bx1.
define QG_BX2		Memi[QG_BX2PTR($1)+$2]		# bx2.
define QG_BY1		Memi[QG_BY1PTR($1)+$2]		# by1.
define QG_BY2		Memi[QG_BY2PTR($1)+$2]		# by2.

# ccdsec = "[cx1:cx2,cy1:cy2]"
define QG_COFF		19				# QG_BOFF+4.
define QG_CX1PTR	Memi[$1+QG_COFF]		# --> cx1 array.
define QG_CX2PTR	Memi[$1+QG_COFF+1]		# --> cx2 array.
define QG_CY1PTR	Memi[$1+QG_COFF+2]		# --> cy1 array.
define QG_CY2PTR	Memi[$1+QG_COFF+3]		# --> cy2 array.
define QG_CX1		Memi[QG_CX1PTR($1)+$2]		# cx1.
define QG_CX2		Memi[QG_CX2PTR($1)+$2]		# cx2.
define QG_CY1		Memi[QG_CY1PTR($1)+$2]		# cy1.
define QG_CY2		Memi[QG_CY2PTR($1)+$2]		# cy2.

# ampsec = "[ax1:ax2,ay1:ay2]"
define QG_AOFF		23				# QG_COFF+4.
define QG_AX1PTR	Memi[$1+QG_AOFF]		# --> ax1 array.
define QG_AX2PTR	Memi[$1+QG_AOFF+1]		# --> ax2 array.
define QG_AY1PTR	Memi[$1+QG_AOFF+2]		# --> ay1 array.
define QG_AY2PTR	Memi[$1+QG_AOFF+3]		# --> ay2 array.
define QG_AX1		Memi[QG_AX1PTR($1)+$2]		# ax1.
define QG_AX2		Memi[QG_AX2PTR($1)+$2]		# ax2.
define QG_AY1		Memi[QG_AY1PTR($1)+$2]		# ay1.
define QG_AY2		Memi[QG_AY2PTR($1)+$2]		# ay2.

# Phantom markers
define QG_PHOFF		27				# QG_AOFF+4
define QG_PHPTR		Memi[$1+QG_PHOFF]		# --> Phantom array
define QG_PHANTOM	Memi[QG_PHPTR($1)+$2]		# Phantom value

# Macros to convert between array offset and grid position
define	QG_GRIDX	($2 - (($2-1)/QG_NAMPSX($1))*QG_NAMPSX($1))
define	QG_GRIDY	(($2-1)/QG_NAMPSX($1)+1)
define	QG_AMP		($2 + ($3-1) * QG_NAMPSX($1))

# Symbolic values for AMPTYPE codes
define	AMPDICT		"|11|12|21|22|"
define	AMP11		1		# BLHC
define	AMP12		2		# BRHC
define	AMP21		3		# TLHC
define	AMP22		4		# TRHC

define	SZ_AMPID	2
