# Structure definitions for fitting surface graphically

define	LEN_GEOGRAPH	10

define	GG_NEWFUNCTION		Memi[$1]	# New function
define	GG_PLOTTYPE		Memi[$1+1]	# Type of plot
define	GG_OVERPLOT		Memi[$1+2]	# Overplot previous graph?
define	GG_FITERROR		Memi[$1+3]	# Error fitting x function
define	GG_CONSTXY		Memi[$1+4]	# Plot lines of constant x-y

# define plot types

define	FIT		1		# plot x y fit
define	XXRESID		2		# x fit residuals versus x
define	XYRESID		3		# x fit residuals versus y
define	YXRESID		4		# y fit residuals versus x
define	YYRESID		5		# y fit residuals versus y
