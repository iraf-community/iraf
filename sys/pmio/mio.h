# MIO -- Image i/o through a mask.
# 
#     The MIO routines are used to sequentially read or write the portion of
# the image IM which is "visible" through the mask denoted by the mask name
# PLNAME or the open mask descriptor PM.  An image pixel is said to be visible
# if the associated mask pixel is nonzero.  The PMIO routines may be used prior
# to performing any i/o to prepare the desired mask, e.g., a mask might be
# inverted to access only the "masked" pixels, or a mask might be ANDed with a
# region to limit i/o to only the portions of the image visible through both
# the mask and the region.  Certain commonly performed mask conversions may be
# performed at MIO_OPEN time via the FLAGS argument, e.g., inversion, or
# conversion of an integer mask to a boolean mask.
# 
# 		  mp = mio_open (mask, flags, im)
# 		 mp = mio_openo (pm, im)
# 	      value = mio_stati (mp, param)
# 		       mio_seti (mp, param, value)
# 		   mio_setrange (mp, vs, ve, ndim)
#  n|EOF = mio_[gp]lseg[silrdx] (mp, ptr, mval, v, npix)
# 		      mio_close (mp)
# 
# mio_open flags (defined in <pmset.h>):
# 
# 	INVERT_MASK		invert mask (PIX_NOT(PIX_SRC))
# 	BOOLEAN_MASK		convert mask to boolean if not already
# 
# set/stat params (defined in <pmset.h>):
# 	
# 	P_PMDES			pixel mask descriptor
# 	P_IMDES			image descriptor
#	P_REGCOORDS		mio_setrange region relative coords
# 
# The get/put line segment i/o routines return successive line segments of
# constant value from the data image IM, advancing through the image in storage
# order starting at the position vector [1,1,1...,N].  A pointer to each line
# segment is returned in PTR, with the associated integer mask value in MVAL,
# and the vector coordinates and length of the line segment in V and NPIX.
# EOF is returned when there are no more visible pixels to be read through the
# masked region.

define	LEN_MIODES	50
define	M_IM		Memi[$1]		# image descriptor
define	M_PM		Memi[$1+1]		# mask descriptor
define	M_PMCLOSE	Memi[$1+2]		# have mio_close close mask
define	M_DEPTH		Memi[$1+3]		# have mio_close close mask
define	M_ACTIVE	Memi[$1+4]		# set once i/o begins
define	M_LBP		Memi[$1+5]		# line buffer pointer
define	M_RLP		Memi[$1+6]		# range list pointer
define	M_RLI		Memi[$1+7]		# range list index
define	M_NDIM		Memi[$1+8]		# dimensionality of section
define	M_LINEIO	Memi[$1+9]		# section is entire line
define	M_REGCOORDS	Memi[$1+10]		# region relative coords
define	M_V		Meml[$1+11+$2-1]	# current vector
define	M_VS		Meml[$1+20+$2-1]	# start vector
define	M_VE		Meml[$1+30+$2-1]	# end vector
define	M_VN		Meml[$1+40+$2-1]	# size of section
