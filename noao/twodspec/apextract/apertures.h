# Aperture Definition

define	AP_MAXAPS	100		# Maximum number of apertures

# Aperture structure --  The aperture structure consists of an integer
# identification number, the center of the aperture, the lower and upper
# limits of the aperture measured relative to the center, the axis for
# a curve giving an offset relative to the center, the CURFIT pointer
# describing the curve and an ICFIT pointer for background subtraction.
# The center and lower and upper limits are pairs of
# real numbers in the order column value and line value.  The edges of
# the aperture are given by:
#
#	low column = center column + low column offset + curve (line)
#	high column = center column + high column offset + curve (line)
#	low line = center line + low line offset + curve (column)
#	high line = center line + high line offset + curve (column)
#
# The curve is aplied to the column positions if the curve axis is 1 and
# to the line positions if the curve axis is 2.

define	AP_LEN	11			# Length of aperture description

define	AP_ID		Memi[$1]	# Aperture ID
define	AP_BEAM		Memi[$1+1]	# Aperture beam number
define	AP_CEN		Memr[$1+2+$2-1]	# Aperture center
define	AP_LOW		Memr[$1+4+$2-1]	# Aperture limit
define	AP_HIGH		Memr[$1+6+$2-1]	# Aperture limit
define	AP_AXIS		Memi[$1+8]	# Axis for curve
define	AP_CV		Memi[$1+9]	# Aperture curve
define	AP_IC		Memi[$1+10]	# ICFIT pointer
