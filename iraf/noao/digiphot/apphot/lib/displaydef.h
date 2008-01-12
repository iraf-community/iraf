# APPHOT display structure

define	LEN_DISPLAYSTRUCT	10

# display parameters

define	AP_MKSKY		Memi[P2I($1)]	# Mark sky circles
define	AP_MKCENTER		Memi[P2I($1+1)]	# Mark center circles
define	AP_MKAPERT		Memi[P2I($1+2)]	# Mark aperture circles
define	AP_MKPOLYGON		Memi[P2I($1+3)]	# Mark polygon
define	AP_MKPSFBOX		Memi[P2I($1+4)]	# Mark psf fitting box
define	AP_RADPLOTS		Memi[P2I($1+5)]	# Make radial plots
define	AP_MKDETECTIONS		Memi[P2I($1+6)]	# Mark detected stars

# default values for the display parameters

define	DEF_MKSKY		NO
define	DEF_MKCENTER		NO
define	DEF_MKAPERT		NO
define	DEF_MKPOLYGON		NO
define	DEF_MKPSFBOX		NO
define	DEF_RADPLOTS		NO
define	DEF_MKDETECTIONS	NO
