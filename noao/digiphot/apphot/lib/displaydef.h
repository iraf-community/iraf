# APPHOT display structure

define	LEN_DISPLAYSTRUCT	10

# display parameters

define	AP_MKSKY		Memi[$1]	# mark sky circles
define	AP_MKCENTER		Memi[$1+1]	# mark center circles
define	AP_MKAPERT		Memi[$1+2]	# mark aperture circles
define	AP_MKPOLYGON		Memi[$1+3]	# mark polygon
define	AP_MKPSFBOX		Memi[$1+4]	# mark psf fitting box
define	AP_RADPLOTS		Memi[$1+5]	# make radial plots
define	AP_MKDETECTIONS		Memi[$1+6]	# mark detected stars

# default values for the display parameters

define	DEF_MKSKY		NO
define	DEF_MKCENTER		NO
define	DEF_MKAPERT		NO
define	DEF_MKPOLYGON		NO
define	DEF_MKPSFBOX		NO
define	DEF_RADPLOTS		NO
define	DEF_MKDETECTIONS	NO
