# POLYPHOT header file

define	LEN_PYSTRUCT	(25 + SZ_FNAME + 1)

# polyphot parameters

define	AP_PYZMAG	Memr[$1]	# Zero point of mag scale

# polygon parameters

define	AP_PYXMEAN	Memr[$1+1]	# Original mean X of polygon
define	AP_PYYMEAN	Memr[$1+2]	# Original mean Y of polygon
define  AP_PYCX		Memr[$1+3]	# Current mean X of polygon
define	AP_PYCY		Memr[$1+4]	# Current mean Y of polygon
define	AP_PYX		Memr[$1+5]	# Previous mean X of polygon
define	AP_PYY		Memr[$1+6]	# Previous mean Y of polygon
define	AP_PYNVER	Memi[$1+7]	# Number of vertices
define	AP_PYMINRAD	Memr[$1+8]	# Minimum sky fitting radius in scale

# polyphot answers

define	AP_PYBADPIX	Memi[$1+9]	# Are there bad pixels ?
define	AP_PYFLUX	Memd[P2D($1+10)]# Flux
define	AP_PYNPIX	Memd[P2D($1+12)]# Polygon area
define	AP_PYMAG	Memr[$1+14]	# Magnitude
define	AP_PYMAGERR	Memr[$1+15]	# Magnitude error

define	AP_PYNAME	Memc[P2C($1+21)]# Polygons file name

# polyphot defaults

define	DEF_PYZMAG	25.0
