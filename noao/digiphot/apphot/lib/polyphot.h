# POLYPHOT header file

# polyphot error codes (# 801 - 900)

define	PY_NOPOLYGON		801
define	PY_OUTOFBOUNDS		802
define	PY_NOPIX		803
define	PY_NOSKYMODE		804
define	PY_BADDATA		805
define	PY_OK			0

# polyphot parameters and answers (# 801 - 900)

define	PYXMEAN			801
define	PYYMEAN			802
define	PYCX			803
define	PYCY			804
define	PYFLUX			805
define	PYNPIX			806
define	PYNVER			807
define	PYMINRAD		808
define	PYX			809
define	PYY			810
define	PYZMAG			811
define	PYMAG			812
define	PYMAGERR		813
define	PYNAME			814
define	PYBADPIX		815

# polyphot keywords

define	KY_PYZMAG	"zmag"
define	KY_PYNAME	"polyfile"

# polyphot units

define	UN_PYZMAG	"zeropoint"

# miscellaneous polyphot definitions

define	MAX_NVERTICES		900
define	NEXT_POLYGON		0
define	NEXT_OBJECT		1
define	THIS_OBJECT		2

# polyphot strings

define	PYSHOWARGS	"|center|sky|phot|data|"
define	PYCMDS		"|zmag|mkpolygon|polygons|"

define	PLCMD_CENTER		1
define	PLCMD_SKY		2
define	PLCMD_PHOT		3
define	PLCMD_DATA		4

define	PLCMD_ZMAG		1
define	PLCMD_MKPOLYGON		2
define	PLCMD_POLYGONS		3
