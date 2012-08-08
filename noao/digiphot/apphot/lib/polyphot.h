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
define	OPYXMEAN		805
define	OPYYMEAN		806
define	OPYCX			807
define	OPYCY			808
define	PYFLUX			809
define	PYNPIX			810
define	PYNVER			811
define	PYMINRAD		812
define	PYX			813
define	PYY			814
define	PYZMAG			815
define	PYMAG			816
define	PYMAGERR		817
define	PYNAME			818
define	PYROOT			819
define	PYBADPIX		820

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
