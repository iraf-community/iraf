# Define the public default image wcs structure.

# Define the WCS parameters (# 401 - 500)

define	WXREF		401
define	WYREF		402
define	WXMAG		403
define	WYMAG		404
define	WXROT		405
define	WYROT		406
define	WRAREF		407
define	WDECREF		408
#define	WMJDOBS		409
define	WRAUNITS	410
define	WDECUNITS	411
define	WPROJ		412
define	WSYSTEM		413
#define	WEQUINOX	414
#define	WRADECSYS	415
define	WCST		416

# Define the default WCS parameters symbol table.
define	LEN_WCST_STRUCT	15

define	AT_WCSTKVAL	Memc[P2C($1)]

define	DEF_LEN_WCST	100
define	DEF_WCST_ROOTNAME	"wcs"

define	AT_NWFIELDS	12

# Define the wcs standard fields

define	AT_WFIELDS "|wxref|wyref|wxmag|wymag|wxrot|wyrot|wraref|wdecref|\
wraunits|wdecunits|wproj|wsystem|"

define	WCS_WXREF	1
define	WCS_WYREF	2
define	WCS_WXMAG	3
define	WCS_WYMAG	4
define	WCS_WXROT	5
define	WCS_WYROT	6
define	WCS_WRAREF	7
define	WCS_WDECREF	8
define	WCS_WRAUNITS	9
define	WCS_WDECUNITS	10
define	WCS_WPROJ	11
define	WCS_WSYSTEM	12

# Define the defaults standard field values.

define AT_WVALUES "|INDEF|INDEF|INDEF|INDEF|INDEF|INDEF|RA|DEC|\
INDEF|INDEF|tan|J2000|"

# Define the default wcs datatypes.

define	AT_WTYPES "|d|d|d|d|d|d|d|d|i|i|c|c|"

define	AT_WUNITS "|pixels|pixels|arcsec/pixel|arcsec/pixel|degrees|degrees|\
hours|degrees|||||"


# Define the image data parameters (# 501 - 600)

define	OBSERVAT	501
define	ESITELNG	502
define	ESITELAT	503
define	ESITEALT	504
define	ESITETZ		505
define	EMJDOBS		507
#define	EXPOSURE	508
define	EDATAMIN	509
define	EDATAMAX	510
define	EGAIN		511
define	ERDNOISE	512
define	EWAVLEN		513
define	ETEMP		514
define	EPRESS		515
define	IMST		516

# Define the default image parameters symbol table.

define	LEN_IMST_STRUCT	15

define	AT_IMSTKVAL	Memc[P2C($1)]

define	DEF_LEN_IMST		100
define	DEF_IMST_ROOTNAME	"impars"

# Define the nu,ber of image fields.

define	AT_NIMFIELDS 13

# Define the image data standard fields

define	AT_IMFIELDS "|observat|esitelng|esitelat|esitealt|esitetz|emjdobs|\
edatamin|edatamax|egain|erdnoise|ewavlen|etemp|epress|"

define	HDR_OBSERVAT	1
define	HDR_ESITELNG	2
define	HDR_ESITELAT	3
define	HDR_ESITEALT	4
define	HDR_ESITETZ	5
define	HDR_EMJDOBS	6
define	HDR_EDATAMIN	7
define	HDR_EDATAMAX	8
define	HDR_EGAIN	9
define	HDR_ERDNOISE	10
define	HDR_EWAVLEN	11
define	HDR_ETEMP	12
define	HDR_EPRESS	13

# Define the defaults standard field values.

define AT_IMVALUES "|OBSERVAT|INDEF|INDEF|INDEF|INDEF|MJD-OBS|\
INDEF|INDEF|GAIN|RDNOISE|INDEF|INDEF|INDEF|"

# Define the default wcs datatypes.

define	AT_IMTYPES "|c|d|d|r|r|d|r|r|r|r|r|r|r|r|"

define	AT_IMUNITS "||degrees|degrees|meters|||ADU|ADU|electrons|\
electrons/ADU|microns|degrees|mbars|"
