# Public definitions file for the SKYWCS library.

# Define the SKYWCS library parameters.

define	S_VXOFF		1
define	S_VYOFF		2
define	S_VXSTEP	3
define	S_VYSTEP	4
define	S_EQUINOX	5
define	S_EPOCH		6
define	S_CTYPE		7
define	S_RADECSYS	8
define	S_WTYPE		9
define	S_PLNGAX	10
define	S_PLATAX	11
define	S_XLAX		12
define	S_YLAX		13
define	S_PIXTYPE	14
define	S_NLNGAX	15
define	S_NLATAX	16
define	S_NLNGUNITS	17
define	S_NLATUNITS	18
define	S_COOSYSTEM	19
define	S_STATUS	20

# Define the list of supported fundamental coordinate systems.

define	FTYPE_LIST	"|fk4|noefk4|fk5|icrs|apparent|ecliptic|galactic|\
supergalactic|"

define	FTYPE_FK4		1
define	FTYPE_FK4NOE		2
define	FTYPE_FK5		3
define	FTYPE_ICRS		4
define	FTYPE_GAPPT		5
define	FTYPE_ECLIPTIC		6
define	FTYPE_GALACTIC		7
define	FTYPE_SUPERGALACTIC	8

# Define the list of supported coordinate systems.

define	CTYPE_LIST	"|equatorial|ecliptic|galactic|supergalactic|"

define	CTYPE_EQUATORIAL	1
define	CTYPE_ECLIPTIC		2
define	CTYPE_GALACTIC		3
define	CTYPE_SUPERGALACTIC	4

# Define the supported equatoral reference systems.

define  EQTYPE_LIST     "|fk4|fk4-no-e|fk5|icrs|gappt|"

define	EQTYPE_FK4	1
define	EQTYPE_FK4NOE	2
define	EQTYPE_FK5	3
define	EQTYPE_ICRS	4
define	EQTYPE_GAPPT	5

# Define the input coordinate file longitude latitude units.

define	SKY_LNG_UNITLIST	"|degrees|radians|hours|"
define	SKY_LAT_UNITLIST	"|degrees|radians|"

define	SKY_DEGREES	1
define	SKY_RADIANS	2
define	SKY_HOURS	3

# Define the list of supported image sky projection types.

define  WTYPE_LIST      "|lin|azp|tan|sin|stg|arc|zpn|zea|air|cyp|car|\
mer|cea|cop|cod|coe|coo|bon|pco|gls|par|ait|mol|csc|qsc|tsc|tnx|zpx|tpv|"

define  PTYPE_LIST      "|z|z|z|z|z|z|z|z|z|c|c|c|c|n|n|n|n|c|c|c|c|c|c|c|c|c|\
x|x|z|"

define  WTYPE_LIN       1
define  WTYPE_AZP       2
define  WTYPE_TAN       3
define  WTYPE_SIN       4
define  WTYPE_STG       5
define  WTYPE_ARC       6
define  WTYPE_ZPN       7
define  WTYPE_ZEA       8
define  WTYPE_AIR       9
define  WTYPE_CYP       10
define  WTYPE_CAR       11
define  WTYPE_MER       12
define  WTYPE_CEA       13
define  WTYPE_COP       14
define  WTYPE_COD       15
define  WTYPE_COE       16
define  WTYPE_COO       17
define  WTYPE_BON       18
define  WTYPE_PCO       19
define  WTYPE_GLS       20
define  WTYPE_PAR       21
define  WTYPE_AIT       22
define  WTYPE_MOL       23
define  WTYPE_CSC       24
define  WTYPE_QSC       25
define  WTYPE_TSC       26
define  WTYPE_TNX       27
define  WTYPE_ZPX       28
define  WTYPE_TPV       29

define  PTYPE_NAMES     "|z|c|n|x|"

define  PTYPE_ZEN       1
define  PTYPE_CYL       2
define  PTYPE_CON       3
define  PTYPE_EXP       4

# Define the supported image axis types.

define  AXTYPE_LIST     "|ra|dec|glon|glat|elon|elat|slon|slat|"

define  AXTYPE_RA       1
define  AXTYPE_DEC      2
define  AXTYPE_GLON     3
define  AXTYPE_GLAT     4
define  AXTYPE_ELON     5
define  AXTYPE_ELAT     6
define  AXTYPE_SLON     7
define  AXTYPE_SLAT     8

# Define the supported image pixel coordinate systems.

define	PIXTYPE_LIST	"|logical|tv|physical|world|"

define  PIXTYPE_LOGICAL		1
define  PIXTYPE_TV		2
define  PIXTYPE_PHYSICAL	3
define  PIXTYPE_WORLD		4
