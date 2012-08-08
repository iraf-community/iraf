# Define the public astrometry pacakge interface.

# Define the astrom parameters (1 -100)

define	PIO		1		# pointer to the i/o structure
define	PRCENTER	2		# pointer to the region structure
define	PFILTER		3		# pointer to the filter structure
define	PWCS		4		# pointer to the wcs structure
define	PIMPARS		5		# pointer to the image data structure


# Define the region parameters (101 - 200).

define	RCRA		101		# the field center ra
define	RCDEC		102		# the field center dec
define	RCRAWIDTH	103		# the field center ra width
define	RCDECWIDTH	104		# the field center dec width
define	RCRAUNITS	105		# the field center ra units
define	RCDECUNITS	106		# the fields center dec units
#define	RCCC		107		# the field center coordinate system
define	RCST		108		# the field center symbol table
define	RCSYSTEM	109		# the field center coordinate system
define	RCSOURCE	110		# the field center source

# Define the region commands for interactive mode.

define	RCCMDS		"|rcra|rcdec|rrawidth|rdecwidth|rcraunits|\
rcdecunits|rcsystem|"


# Define the field center symbol table structure. 

define	RCST_SZ_FNAME	(1 + SZ_FNAME) / 2
define	LEN_RCST_STRUCT	(15 + 2 * RCST_SZ_FNAME)

define  AT_RCSTRA	Memd[P2D($1)]               # the field center ra / lon
define  AT_RCSTDEC	Memd[P2D($1+2)]             # the field center dec / lat
define  AT_RCSTRAWIDTH	Memd[P2D($1+4)]             # the field ra / lon width
define  AT_RCSTDECWIDTH	Memd[P2D($1+6)]             # the field dec / lat width
define  AT_RCSTRAUNITS	Memi[$1+8]                  # the ra / lon units
define  AT_RCSTDECUNITS	Memi[$1+9]                  # the dec / lat units
define	AT_RCSTSOURCE	Memc[P2C($1+10)]            # the field center source
define	AT_RCSTNAME	Memc[P2C($1+15)]            # the field center file
define	AT_RCSTSYSTEM	Memc[P2C($1+15+RCST_SZ_FNAME)] # the field center cc system

define	DEF_LEN_RCST		100
define	DEF_RCST_ROOTNAME	"reg"


# Define the builtin region query fields.

define	AT_QRCRA	1
define	AT_QRCDEC	2
define	AT_QRCWIDTH	3
define	AT_QRCHWIDTH	4
define	AT_QRCRADIUS	5
define	AT_QRCRAWIDTH	6
define	AT_QRCDECWIDTH	7
define	AT_QRCRAHWIDTH	8
define	AT_QRCDECHWIDTH	9
define	AT_QRCXWIDTH	10
define	AT_QRCYWIDTH	11
define	AT_QRCXHWIDTH	12
define	AT_QRCYHWIDTH	13

define	AT_QRCFIELDS	"|ra|dec|width|hwidth|radius|rawidth|decwidth|\
rahwidth|dechwidth|xwidth|ywidth|xhwidth|yhwidth|"


# Define the i/o substructure parameters (201 - 300).

define	CATALOGS	201
define	SURVEYS		202
define	IMAGES		203
define	INPUT		204
define	OUTPUT		205
define	CATNAME		206
define	SVNAME		207
define	IMNAME		208
define	INFNAME		209
define	OUTFNAME	210
define	CATDB		211
define	IMDB		212

# Define the field center commands for interactive mode.

define	IOCMDS	"|catalogs|surveys|images|input|output|catname|svname|imname|\
infname|outfname|catdb|imdb|"

# Define the filtering / selection parameters (301 - 400)

define	FREVERSE	301
define	FREPLACE	302
define	FORAUNITS	303
define	FODECUNITS	304
define	FSORT		305
define	FOSYSTEM	306
define	FIRA		307
define	FIDEC		308
define	FORAFORMAT	309
define	FODECFORMAT	310
define	FIXP		311
define	FIYP		312
define	FIXC		313
define	FIYC		314
define	FOXFORMAT	315
define	FOYFORMAT	316
define	FIELDS		317
define	FEXPR		318
define	FNAMES		319
define	FNTYPES		320
define	FNUNITS		321
define	FNFORMATS	322

define	FSCMDS "|freverse|freplace|foraunits|fodecunits|fsort|fosystem|fira|\
fidec|fixp|fiyp|fixc|fiyc|foxformat|foyformat|foraformat|fodecformat|fields|\
fexpr|fnames|fntypes|fnunits|fnformats|"

# Units definitions.

# Define the supported celestial coordinate units.
# It appears only the first three are used and they must agree with skywcs.h. FV

define  AT_RA_UNITS    "|degrees|radians|hours|dms|hms|"
define  AT_DEC_UNITS   "|degrees|radians|invalid|dms|"
define  AT_DEGREES      1
define  AT_RADIANS      2
define  AT_HOURS        3
define  AT_DMS          4
define  AT_HMS          5

# Define the supported celestial coordinate error units.

define  AT_ERA_UNITS   "|asecs|masecs|secs|msecs|"
define  AT_EDEC_UNITS  "|asecs|masecs|"
define  AT_ASECS        1
define  AT_MASECS       2
define  AT_SECS         3
define  AT_MSECS        4

# Define the supported proper motion units.

define  AT_PMRA_UNITS  "|asecs/yr|masecs/yr|secs/yr|msecs/yr|"
define  AT_PMDEC_UNITS "|asecs/yr|masecs/yr|"
define  AT_ASECSYR      1
define  AT_MASECSYR     2
define  AT_SECSYR       3
define  AT_MSECSYR      4
