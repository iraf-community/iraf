
# Define the default image WCS data structure.

define	SZ_WPROJ	10
define	SZ_WFNAME	(1 + SZ_FNAME) / 2
define	SZ_OBSERVAT	20
define	SZ_DATEOBS	20

#define	LEN_PWCS	(25 + SZ_WPROJ + 2 * SZ_WFNAME)
define	LEN_PWCS	(25 + SZ_WPROJ + SZ_WFNAME)

define	AT_WXREF	Memd[P2D($1)]
define	AT_WYREF	Memd[P2D($1+2)]
define	AT_WXMAG	Memd[P2D($1+4)]
define	AT_WYMAG	Memd[P2D($1+6)]
define	AT_WXROT	Memd[P2D($1+8)]
define	AT_WYROT	Memd[P2D($1+10)]
define	AT_WRAREF	Memd[P2D($1+12)]
define	AT_WDECREF	Memd[P2D($1+14)]
#define	AT_WMJDOBS	Memd[P2D($1+16)]
#define	AT_WEQUINOX	Memd[P2D($1+18)]
define	AT_WRAUNITS	Memi[$1+20]
define	AT_WDECUNITS	Memi[$1+21]
define	AT_WCST		Memi[$1+22]
define	AT_WPROJ	Memc[P2C($1+23)]
define	AT_WSYSTEM	Memc[P2C($1+23+SZ_WPROJ)]
#define	AT_WRADECSYS	Memc[P2C($1+23+SZ_WPROJ+SZ_WFNAME)]

#define LEN_PIMPARS	(20 + SZ_OBSERVAT + SZ_DATEOBS)
define LEN_PIMPARS	(20 + SZ_OBSERVAT)

define	AT_ESITELNG	Memd[P2D($1)]
define	AT_ESITELAT	Memd[P2D($1+2)]
define	AT_EMJDOBS	Memd[P2D($1+4)]
define	AT_ESITEALT	Memr[P2R($1+6)]
define	AT_ESITETZ	Memr[P2R($1+7)]
#define	AT_EXPOSURE	Memr[P2R($1+8)]
define	AT_EDATAMIN	Memr[P2R($1+9)]
define	AT_EDATAMAX	Memr[P2R($1+10)]
define	AT_EGAIN	Memr[P2R($1+11)]
define	AT_ERDNOISE	Memr[P2R($1+12)]
define	AT_EWAVLEN	Memr[P2R($1+13)]
define	AT_ETEMP	Memr[P2R($1+14)]
define	AT_EPRESS	Memr[P2R($1+15)]
define	AT_IMST		Memi[$1+16]
define	AT_OBSERVAT	Memc[P2C($1+17)]
#define	AT_DATEOBS	Memc[P2C($1+17+SZ_OBSERVAT)]
