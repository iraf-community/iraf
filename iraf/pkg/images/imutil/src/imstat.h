# Header file for the IMSTATISTTICS task.

define	LEN_IMSTAT	20

define	IST_SUMX	Memd[P2D($1)]
define	IST_SUMX2	Memd[P2D($1+2)]
define	IST_SUMX3	Memd[P2D($1+4)]
define	IST_SUMX4	Memd[P2D($1+6)]
define	IST_LO		Memr[P2R($1+8)]
define	IST_HI		Memr[P2R($1+9)]
define	IST_MIN		Memr[P2R($1+10)]
define	IST_MAX		Memr[P2R($1+11)]
define	IST_MEAN	Memr[P2R($1+12)]
define	IST_MEDIAN	Memr[P2R($1+13)]
define	IST_MODE	Memr[P2R($1+14)]
define	IST_STDDEV	Memr[P2R($1+15)]
define	IST_SKEW	Memr[P2R($1+16)]
define	IST_KURTOSIS	Memr[P2R($1+17)]
define	IST_NPIX	Memz[P2Z($1+18)]
define	IST_SW		Memp[$1+19]

define	LEN_NSWITCHES	8

define	IST_SKURTOSIS	Memi[P2I($1)]
define	IST_SSKEW	Memi[P2I($1+1)]
define	IST_SSTDDEV	Memi[P2I($1+2)]
define	IST_SMODE	Memi[P2I($1+3)]
define	IST_SMEDIAN	Memi[P2I($1+4)]
define	IST_SMEAN	Memi[P2I($1+5)]
define	IST_SMINMAX	Memi[P2I($1+6)]
define	IST_SNPIX	Memi[P2I($1+7)]

define  IST_FIELDS  "|image|npix|min|max|mean|midpt|mode|stddev|skew|kurtosis|"

define	IST_NFIELDS	10

define	IST_KIMAGE	"IMAGE"
define	IST_KNPIX	"NPIX"
define	IST_KMIN	"MIN"
define	IST_KMAX	"MAX"
define	IST_KMEAN	"MEAN"
define	IST_KMEDIAN	"MIDPT"
define	IST_KMODE	"MODE"
define	IST_KSTDDEV	"STDDEV"
define	IST_KSKEW	"SKEW"
define	IST_KKURTOSIS	"KURTOSIS"

define	IST_FIMAGE	1
define	IST_FNPIX	2
define	IST_FMIN	3
define	IST_FMAX	4
define	IST_FMEAN	5
define	IST_FMEDIAN	6
define	IST_FMODE	7
define	IST_FSTDDEV	8
define	IST_FSKEW	9
define	IST_FKURTOSIS	10

define	IST_FCOLUMN	"%10d"
define	IST_FINTEGER	"%10d"
define	IST_FREAL	"%10.4g"
define	IST_FSTRING	"%20s"
