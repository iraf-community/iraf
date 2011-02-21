# Header file for the IMSTATISTICS task.

define	LEN_IMSTAT	20

define	IS_SUMX		Memd[P2D($1)]
define	IS_SUMX2	Memd[P2D($1+2)]
define	IS_SUMX3	Memd[P2D($1+4)]
define	IS_SUMX4	Memd[P2D($1+6)]
define	IS_LO		Memr[P2R($1+8)]
define	IS_HI		Memr[P2R($1+9)]
define	IS_MIN		Memr[P2R($1+10)]
define	IS_MAX		Memr[P2R($1+11)]
define	IS_MEAN		Memr[P2R($1+12)]
define	IS_MEDIAN	Memr[P2R($1+13)]
define	IS_MODE		Memr[P2R($1+14)]
define	IS_STDDEV	Memr[P2R($1+15)]
define	IS_SKEW		Memr[P2R($1+16)]
define	IS_KURTOSIS	Memr[P2R($1+17)]
define	IS_NPIX		Memi[$1+18]

define  IS_FIELDS  "|image|npix|min|max|mean|midpt|mode|stddev|skew|kurtosis|"

define	NFIELDS		10

define	IS_KIMAGE	"IMAGE"
define	IS_KNPIX	"NPIX"
define	IS_KMIN		"MIN"
define	IS_KMAX		"MAX"
define	IS_KMEAN	"MEAN"
define	IS_KMEDIAN	"MIDPT"
define	IS_KMODE	"MODE"
define	IS_KSTDDEV	"STDDEV"
define	IS_KSKEW	"SKEW"
define	IS_KKURTOSIS	"KURTOSIS"

define	IS_FIMAGE	1
define	IS_FNPIX	2
define	IS_FMIN		3
define	IS_FMAX		4
define	IS_FMEAN	5
define	IS_FMEDIAN	6
define	IS_FMODE	7
define	IS_FSTDDEV	8
define	IS_FSKEW	9
define	IS_FKURTOSIS	10

define	IS_FCOLUMN	"%10d"
define	IS_FINTEGER	"%10d"
define	IS_FREAL	"%10.4g"
define	IS_FSTRING	"%20s"
