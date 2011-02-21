# Header file for the IMSTATMISTICS task.

define	LEN_MIMSTAT	20

define	MIS_SUMX	Memd[P2D($1)]
define	MIS_SUMX2	Memd[P2D($1+2)]
define	MIS_SUMX3	Memd[P2D($1+4)]
define	MIS_SUMX4	Memd[P2D($1+6)]
define	MIS_LO		Memr[P2R($1+8)]
define	MIS_HI		Memr[P2R($1+9)]
define	MIS_MIN		Memr[P2R($1+10)]
define	MIS_MAX		Memr[P2R($1+11)]
define	MIS_MEAN	Memr[P2R($1+12)]
define	MIS_MEDIAN	Memr[P2R($1+13)]
define	MIS_MODE	Memr[P2R($1+14)]
define	MIS_STDDEV	Memr[P2R($1+15)]
define	MIS_SKEW	Memr[P2R($1+16)]
define	MIS_KURTOSIS	Memr[P2R($1+17)]
define	MIS_NPIX	Memi[$1+18]
define	MIS_SW		Memi[$1+19]

define	LEN_NSWITCHES	8

define	MIS_SKURTOSIS	Memi[$1]
define	MIS_SSKEW	Memi[$1+1]
define	MIS_SSTDDEV	Memi[$1+2]
define	MIS_SMODE	Memi[$1+3]
define	MIS_SMEDIAN	Memi[$1+4]
define	MIS_SMEAN	Memi[$1+5]
define	MIS_SMINMAX	Memi[$1+6]
define	MIS_SNPIX	Memi[$1+7]

define  MIS_FIELDS  "|image|npix|min|max|mean|midpt|mode|stddev|skew|kurtosis|mask|"
define	MIS_NFIELDS	11

define  IS_FIELDS  "|image|npix|min|max|mean|midpt|mode|stddev|skew|kurtosis|"

define	IS_NFIELDS	10

define	MIS_KIMAGE	"IMAGE"
define	MIS_KNPIX	"NPIX"
define	MIS_KMIN	"MIN"
define	MIS_KMAX	"MAX"
define	MIS_KMEAN	"MEAN"
define	MIS_KMEDIAN	"MIDPT"
define	MIS_KMODE	"MODE"
define	MIS_KSTDDEV	"STDDEV"
define	MIS_KSKEW	"SKEW"
define	MIS_KKURTOSIS	"KURTOSIS"
define	MIS_KMASK	"MASK"

define	MIS_FIMAGE	1
define	MIS_FNPIX	2
define	MIS_FMIN	3
define	MIS_FMAX	4
define	MIS_FMEAN	5
define	MIS_FMEDIAN	6
define	MIS_FMODE	7
define	MIS_FSTDDEV	8
define	MIS_FSKEW	9
define	MIS_FKURTOSIS	10
define	MIS_FMASK	11

define	MIS_FCOLUMN	"%10d"
define	MIS_FINTEGER	"%10d"
define	MIS_FREAL	"%10.4g"
define	MIS_FSTRING	"%20s"
