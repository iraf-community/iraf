# DAOPHOT header file

# DAOPHOT PSF function types

define	FCTN_FTYPES	"|gauss|lorentz|moffat15|moffat25|penny1|penny2|auto|"
define	FCTN_GAUSS	1
define	FCTN_LORENTZ	2
define	FCTN_MOFFAT15	3
define	FCTN_MOFFAT25	4
define	FCTN_PENNY1	5
define	FCTN_PENNY2	6
define	FCTN_AUTO	7

define	FCTN_NFTYPES	6

# WCS types

define	WCS_LOGICAL	1
define	WCS_TV		2
define	WCS_PHYSICAL	3
define	WCS_WORLD	4

define	WCSINSTR	"|logical|tv|physical|world|"
define	WCSOUTSTR	"|logical|tv|physical|"
define	WCSPSFSTR	"|logical|tv|physical|"

# DAOPHOT parameters (# 1 - 100)

# wcs parameters

define	MW		1	# mwcs pointer
define	WCSIN		2	# input wcs type
define	WCSOUT		3	# output wcs type
define	WCSPSF		4	# psf wcs type
define	CTIN		5	# input transform
define	CTOUT		6	# output transform
define	CTPSF		7	# psf transform

# data dependent parameters

define	SFWHMPSF	8	# full-width at half-max of the psf (scale)
define	FWHMPSF		9	# full-width at half-max of the psf (pixels)
define	MAXGDATA	10	# maximum good data value (ADU)
define	MINGDATA	11	# minimum good data value (ADU)
define	PHOTADU		12 	# gain (e- / ADU)
define	READNOISE	13	# readout noise (e-)
define	SCALE		14 	# scale of the imge

# keyword parameters

define	CCDGAIN		15	# gain image header keyword
define	CCDREAD		16	# readnoise image header keyword
define	EXPTIME		17	# exposure time image header keyword
define	FILTER		18	# filter image header keyword
define	OBSTIME		19	# observing time image header keyword
define	AIRMASS		20	# airmass image header keyword

# observing parameters

define	XAIRMASS	21	# value of the airmass
define	IFILTER		22 	# filter id
define	ITIME		23	# integration time
define	OTIME		24 	# time of observation

# psf fitting parameters 

define	FUNCTION	25	# psf function analytic function
define	VARORDER	26	# order of psf variability (0, 1, or 2)
define	FEXPAND		27	# fraction pixel interpolation (yes or no)
define	SATURATED	28	# use saturated psf stars ?
define	NCLEAN		29	# the number of psf clean passes
define	FUNCLIST	30	# user function list
define	RPSFRAD		31	# requested psf radius (scale)
define	SPSFRAD		32	# psf radius (scale)
define	PSFRAD		33	# psf radius (pixels)
define	SMATCHRAD	34	# matching radius (scale)
define	MATCHRAD	35	# matching radius (pixels)

# star fitting parameters 

define	SFITRAD		36	# fitting radius (scale)
define	FITRAD		37	# fitting radius (pixels)
define	SANNULUS	38	# inner sky radius (scale)
define	ANNULUS		39	# inner sky radius (pixels)
define	SDANNULUS	40	# width of sky annulus (scale)
define	DANNULUS	41	# width of sky annulus (pixels)
define	SMERGERAD	42	# merging radius (scale)
define	MERGERAD	43	# merging radius (pixels)

define	CRITSNRATIO	44	# critical S/N overlap
define	MAXNSTAR	45	# maximum number of stars to fit
define	MAXGROUP	46	# maximum number of stars in group
define	MAXITER		47	# maximum number of iterations
define	RECENTER	48	# recenter ?
define	FITSKY		49	# refit the group sky ?
define	GROUPSKY	50	# use group or individual sky values
define	FLATERR		51	# the flat field error
define	PROFERR		52	# the profile or interpolation error 
define	CLIPRANGE	53	# clipping range
define	CLIPEXP		54	# clipping exponent


# file/image name and input/output parameters

define	TEXT		55	# text file
define	VERBOSE		56	# verbose mode
define	INIMAGE		57	# input image name
define	INPHOTFILE	58	# input photometry file
define	PSFIMAGE	59	# psf image name
define	COORDS		60	# input coordinate file
define	OUTPHOTFILE	61	# output photometry file
define	OUTIMAGE	62	# output image name
define	OUTREJFILE	63	# output rejected photometry file

define	MEMFUDGE	1.05

# DAOPHOT structure definitions

define	LEN_DPSTRUCT (70 + 17 * SZ_FNAME + 17)

# sub-structure pointers

define	DP_VERSION	Memi[$1]	# package version number
define	DP_ADDSTAR	Memi[$1+1]	# pointer to addstar structure
define	DP_ALLSTAR	Memi[$1+2]	# pointer to allstar structure (used)
define	DP_APSEL	Memi[$1+3]	# pointer to phot structure (used)
define	DP_GROUP	Memi[$1+4]	# pointer to group structure
define	DP_NSTAR	Memi[$1+5]	# pointer to nstar structure (used)
define	DP_PEAK		Memi[$1+6]	# pointer to the peak structure
define	DP_PSF		Memi[$1+7]	# pointer to psf structure (used)
define	DP_PSFFIT	Memi[$1+8]	# pointer to psffit structure (used)
define	DP_SUBSTAR	Memi[$1+9]	# pointer to substar structure

# the wcs parameters

define  DP_WCSIN        Memi[$1+10]     # the input wcs
define  DP_WCSOUT       Memi[$1+11]     # the output wcs
define  DP_WCSPSF       Memi[$1+12]     # the psf wcs
define	DP_MW		Memi[$1+13]	# pointer to mwcs structure
define  DP_CTIN         Memi[$1+14]     # the input transformation pointer
define  DP_CTOUT        Memi[$1+15]     # the output transformation pointer
define  DP_CTPSF        Memi[$1+16]     # the psf transformation pointer

# parameters

define	DP_VARORDER	Memi[$1+17]	# order of psf variability
define	DP_FEXPAND	Memi[$1+18]	# expand the analytic function ?
define	DP_SATURATED	Memi[$1+19]	# use saturated psf stars ?
define	DP_NCLEAN	Memi[$1+20]	# number of psf cleaning passes
define	DP_MAXNSTAR	Memi[$1+21]	# maximum number of stars
define	DP_MAXGROUP	Memi[$1+22]	# maximum group size
define	DP_MAXITER	Memi[$1+23]	# maximum number of iterations
define	DP_RECENTER	Memi[$1+24]	# recenter ?
define	DP_FITSKY	Memi[$1+25]	# fit the sky ?
define	DP_GROUPSKY	Memi[$1+26]	# use the group sky value ?
define	DP_CLIPEXP	Memi[$1+27]	# clip exponent
define	DP_TEXT		Memi[$1+28]	# text file or table input/output ?
define	DP_VERBOSE	Memi[$1+29]	# verbose mode ?

define	DP_SCALE	Memr[P2R($1+30)]	# image scale in units/pixel
define	DP_SFWHMPSF	Memr[P2R($1+31)]	# fwhm of the psf (scale)
define	DP_FWHMPSF	Memr[P2R($1+32)]	# fwhm of the psf (pixels)
define	DP_MAXGDATA	Memr[P2R($1+33)]	# maximum good data value (ADU)
define	DP_MINGDATA	Memr[P2R($1+34)]	# minimum good data value (ADU)
define 	DP_PHOTADU	Memr[P2R($1+35)]	# gain in electrons/ADU
define	DP_READNOISE 	Memr[P2R($1+36)]	# readout noise (electrons)
define	DP_XAIRMASS	Memr[P2R($1+37)]	# value of the airmass
define	DP_ITIME	Memr[P2R($1+38)]	# value of the exposure time
define	DP_SMATCHRAD	Memr[P2R($1+39)]	# matching radius (scale)
define	DP_MATCHRAD	Memr[P2R($1+40)]	# matching radius (pixels)
define	DP_RPSFRAD	Memr[P2R($1+41)]	# requested psf radius (scale)
define	DP_SPSFRAD	Memr[P2R($1+42)]	# actual psf radius (scale)
define	DP_PSFRAD	Memr[P2R($1+43)]	# actual psf radius (pixels)
define	DP_SFITRAD	Memr[P2R($1+44)]	# fitting radius (scale)
define	DP_FITRAD	Memr[P2R($1+45)]	# fitting radius (pixels)
define	DP_SANNULUS	Memr[P2R($1+46)]	# inner sky radius (scale)
define	DP_ANNULUS	Memr[P2R($1+47)]	# inner sky radius (pixels)
define	DP_SDANNULUS	Memr[P2R($1+48)]	# width of sky annulus (scale)
define	DP_DANNULUS	Memr[P2R($1+49)]	# width of sky annulus (pixels)
define	DP_CRITSNRATIO	Memr[P2R($1+50)]	# critical S/N overlap 
define	DP_FLATERR	Memr[P2R($1+51)]	# percent flat field error
define	DP_PROFERR	Memr[P2R($1+52)]	# percent profile error
define	DP_CLIPRANGE	Memr[P2R($1+53)]	# clipping range
define	DP_SMERGERAD	Memr[P2R($1+54)]	# merging radius (scale)
define	DP_MERGERAD	Memr[P2R($1+55)]	# merging radius (pixels)

# temporary variables, not yet used

#define	DP_FITRADSQ	Memr[P2R($1+56)]	# fit radius squared (pixels)
#define	DP_PSFRADSQ	Memr[P2R($1+57)]	# psf radius squared (pixels)
#define	DP_RNOISESQ	Memr[P2R($1+58)]	# read noise squared (ADU)
#define	DP_PERR		Memr[P2R($1+59)]	# percentage error
#define	DP_PKERR	Memr[P2R($1+60)]	# peak error
#define	DP_TMAXGDATA	Memr[P2R($1+61)]	# true data max
#define	DP_TMINGDATA	Memr[P2R($1+62)]	# true data min

# file / image names

define	DP_INIMAGE	Memc[P2C($1+64)]                # input image name
define	DP_PSFIMAGE	Memc[P2C($1+64+SZ_FNAME+1)]     # psf image name
define	DP_INPHOTFILE	Memc[P2C($1+64+2*SZ_FNAME+2)]   # input photometry file
define	DP_COORDS	Memc[P2C($1+64+3*SZ_FNAME+3)]   # input coordinate file
define	DP_OUTIMAGE	Memc[P2C($1+64+4*SZ_FNAME+4)]   # output image name
define	DP_OUTPHOTFILE	Memc[P2C($1+64+5*SZ_FNAME+5)]   # output photometry file
define	DP_OUTREJFILE	Memc[P2C($1+64+6*SZ_FNAME+6)]   # output photometry file

# keyword / parameter names

define	DP_IFILTER	Memc[P2C($1+64+7*SZ_FNAME+7)] # filter id
define	DP_OTIME	Memc[P2C($1+64+8*SZ_FNAME+8)] # time of observation
define	DP_CCDGAIN	Memc[P2C($1+64+9*SZ_FNAME+9)] # gain keyword
define	DP_CCDREAD	Memc[P2C($1+64+10*SZ_FNAME+10)] # readnoise keyword
define	DP_EXPTIME	Memc[P2C($1+64+11*SZ_FNAME+11)] # exposure keyword
define	DP_FILTER	Memc[P2C($1+64+12*SZ_FNAME+12)] # filter keyword
define	DP_OBSTIME	Memc[P2C($1+64+13*SZ_FNAME+13)] # obstime keyword
define	DP_AIRMASS	Memc[P2C($1+64+14*SZ_FNAME+14)] # airmass keyword
define	DP_FUNCTION	Memc[P2C($1+64+15*SZ_FNAME+15)]	# analytic psf function
define	DP_FUNCLIST	Memc[P2C($1+64+16*SZ_FNAME+16)]	# psf function list

# PSF Fit Parameters

define	LEN_PSFFIT	(20)

define	DP_PSFUNCTION	Memi[$1]	# PSF type
define	DP_PSFNPARS	Memi[$1+1]	# number of psf parameters
define	DP_PSFSIZE	Memi[$1+2]	# size of PSF lut
define	DP_PSFPARS	Memi[$1+3]	# pointer to the PSF parameters 
define	DP_PSFLUT	Memi[$1+4]	# pointer to the PSF lookup table
define	DP_NVLTABLE	Memi[$1+5]	# number of variability luts
define	DP_NFEXTABLE	Memi[$1+6]	# number of PSF expansion luts
define  DP_PSFHEIGHT 	Memr[P2R($1+7)]	# brightness of psf
define	DP_PSFMAG	Memr[P2R($1+8)]	# magnitude of the PSF
define	DP_PSFX		Memr[P2R($1+9)]	# x position of the PSF
define	DP_PSFY		Memr[P2R($1+10)]# y position of the PSF

define	MAX_NFCTNPARS	(6)		# max number of pars in psf function
define	MAX_NEXPTERMS	(11)		# max number of expansion terms

# default daophot parameter values

define	DEF_SCALE	1.0
define	DEF_FWHMPSF	2.5
define	DEF_PSFRAD	11.0
define	DEF_FITRAD	2.5
define	DEF_ANNULUS	10.0
define	DEF_DANNULUS	10.0
define	DEF_MAXNSTAR	5000
define	DEF_MAXGROUP	60
define	DEF_MAXITER	50
define	DEF_DEFNAME	"default"
define	DEF_LENDEFNAME	7

# some useful macros

define	DAO_GOODDATA	($2>DP_MINGDATA($1)&&$2<DP_MAXGDATA($1))
define	DAO_RELBRIGHT	(10.0**(0.4*(DP_PSFMAG($1) - $2)))
define	DAO_MAGCHECK	(0.4*(DP_PSFMAG($1) - $2))
