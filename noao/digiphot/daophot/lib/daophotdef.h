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

# DAOPHOT parameters (# 1 - 100)

# data dependent parameters

define	SFWHMPSF	1	# full-width at half-max of the psf (scale)
define	FWHMPSF		2	# full-width at half-max of the psf (pixels)
define	MAXGDATA	3	# maximum good data value (ADU)
define	MINGDATA	4	# minimum good data value (ADU)
define	PHOTADU		5 	# gain (photon / ADU)
define	READNOISE	6	# readout noise
define	SCALE		7 	# scale of the imge

# keyword parameters

define	CCDGAIN		8	# gain image header keyword
define	CCDREAD		9	# readnoise image header keyword
define	EXPTIME		10	# exposure time image header keyword
define	FILTER		11	# filter image header keyword
define	OBSTIME		12	# observing time image header keyword
define	AIRMASS		13	# airmass image header keyword

# observing parameters

define	XAIRMASS	14	# value of the airmass
define	IFILTER		15 	# filter id
define	ITIME		16	# integration time
define	OTIME		17 	# time of observation

# psf fitting parameters 

define	FUNCTION	18	# psf function analytic function
define	VARORDER	19	# order of psf variability (0, 1, or 2)
define	FEXPAND		20	# fraction pixel interpolation (yes or no)
define	SATURATED	21	# use saturated psf stars ?
define	NCLEAN		22	# the number of psf clean passes
define	FUNCLIST	23	# user function list
define	RPSFRAD		24	# requested psf radius (scale)
define	SPSFRAD		25	# psf radius (scale)
define	PSFRAD		26	# psf radius (pixels)
define	SMATCHRAD	27	# matching radius (scale)
define	MATCHRAD	28	# matching radius (pixels)

# star fitting parameters 

define	SFITRAD		29	# fitting radius (scale)
define	FITRAD		30	# fitting radius (pixels)
define	SANNULUS	31	# inner sky radius (scale)
define	ANNULUS		32	# inner sky radius (pixels)
define	SDANNULUS	33	# width of sky annulus (scale)
define	DANNULUS	34	# width of sky annulus (pixels)
define	SMERGERAD	35	# merging radius (scale)
define	MERGERAD	36	# merging radius (pixels)

define	CRITSNRATIO	37	# critical S/N overlap
define	MAXNSTAR	38	# maximum number of stars to fit
define	MAXGROUP	39	# maximum number of stars in group
define	MAXITER		40	# maximum number of iterations
define	RECENTER	41	# recenter ?
define	FITSKY		42	# refit the group sky ?
define	GROUPSKY	43	# use group or individual sky values
define	FLATERR		44	# the flat field error
define	PROFERR		45	# the profile or interpolation error 
define	CLIPRANGE	46	# clipping range
define	CLIPEXP		47	# clipping exponent


# file/image name and input/output parameters

define	TEXT		48	# text file
define	VERBOSE		49	# verbose mode
define	INIMAGE		50	# input image name
define	INPHOTFILE	51	# input photometry file
define	PSFIMAGE	52	# psf image name
define	COORDS		53	# input coordinate file
define	OUTPHOTFILE	54	# output photometry file
define	OUTIMAGE	55	# output image name
define	OUTREJFILE	56	# output rejected photometry file


# DAOPHOT structure definitions

define	LEN_DPSTRUCT (60 + 17 * SZ_FNAME + 17)

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

# parameters

define	DP_VARORDER	Memi[$1+10]	# order of psf variability
define	DP_FEXPAND	Memi[$1+11]	# expand the analytic function ?
define	DP_SATURATED	Memi[$1+12]	# use saturated psf stars ?
define	DP_NCLEAN	Memi[$1+13]	# number of psf cleaning passes
define	DP_MAXNSTAR	Memi[$1+14]	# maximum number of stars
define	DP_MAXGROUP	Memi[$1+15]	# maximum group size
define	DP_MAXITER	Memi[$1+16]	# maximum number of iterations
define	DP_RECENTER	Memi[$1+17]	# recenter ?
define	DP_FITSKY	Memi[$1+18]	# fit the sky ?
define	DP_GROUPSKY	Memi[$1+19]	# use the group sky value ?
define	DP_CLIPEXP	Memi[$1+20]	# clip exponent
define	DP_TEXT		Memi[$1+21]	# text file or table input/output ?
define	DP_VERBOSE	Memi[$1+22]	# verbose mode ?

define	DP_SCALE	Memr[$1+23]	# image scale in units/pixel
define	DP_SFWHMPSF	Memr[$1+24]	# fwhm of the psf (scale)
define	DP_FWHMPSF	Memr[$1+25]	# fwhm of the psf (pixels)
define	DP_MAXGDATA	Memr[$1+26]	# maximum good data value (ADU)
define	DP_MINGDATA	Memr[$1+27]	# minimum good data value (ADU)
define 	DP_PHOTADU	Memr[$1+28]	# gain in electrons/ADU
define	DP_READNOISE 	Memr[$1+29]	# readout noise (electrons)
define	DP_XAIRMASS	Memr[$1+30]	# value of the airmass
define	DP_ITIME	Memr[$1+31]	# value of the exposure time
define	DP_SMATCHRAD	Memr[$1+32]	# matching radius (scale)
define	DP_MATCHRAD	Memr[$1+33]	# matching radius (pixels)
define	DP_RPSFRAD	Memr[$1+34]	# requested psf radius (scale)
define	DP_SPSFRAD	Memr[$1+35]	# actual psf radius (scale)
define	DP_PSFRAD	Memr[$1+36]	# actual psf radius (pixels)
define	DP_SFITRAD	Memr[$1+37]	# fitting radius (scale)
define	DP_FITRAD	Memr[$1+38]	# fitting radius (pixels)
define	DP_SANNULUS	Memr[$1+39]	# inner sky radius (scale)
define	DP_ANNULUS	Memr[$1+40]	# inner sky radius (pixels)
define	DP_SDANNULUS	Memr[$1+41]	# width of sky annulus (scale)
define	DP_DANNULUS	Memr[$1+42]	# width of sky annulus (pixels)
define	DP_CRITSNRATIO	Memr[$1+43]	# critical S/N overlap 
define	DP_FLATERR	Memr[$1+44]	# percent flat field error
define	DP_PROFERR	Memr[$1+45]	# percent profile error
define	DP_CLIPRANGE	Memr[$1+46]	# clipping range
define	DP_SMERGERAD	Memr[$1+47]	# merging radius (scale)
define	DP_MERGERAD	Memr[$1+48]	# merging radius (pixels)

# temporary variables, not yet used

#define	DP_FITRADSQ	Memr[$1+47]	# fit radius squared (pixels)
#define	DP_PSFRADSQ	Memr[$1+48]	# psf radius squared (pixels)
#define	DP_RNOISESQ	Memr[$1+49]	# read noise squared (ADU)
#define	DP_PERR		Memr[$1+51]	# percentage error
#define	DP_PKERR	Memr[$1+52]	# peak error
#define	DP_TMAXGDATA	Memr[$1+53]	# true data max
#define	DP_TMINGDATA	Memr[$1+54]	# true data min

# file / image names

define	DP_INIMAGE	Memc[P2C($1+56)]                # input image name
define	DP_PSFIMAGE	Memc[P2C($1+56+SZ_FNAME+1)]     # psf image name
define	DP_INPHOTFILE	Memc[P2C($1+56+2*SZ_FNAME+2)]   # input photometry file
define	DP_COORDS	Memc[P2C($1+56+3*SZ_FNAME+3)]   # input coordinate file
define	DP_OUTIMAGE	Memc[P2C($1+56+4*SZ_FNAME+4)]   # output image name
define	DP_OUTPHOTFILE	Memc[P2C($1+56+5*SZ_FNAME+5)]   # output photometry file
define	DP_OUTREJFILE	Memc[P2C($1+56+6*SZ_FNAME+6)]   # output photometry file

define	DP_IFILTER	Memc[P2C($1+56+7*SZ_FNAME+7)] # filter id
define	DP_OTIME	Memc[P2C($1+56+8*SZ_FNAME+8)] # time of observation
define	DP_CCDGAIN	Memc[P2C($1+56+9*SZ_FNAME+9)] # gain keyword
define	DP_CCDREAD	Memc[P2C($1+56+10*SZ_FNAME+10)] # readnoise keyword
define	DP_EXPTIME	Memc[P2C($1+56+11*SZ_FNAME+11)] # exposure keyword
define	DP_FILTER	Memc[P2C($1+56+12*SZ_FNAME+12)] # filter keyword
define	DP_OBSTIME	Memc[P2C($1+56+13*SZ_FNAME+13)] # obstime keyword
define	DP_AIRMASS	Memc[P2C($1+56+14*SZ_FNAME+14)] # airmass keyword
define	DP_FUNCTION	Memc[P2C($1+56+15*SZ_FNAME+15)]	# analytic psf function
define	DP_FUNCLIST	Memc[P2C($1+56+16*SZ_FNAME+16)]	# psf function list

# PSF Fit Parameters

define	LEN_PSFFIT	(20)

define	DP_PSFUNCTION	Memi[$1]	# PSF type
define	DP_PSFNPARS	Memi[$1+1]	# number of psf parameters
define	DP_PSFSIZE	Memi[$1+2]	# size of PSF lut
define	DP_PSFPARS	Memi[$1+3]	# pointer to the PSF parameters 
define	DP_PSFLUT	Memi[$1+4]	# pointer to the PSF lookup table
define	DP_NVLTABLE	Memi[$1+5]	# number of variability luts
define	DP_NFEXTABLE	Memi[$1+6]	# number of PSF expansion luts
define  DP_PSFHEIGHT 	Memr[$1+7]	# brightness of psf
define	DP_PSFMAG	Memr[$1+8]	# magnitude of the PSF
define	DP_PSFX		Memr[$1+9]	# x position of the PSF
define	DP_PSFY		Memr[$1+10]	# y position of the PSF

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
