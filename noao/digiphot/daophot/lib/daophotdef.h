# DAOPHOT Structure

define	LEN_DPSTRUCT (50 + 20 * SZ_FNAME + 20)

# DAOPHOT structure definitions

# substructure pointers

define	DP_VERSION	Memi[$1]	# package version number
define	DP_PSF		Memi[$1+1]	# pointer to PSF structure
define	DP_PSFFIT	Memi[$1+2]	# pointer to PSFFIT structure
define	DP_GROUP	Memi[$1+3]	# pointer to GROUP structure
define	DP_PEAK		Memi[$1+4]	# pointer to the PEAK structure
define	DP_NSTAR	Memi[$1+5]	# pointer to NSTAR structure
define	DP_SUBSTAR	Memi[$1+6]	# pointer to SUBSTAR structure
define	DP_ADDSTAR	Memi[$1+7]	# pointer to ADDSTAR structure
define	DP_ALLSTAR	Memi[$1+8]	# pointer to ALLSTAR structure
define	DP_APSEL	Memi[$1+9]	# pointer to PHOT structure
define	DP_VERBOSE	Memi[$1+10]	# verbose mode

# parameters

define	DP_MAXITER	Memi[$1+12]	# maximum number of iterations
define	DP_MAXGROUP	Memi[$1+13]	# maximum group size
define	DP_MAXSTAR	Memi[$1+14]	# maximum number of stars
define	DP_RECENTER	Memi[$1+15]	# recenter (allstar)
define	DP_TEXT		Memi[$1+16]	# text file or table
define	DP_VARPSF	Memi[$1+17]	# variable psf
define	DP_CLIPEXP	Memi[$1+18]	# clip exponent (allstar)

define	DP_SCALE	Memr[$1+19]	# image scale in units/pixel
define	DP_RPSFRAD	Memr[$1+20]	# requested psf radius in scale units
define	DP_SPSFRAD	Memr[$1+21]	# actual psf radius in scale units
define	DP_PSFRAD	Memr[$1+22]	# actual radius of the PSF in pixels
define	DP_SFITRAD	Memr[$1+23]	# fitting radius in scale units
define	DP_FITRAD	Memr[$1+24]	# fitting radius in pixels
define	DP_SMATCHRAD	Memr[$1+25]	# matching radius in scale units
define	DP_MATCHRAD	Memr[$1+26]	# matching radius in pixels
define	DP_CRITOVLAP	Memr[$1+27]	# critical overlap for GROUP
define	DP_MAXGDATA	Memr[$1+28]	# maximum good data value
define	DP_MINGDATA	Memr[$1+29]	# minimum good data value
define 	DP_PHOT_ADC	Memr[$1+30]	# gain in electrons/ADU
define	DP_READ_NOISE 	Memr[$1+31]	# readout noise in electrons
define	DP_CLIPRANGE	Memr[$1+32]	# clip range (allstar)
define	DP_XAIRMASS	Memr[$1+33]	# value of the airmass
define	DP_ITIME	Memr[$1+34]	# value of the exposure time

# temporary variables, most not yet used

define	DP_FITRADSQ	Memr[$1+35]	# fit radius squared
define	DP_PSFRADSQ	Memr[$1+36]	# psf radius squared
define	DP_RNOISESQ	Memr[$1+37]	# read noise squared (ADU)
define	DP_RO32K	Memr[$1+38]	# norm read noise squared
define	DP_PKERR	Memr[$1+39]	# peak error
define	DP_TMAXGDATA	Memr[$1+40]	# true data max
define	DP_TMINGDATA	Memr[$1+41]	# true data min

# file names

define	DP_IMNAME	Memc[P2C($1+45)]                # image name
define	DP_PSFIMAGE	Memc[P2C($1+45+SZ_FNAME+1)]     # psfname
define	DP_APFILE	Memc[P2C($1+45+2*SZ_FNAME+2)]   # aperture phot file
define	DP_GRPSFFILE	Memc[P2C($1+45+3*SZ_FNAME+3)]   # group PSF file
define	DP_GRPFILE	Memc[P2C($1+45+4*SZ_FNAME+4)]   # group file
define	DP_NSTARFILE	Memc[P2C($1+45+5*SZ_FNAME+5)]   # nstar file
define	DP_SUBIMAGE	Memc[P2C($1+45+6*SZ_FNAME+6)]   # subtracted image
define	DP_ASTARFILE	Memc[P2C($1+45+7*SZ_FNAME+7)]   # allstar file
define	DP_PKFILE	Memc[P2C($1+45+8*SZ_FNAME+8)]   # allstar file
define	DP_ADDIMAGE	Memc[P2C($1+45+9*SZ_FNAME+9)]   # added image
define	DP_ADDFILE	Memc[P2C($1+45+10*SZ_FNAME+10)] # add file
define	DP_IFILTER	Memc[P2C($1+45+11*SZ_FNAME+11)] # filter id
define	DP_OTIME	Memc[P2C($1+45+12*SZ_FNAME+12)] # time of observation
define	DP_CCDGAIN	Memc[P2C($1+45+13*SZ_FNAME+13)] # gain keyword
define	DP_CCDREAD	Memc[P2C($1+45+14*SZ_FNAME+14)] # readnoise keyword
define	DP_EXPTIME	Memc[P2C($1+45+15*SZ_FNAME+15)] # exposure keyword
define	DP_FILTER	Memc[P2C($1+45+16*SZ_FNAME+16)] # filter keyword
define	DP_OBSTIME	Memc[P2C($1+45+17*SZ_FNAME+17)] # obstime keyword
define	DP_AIRMASS	Memc[P2C($1+45+18*SZ_FNAME+18)] # airmass keyword

# PSF Fit Parameters

define	LEN_PSFFIT (20)

define	DP_PSFSIZE	Memi[$1]	# size of PSF lookup table
define	DP_PSFLUT	Memi[$1+1]	# pointer to the PSF lookup table
define  DP_PSFHEIGHT 	Memr[$1+2]	# height of fitted Gaussian
define	DP_XPSF		Memr[$1+3]	# x Position of the PSF
define	DP_YPSF		Memr[$1+4]	# y Position of the PSF
define  DP_PSFDXCEN	Memr[$1+5]	# offset of x fit center to centroid
define 	DP_PSFDYCEN	Memr[$1+6]	# offset of y fit center to centroid
define	DP_PSFSIGX	Memr[$1+7]	# the x sigma of the Gaussian
define	DP_PSFSIGY	Memr[$1+8]	# the y sigma of the Gaussian
define	DP_PSFDHDXC	Memr[$1+9]	# x derivative
define	DP_PSFDHDYC	Memr[$1+10]	# y derivative
define	DP_PSFDHDSX	Memr[$1+11]	# x sigma derivative
define	DP_PSFDHDSY	Memr[$1+12]	# y sigma derivative
define	DP_PSFMAG	Memr[$1+13]	# Magnitude of the PSF

# default daophot parameter values

define	DEF_SCALE	1.0
define	DEF_PSFRAD	11.0
define	DEF_FITRAD	3.0
define	DEF_MAXITER	50
define	DEF_MAXGROUP	60
define	DEF_MAXSTAR	50000

# some useful macros

define	DAO_GOODDATA	($2>DP_MINGDATA($1)&&$2<DP_MAXGDATA($1))
define	DAO_RELBRIGHT	(10.0**(0.4*(DP_PSFMAG($1) - $2)))
define	DAO_MAGCHECK	(0.4*(DP_PSFMAG($1) - $2))
