# DAOPHOT parameters (# 1 - 50)

# data dependent parameters

define	SCALE		1 	# scale of the imge
define	MAXGDATA	2	# maximum good data value
define	MINGDATA	3	# minimum good data value
define	READ_NOISE	4	# readout noise
define	PHOT_ADC	5 	# gain in photon/ADU

# observing parameters

define	XAIRMASS	6	# value of the airmass
define	IFILTER		7 	# filter id
define	ITIME		8	# integration time
define	OTIME		9 	# time of observation

# fitting parameters 

define	VARPSF		10	# variable psf
define	RPSFRAD		11
define	SPSFRAD		12	# scaled psf radius
define	SFITRAD		13	# scaled fitting radius
define	SMATCHRAD	14	# scaled matching radius
define	PSFRAD		15	# radius of the PSF
define	FITRAD		16	# fitting radius in pixels
define	MATCHRAD	17	# match radiius with PHOT results
define	MAXITER		18	# maximum number of iterations
define	MAXGROUP	19	# maximum number of stars in group
define	MAXSTAR		20	# maximum number of stars to fit
define	CRITOVLAP	21	# critical overlap with GROUP results
define	CLIPRANGE	22	# clipping range
define	CLIPEXP		23	# clipping exponent
define	RECENTER	24	# recenter

# output parameters

define	TEXT		25	# text file
define	VERBOSE		26	# verbose mode
define	IMNAME		27	# image name
define	APFILE		28	# aperture photometry file
define	PSFIMAGE	29	# psf image
define	GRPSFFILE	30	# group psf file
define	PKFILE		31	# peak output file
define	GRPFILE		32	# group file
define	NSTARFILE	33	# nstar file
define	SUBIMAGE	34	# subtracted image
define	ALLSTARFILE	35	# allstar output file
define	ADDIMAGE	36	# image with added stars
define	ADDFILE		37	# file of added stars

# keyword parameters

define	CCDGAIN		38	# gain image header keyword
define	CCDREAD		39	# readnoise image header keyword
define	EXPTIME		40	# exposure time image header keyword
define	FILTER		41	# filter image header keyword
define	OBSTIME		42	# observing time image header keyword
define	AIRMASS		43	# airmass image header keyword
