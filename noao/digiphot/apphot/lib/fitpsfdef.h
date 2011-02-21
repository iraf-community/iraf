# FITPSF header file

define	LEN_PSFSTRUCT		(25 + SZ_FNAME + 1)

# fitpsf parameters

define	AP_PSFUNCTION	Memi[$1]	# PSF function
define	AP_PSFNPARS	Memi[$1+1]	# Number of parameters
define	AP_MAXNPARS	Memi[$1+2]	# Maximum number of parameters
define	AP_PSFAPERT	Memr[P2R($1+3)]	# Half-width of fitting box
define	AP_PMAXITER	Memi[$1+4]	# Maximum number of iterations
define	AP_PK2		Memr[P2R($1+5)]	# Rejection limit in sigma
define	AP_PNREJECT	Memi[$1+6]	# Maximum number of rejections

# fitpsf buffers

define	AP_PSFPIX	Memi[$1+7]	# Pointer to pixels
define	AP_PSFXPIX	Memi[$1+8]	# X coordinates array (not used)
define	AP_PSFYPIX	Memi[$1+9]	# Y coordinates array (not used)
define	AP_NPSFPIX	Memi[$1+10]	# Number of pixels (not used)
define	AP_LENPSFBUF	Memi[$1+11]	# Length of pixel buffer (not used)
define	AP_PXC		Memr[P2R($1+12)]# X center of subraster
define	AP_PYC		Memr[P2R($1+13)]# Y center of subraster
define	AP_PNX		Memi[$1+14]	# X dimension of subraster
define	AP_PNY		Memi[$1+15]	# Y dimension of subraster
define	AP_PFXCUR	Memr[P2R($1+16)]# X initial position
define	AP_PFYCUR	Memr[P2R($1+17)]# Y initial position
define	AP_OPFXCUR	Memr[P2R($1+18)]# X initial position
define	AP_OPFYCUR	Memr[P2R($1+19)]# Y initial position

# fitpsf answers

define	AP_PPARS	Memi[$1+20]	# fitted parameters
define	AP_PPERRS	Memi[$1+21]	# errors in the parameters
define	AP_PSFSTRING	Memc[P2C($1+22)] # functions string

# default fitpsf parameters

define	DEF_MAXNPARS		10
define	DEF_PSFUNCTION		AP_RADGAUSS
define	DEF_PMAXITER		50
define	DEF_PK2			3.0
define	DEF_PNREJECT		1
