# STARLIST/GALLIST task definitions file

define	ST_STARS		1		# Make star list
define	ST_GALAXIES		2		# Make galaxies list

# Spatial distribution functions

define	ST_UNIFORM		1		# Uniform spatial distribution
define	ST_HUBBLE		2		# Hubble law
define	ST_SPFILE		3		# User input

# Luminosity distribution function

define	ST_UNIFORM		1		# Uniform luminosity function
define	ST_SALPETER		2		# Salpeter luminosity function
define	ST_BANDS		3		# Bahcall and Soneira
define	ST_LFFILE		4		# User input
define	ST_POWLAW		5		# Power law
define	ST_SCHECTER		6		# Schecter luminosity function

# Galaxies types

define	ST_DEVAUC		1		# Ellipticals
define	ST_EXP			2		# Spirals

define LEN_STSTRUCT		(45 + 4 * SZ_FNAME + 4)

define	ST_TYPE			Memi[$1]	# Stars or galaxies
define	ST_SPATIAL		Memi[$1+1]	# Spatial function
define	ST_XC			Memr[P2R($1+2)]	# X center
define	ST_YC			Memr[P2R($1+3)]	# Y center
define	ST_CORE			Memr[P2R($1+4)]	# Hubble core radius
define	ST_BASE			Memr[P2R($1+5)]	# Hubble baseline probability
define	ST_XMIN			Memr[P2R($1+6)]	# Minimum x value
define	ST_XMAX			Memr[P2R($1+7)]	# Maximum x value
define	ST_YMIN			Memr[P2R($1+8)]	# Minimum y value
define	ST_YMAX			Memr[P2R($1+9)]	# Maximum y value

define	ST_LUMINOSITY		Memi[$1+10]	# Luminosity function
define	ST_POWER		Memr[P2R($1+11)]# Power law
define	ST_MZERO		Memr[P2R($1+12)]# Zero point of magnitudes
define	ST_ALPHA		Memr[P2R($1+13)]# Bands function alpha
define	ST_BETA			Memr[P2R($1+14)]# Bands function beta
define	ST_DELTA		Memr[P2R($1+15)]# Bands function delta
define	ST_MSTAR		Memr[P2R($1+16)]# Bands function mstar
define	ST_MINMAG		Memr[P2R($1+17)]# Minimum magnitude
define	ST_MAXMAG		Memr[P2R($1+18)]# Maximum magnitude

define	ST_Z			Memr[P2R($1+19)]# Minimum redshift
define	ST_AR			Memr[P2R($1+20)]# Minimum roundness
define	ST_ERADIUS		Memr[P2R($1+21)]# Maximum elliptical radius
define	ST_SRADIUS		Memr[P2R($1+22)]# Maximum spiral radius
define	ST_EGALMIX		Memr[P2R($1+23)]# Egal fraction
define	ST_ABSORPTION		Memr[P2R($1+24)]# Absorption

define	ST_SSEED		Meml[$1+25]	# Spatial function seed
define	ST_LSEED		Meml[$1+26]	# Luminosity function seed
define	ST_NSSAMPLE		Memi[$1+27]	# Spatial function sampling
define	ST_NLSAMPLE		Memi[$1+28]	# Luminosity function sampling
define	ST_SORDER		Memi[$1+29]	# Spatial spline order
define	ST_LORDER		Memi[$1+30]	# Luminosity spline order
define	ST_NSTARS		Memi[$1+31]	# Number of stars

define	ST_RBINSIZE		Memr[P2R($1+32)]# Radial histogram resolution
define	ST_MBINSIZE		Memr[P2R($1+33)]# Magnitude histogram resolution
define	ST_DBINSIZE		Memr[P2R($1+34)]# Diameter histogram resolution
define	ST_EBINSIZE		Memr[P2R($1+35)]# Roundness histogram resolution
define	ST_PBINSIZE		Memr[P2R($1+36)]# Posang histogram resolution

define	ST_SPSTRING		Memc[P2C($1+37)]
define	ST_LFSTRING		Memc[P2C($1+37+SZ_FNAME+1)]
define	ST_SFILE		Memc[P2C($1+37+2*SZ_FNAME+2)]
define	ST_LFILE		Memc[P2C($1+37+3*SZ_FNAME+3)]

define	STCMDS "|show|nstars|spatial|xcenter|ycenter|core|base|xmin|xmax|\
ymin|ymax|luminosity|power|alpha|beta|delta|mstar|minmag|maxmag|||nssample|\
nlsample|sorder|lorder|sfile|lfile|rbinsize|mbinsize|ar|z|eradius|sradius|\
egalmix|dbinsize|ebinsize|pbinsize|ngals|mzero|absorption|"

define	SPFUNCS		"|uniform|hubble|file|"
define	LUMFUNCS	"|uniform|salpeter|bands|file|powlaw|"
define	GLUMFUNCS	"|uniform|||file|powlaw|schecter|"

define	STCMD_SHOW		1
define	STCMD_NSTARS		2
define	STCMD_SPATIAL		3
define	STCMD_XCENTER		4
define	STCMD_YCENTER		5
define	STCMD_CORE		6
define	STCMD_BASE		7
define	STCMD_XMIN		8
define	STCMD_XMAX		9
define	STCMD_YMIN		10
define	STCMD_YMAX		11
define	STCMD_LUMINOSITY	12
define	STCMD_POWER		13
define	STCMD_ALPHA		14
define	STCMD_BETA		15
define	STCMD_DELTA		16
define	STCMD_MSTAR		17
define	STCMD_MINMAG		18
define	STCMD_MAXMAG		19
define	STCMD_SSEED		20
define	STCMD_LSEED		21
define	STCMD_NSSAMPLE		22
define	STCMD_NLSAMPLE		23
define	STCMD_SORDER		24
define	STCMD_LORDER		25
define	STCMD_SFILE		26
define	STCMD_LFILE		27
define	STCMD_RBINSIZE		28
define	STCMD_MBINSIZE		29
define	STCMD_AR		30
define	STCMD_Z			31
define	STCMD_ERADIUS		32
define	STCMD_SRADIUS		33
define	STCMD_EGALMIX		34
define	STCMD_DBINSIZE		35
define	STCMD_EBINSIZE		36
define	STCMD_PBINSIZE		37
define	STCMD_NGALS		38
define	STCMD_MZERO		39
define	STCMD_ABSORPTION	40

# Miscellaneous default values

define	DEF_CORE		20.0
define	DEF_BASE		0.00

define	DEF_ALPHA		0.74
define	DEF_BETA		0.04
define	DEF_DELTA		0.294
define	DEF_MSTAR		1.28

define	DEF_GMSTAR		-20.6
define	DEF_GALPHA		-1.25
