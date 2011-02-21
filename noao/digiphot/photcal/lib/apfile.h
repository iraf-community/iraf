# Set up the definitions for the required input photometry file fields

define	CAT_NFIELDS	10

define	CAT_IMAGE	1		# the image name
define	CAT_XCENTER	2		# x center position
define	CAT_YCENTER	3		# y center position
define	CAT_IFILTER	4
define	CAT_ITIME	5
define	CAT_XAIRMASS	6		# the airmass
define	CAT_OTIME	7		# the airmass
define	CAT_RAPERT	8		# list of aperture radii
define	CAT_MAG		9		# list of magnitudes
define	CAT_MERR	10		# list of magnitude errors

# Set up the definitions for the required input airmass file fields

define	OBS_NFIELDS	5

define	OBS_IMAGE	1		# the image name
define	OBS_IFILTER	2		# filter id
define	OBS_ITIME	3		# exposure time
define	OBS_XAIRMASS	4		# the airmass
define	OBS_OTIME	5		# the time of observation

# Define the image name symbol table data structure

define	LEN_IMT_STRUCT	(8 + 2 * SZ_FNAME + 2)	

define	IMT_IMNO	Memi[$1]	         # the image sequence number
define	IMT_NENTRIES	Memi[$1+1]	         # number of data entries 
define	IMT_OFFSET	Memi[$1+2]	         # offset to image data
define	IMT_RO		Memr[P2R($1+3)]	         # the HWHM of the psf
define	IMT_ITIME	Memr[P2R($1+4)]		 # the exposure time
define	IMT_XAIRMASS	Memr[P2R($1+5)]	         # the airmass
define	IMT_NXAIRMASS	Memr[P2R($1+6)]	         # the normalized airmass
define	IMT_OTIME	Memr[P2R($1+7)]	         # the normalized airmass
define	IMT_IFILTER	Memc[P2C($1+8)]		 # the filter id
define	IMT_IMNAME	Memc[P2C($1+8+SZ_FNAME+1)]	 # the image name

define	LEN_IMTABLE	100		# initial length of the image table
define	DEF_BUFSIZE	1000		# default object data buffer size


# Define the fitting code structures

define	LEN_AGRSTRUCT	35

define	AGR_DM		Memi[$1]	# pointer to model estimates
define	AGR_DDDR	Memi[$1+1]	# pointer to r0 derivative estimates
define	AGR_T		Memi[$1+2]	# pointer to parameter derivatives
define	AGR_U		Memi[$1+3]	# pointer to accumulation matrix
define	AGR_V		Memi[$1+4]	# pointer to accumulation vector
define	AGR_POLD	Memi[$1+5]	# pointer to previous parameters incr
define	AGR_DP		Memi[$1+6]	# pointer to parameter increments
define	AGR_PCLAMPS	Memi[$1+7]	# pointer to parameter clamps
define	AGR_PARAMS	Memi[$1+8]	# pointer to parameters
define	AGR_PERRORS	Memi[$1+9]	# pointer to parameter errors
define	AGR_RBAR	Memi[$1+11]	# pointer to mean radii
define	AGR_THEO	Memi[$1+12]	# pointer to the model estimates
define	AGR_W		Memi[$1+13]	# pointer to the working weight array
define	AGR_ADOPT	Memi[$1+14]	# pointer to the adopted differences
define	AGR_WOBS	Memi[$1+15]	# pointer to the weighted observations
define	AGR_OBS		Memi[$1+16]	# pointer to the observations
define	AGR_WADO	Memi[$1+17]	# pointer to the errors in observations
define	AGR_CUM		Memi[$1+18]	# pointer to the accumulated differences
define	AGR_TCUM	Memi[$1+19]	# pointer to the accumulated differences
define	AGR_WCUM	Memi[$1+20]	# pointer to the accumulated errors
define	AGR_MAGS	Memi[$1+21]	# the temporary magnitude array
define	AGR_CMAGS	Memi[$1+22]	# the summed magnitude array
define	AGR_TMAGS	Memi[$1+23]	# the total summed magnitude array
define	AGR_WMAGS	Memi[$1+24]	# the summed magnitude error array
define	AGR_AVE		Memi[$1+25]	# pointer to the model averages 
define	AGR_RESID	Memi[$1+26]	# pointer to the residuals
define	AGR_RESSQ	Memi[$1+27]	# pointer to the residuals squared
define	AGR_WR		Memi[$1+28]	# pointer to the sum of the weights
define	AGR_TAVE	Memi[$1+29]	# pointer to the model averages 
define	AGR_TRESID	Memi[$1+30]	# pointer to the residuals
define	AGR_TRESSQ	Memi[$1+31]	# pointer to the residuals squared
define	AGR_TWR		Memi[$1+32]	# pointer to the sum of the weights

# Define some miscellaneous parameters

define	DEF_AIROFFSET	1.25
define	MAX_MTERMS	5
define	AGR_ITMAX	300

# Define the permitted plot types

define	AGR_FIT		1
define	AGR_ARESIDUALS	2
define	AGR_CUMULATIVE	3
define	AGR_XRESIDUALS	4
define	AGR_YRESIDUALS	5
define	AGR_BRESIDUALS	6

# Define the interactive colon commands

define	AGR_CMDS	"|show|image|nparams|swings|pwings|pgauss|rgescale|\
xwings|smallap|largeap|"

define	AGR_CMD_SHOW		1
define	AGR_CMD_IMAGE		2
define	AGR_CMD_MTERMS		3
define	AGR_CMD_SWINGS		4
define	AGR_CMD_PWINGS		5
define	AGR_CMD_PGAUSS		6
define	AGR_CMD_RGESCALE	7
define	AGR_CMD_XWINGS		8
define	AGR_CMD_SMALLAP		9
define	AGR_CMD_LARGEAP		10

define	AGR_SHOWCMDS	"|model|seeing|parameters|"

define	AGR_CMD_MODEL		1
define	AGR_CMD_SEEING		2
define	AGR_CMD_PARAMETERS	3
