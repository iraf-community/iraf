# ACEDETECT parameter structure.
define	PAR_SZSTR	199		# Length of strings in par structure
define	PAR_LEN		128		# Length of parameter structure

define	PAR_IMLIST	Memi[$1+$2-1]	# List of images (2)
define	PAR_BPMLIST	Memi[$1+$2+1]	# List of bad pixel masks (2)
define	PAR_SKYLIST	Memi[$1+$2+3]	# List of skys (2)
define	PAR_SIGLIST	Memi[$1+$2+5]	# List of sigmas (2)
define	PAR_EXPLIST	Memi[$1+$2+7]	# List of sigmas (2)
define	PAR_GAINLIST	Memi[$1+$2+9]	# List of measurement gain maps (2)
define	PAR_SCALELIST	Memi[$1+$2+11]	# List of scales (2)
define	PAR_OMLIST	Memi[$1+14]	# List of object masks
define	PAR_INCATLIST	Memi[$1+15]	# List of input catalogs
define	PAR_OUTCATLIST	Memi[$1+16]	# List of output catalogs
define	PAR_CATDEFLIST	Memi[$1+17]	# List of catalog definitions
define	PAR_LOGLIST	Memi[$1+18]	# List of log files
define	PAR_OUTSKYLIST	Memi[$1+19]	# List of output sky images
define	PAR_OUTSIGLIST	Memi[$1+20]	# List of output sigma images

define	PAR_SKY		Memi[$1+21]	# Sky parameters
define	PAR_DET		Memi[$1+22]	# Detection parameters
define	PAR_SPT		Memi[$1+23]	# Split parameters
define	PAR_GRW		Memi[$1+24]	# Grow parameters
define	PAR_EVL		Memi[$1+25]	# Evaluate parameters

define	PAR_OMTYPE	Memi[$1+26]	# Output object mask type
define	PAR_EXTNAMES	Memc[P2C($1+27)] # Extensions names
