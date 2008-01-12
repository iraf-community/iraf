# ACEDETECT parameter structure.
define	PAR_SZSTR	199		# Length of strings in par structure
define	PAR_LEN		128		# Length of parameter structure

define	PAR_IMLIST	Memi[P2I($1+$2-1)]	# List of images (2)
define	PAR_BPMLIST	Memi[P2I($1+$2+1)]	# List of bad pixel masks (2)
define	PAR_SKYLIST	Memi[P2I($1+$2+3)]	# List of skys (2)
define	PAR_SIGLIST	Memi[P2I($1+$2+5)]	# List of sigmas (2)
define	PAR_EXPLIST	Memi[P2I($1+$2+7)]	# List of sigmas (2)
define	PAR_GAINLIST	Memi[P2I($1+$2+9)]	# List of measurement gain maps (2)
define	PAR_SCALELIST	Memi[P2I($1+$2+11)]	# List of scales (2)
define	PAR_OMLIST	Memi[P2I($1+14)]	# List of object masks
define	PAR_INCATLIST	Memi[P2I($1+15)]	# List of input catalogs
define	PAR_OUTCATLIST	Memi[P2I($1+16)]	# List of output catalogs
define	PAR_CATDEFLIST	Memi[P2I($1+17)]	# List of catalog definitions
define	PAR_LOGLIST	Memi[P2I($1+18)]	# List of log files
define	PAR_OUTSKYLIST	Memi[P2I($1+19)]	# List of output sky images
define	PAR_OUTSIGLIST	Memi[P2I($1+20)]	# List of output sigma images

define	PAR_SKY		Memi[P2I($1+21)]	# Sky parameters
define	PAR_DET		Memi[P2I($1+22)]	# Detection parameters
define	PAR_SPT		Memi[P2I($1+23)]	# Split parameters
define	PAR_GRW		Memi[P2I($1+24)]	# Grow parameters
define	PAR_EVL		Memi[P2I($1+25)]	# Evaluate parameters

define	PAR_OMTYPE	Memi[P2I($1+26)]	# Output object mask type
define	PAR_EXTNAMES	Memc[P2C($1+27)] # Extensions names
