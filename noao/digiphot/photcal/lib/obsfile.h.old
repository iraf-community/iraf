# Set up the definitions for the required input fields

define	CAT_NFIELDS	9

define	CAT_IMAGE	1		# the image name
define	CAT_XCENTER	2		# x center position
define	CAT_YCENTER	3		# y center position
define	CAT_IFILTER	4		# the filter id
define	CAT_ITIME	5		# the exposure time
define	CAT_XAIRMASS	6		# the airmass
define	CAT_OTIME	7		# the time of observation
define	CAT_MAG		8		# magnitude
define	CAT_MERR	9		# magnitude error

define	OBS_NFIELDS	5

define	OBS_IMAGE	1
define	OBS_IFILTER	2
define	OBS_ITIME	3
define	OBS_XAIRMASS	4
define	OBS_OTIME	5

# Define the IMTABLE data structure

define	LEN_IMT_STRUCT	(10 + 3 * SZ_FNAME + 3)	

define	IMT_IMSETNO	Memi[$1]		     # the image set id number
define	IMT_IMNO	Memi[$1+1]		     # the image sequence number
define	IMT_OFFSET	Memi[$1+2]		     # offset to image data
define	IMT_NENTRIES	Memi[$1+3]		     # number of data entries 
define	IMT_XSHIFT	Memr[$1+4]		     # x shift in pixels
define	IMT_YSHIFT	Memr[$1+5]		     # y shift in pixels
define	IMT_APERCOR	Memr[$1+6]		     # aperture corrections
define	IMT_ITIME	Memr[$1+7]		     # the exposure times
define	IMT_XAIRMASS	Memr[$1+8]		     # the airmasses
define	IMT_OTIME	Memr[$1+9]		     # time of observations
define	IMT_IFILTER	Memc[P2C($1+10)]	     # the filter ids
define	IMT_LABEL	Memc[P2C($1+10+SZ_FNAME+1)]   # image set labels
define	IMT_IMNAME	Memc[P2C($1+10+2*SZ_FNAME+2)] # image names

# Miscellaneous

define	LEN_IMTABLE	100		# initial length of the image table
define	DEF_BUFSIZE	1000		# default object data buffer size

define	FIRST_COLUMN		3		
define	DELTA_COLUMN		2
