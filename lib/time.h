# TIME.H -- Define the time value structure for brktime, cnvtime.

define	SZ_TIME		24		# size of "Dow 00:00:00 dd-Mmm-yyyy"
define	SZ_DATE		12		# size of "Mmm dd hh:mm"
define	LEN_TMSTRUCT	8		# length of time struct

define	TM_SEC		$1[1]		# seconds (0-59)
define	TM_MIN		$1[2]		# minutes (0-59)
define	TM_HOUR		$1[3]		# hour (0-23)
define	TM_MDAY		$1[4]		# day of month (1-31)
define	TM_MONTH	$1[5]		# month (1-12)
define	TM_YEAR		$1[6]		# year, e.g. "1982"
define	TM_WDAY		$1[7]		# day of week (Sun == 1)
define	TM_YDAY		$1[8]		# day of year (1-365/6)

# Conversion flags for the etc$dtm date/time conversion routines.
define	TF_OLDFITS	1		# old FITS format DATE-OBS string
