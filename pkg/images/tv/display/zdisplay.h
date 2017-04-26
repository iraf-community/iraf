# Display devices defined by OS

define	IIS		"/dev/iis"	# IIS display device
define	IIS_CHAN	1		# Device channel identifier
define	DEVCODE		100		# Channel = DEVCODE * DEVCHAN
define	FRTOCHAN	(IIS_CHAN*DEVCODE+($1))
