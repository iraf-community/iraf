# Common for IIS display

int	iischan			# the device channel used by FIO
int	iisnopen		# number of times the display has been opened
int	iisframe		# frame number at iisopn time (kludge).
int	iis_xdim, iis_ydim	# frame size, pixels
int	iis_config		# frame size configuration
int	iis_server		# device is actually a display server
bool	packit			# byte pack data for i/o
bool	swap_bytes		# byte swap the IIS header
short	hdr[LEN_IISHDR]		# header

common	/iiscom/ iischan, iisnopen, iisframe, iis_xdim, iis_ydim, iis_config,
	iis_server, packit, swap_bytes, hdr
