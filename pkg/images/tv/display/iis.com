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

int	iis_version		# WCS version
int	iis_valid		# valid mapping info flag
char	iis_region[SZ_FNAME]	# region name
real	iis_sx,  iis_sy		# source raster offset
int	iis_snx, iis_sny	# source raster size
int	iis_dx,  iis_dy		# dest raster offset
int	iis_dnx, iis_dny	# dest raster size
char	iis_objref[SZ_FNAME]	# object reference

common	/iiscom/ iischan, iisnopen, iisframe, iis_xdim, iis_ydim, iis_config,
	iis_server, packit, swap_bytes, hdr, iis_version, iis_valid,
	iis_region, iis_sx, iis_sy, iis_snx, iis_sny,
	iis_dx, iis_dy, iis_dnx, iis_dny, iis_objref
