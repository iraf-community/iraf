# Common for IIS display

int	iischan			# The device channel used by FIO
int	iisnopen		# Number of times the display has been opened
int	iframe, iplane		# frame, bitplanes to read/write
int	i_frame_on		# Which frame is on...cursor readback
short	hdr[LEN_IISHDR]		# Header
short	zoom[16]		# zoom for each plane
short	xscroll[16]		# scroll position for each plane
short	yscroll[16]
common	/iiscom/iischan, iisnopen, iframe, iplane, i_frame_on,
		hdr, zoom, xscroll, yscroll
