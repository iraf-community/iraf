# IDS common.  A common is necessary since there is no graphics descriptor
# in the argument list of the kernel procedures.  The data structures
# are designed along the lines of FIO: a small common is used to hold the time
# critical data elements, and an auxiliary dynamically allocated descriptor is
# used for everything else.

pointer	i_kt				# kernel image display descriptor
pointer	i_tty				# graphcap descriptor
int	i_in, i_out			# input file, output file
int	i_xres, i_yres			# desired device resolution
long	i_frsize			# frame size in chars
short	i_maxframes, i_maxgraph		# max num. of image frames, gr. planes
int	i_linemask			# current linemask
int	i_linewidth			# current line width
int	i_linecolor			# current line color
short	i_pt_x, i_pt_y			# current plot point, device coords
int	i_csize				# text character size
int	i_font				# text font
bool	i_snap				# true if a snap in progress
bool	i_image				# frame/bitplane data is for image
char	i_device[SZ_IDEVICE]		# force output to named device

common	/idscom/ i_kt, i_tty, i_in, i_out, i_xres, i_yres, i_frsize,
	i_maxframes, i_maxgraph, i_linemask, i_linewidth, i_linecolor,
	i_pt_x, i_pt_y, i_csize, i_font, i_snap, i_image, i_device
