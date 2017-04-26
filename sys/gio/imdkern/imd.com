# IMD common.  A common is necessary since there is no graphics descriptor
# in the argument list of the kernel procedures.  The stdgraph data structures
# are designed along the lines of FIO: a small common is used to hold the time
# critical data elements, and an auxiliary dynamically allocated descriptor is
# used for everything else.

pointer	g_kt				# kernel transform graphics descriptor
pointer	g_tty				# graphcap descriptor
int	g_nframes			# number of frames written
int	g_maxframes			# max frames per device metafile
int	g_ndraw				# no draw instr. in current frame
int	g_in, g_out			# input, output files
int	g_xres, g_yres			# desired device resolution
int	g_frame, g_color		# display frame and graphics color
char	g_device[SZ_GDEVICE]		# force output to named device

common	/gioimd/ g_kt, g_tty, g_nframes, g_maxframes, g_ndraw,
	g_in, g_out, g_xres, g_yres, g_frame, g_color, g_device
