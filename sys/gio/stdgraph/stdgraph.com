# STDGRAPH common.  A common is necessary since there is no graphics descriptor
# in the argument list of the kernel procedures.  The stdgraph data structures
# are designed along the lines of FIO: a small common is used to hold the time
# critical data elements, and an auxiliary dynamically allocated descriptor is
# used for everything else.  For maximum efficiency the polyline generation and
# coordinate transformation datums are kept in the common.

pointer	g_sg				# stdgraph graphics descriptor
pointer	g_tty				# graphcap descriptor
pointer	g_term				# termcap descriptor for terminal
pointer	g_pbtty				# script graphcap, playback mode
int	g_nopen				# open count for the kernel
int	g_active			# workstation is open for graphics i/o
int	g_enable			# graphics is enabled
int	g_message			# message mode (output text)
pointer	g_msgbuf			# message buffer (input text)
int	g_msgbuflen			# allocated size of message buffer
int	g_msglen			# amount of data in message
int	g_keycol			# used to show keys in playback mode
int	g_keyline			# used to show keys in playback mode
pointer	g_xy				# pointer to coord encoding string
int	g_stream			# graphics stream (metacode)
int	g_in, g_out			# input, output streams to device
int	g_ucaseout			# stty ucaseout status flag
int	g_xres, g_yres			# desired device resolution
int	g_dxres, g_dyres		# scale down to resolution coords
real	g_dx, g_dy			# scale GKI to window coords
int	g_x1, g_y1			# origin of device window
int	g_x2, g_y2			# upper right corner of device window
int	g_lastx, g_lasty		# used to clip unresolved points
int	g_hardchar			# controls use of hardware character gen
int	g_cursor			# user override of logical cursor
int	g_reg[NREGISTERS]		# encoder registers
char	g_mem[SZ_MEMORY]		# encoder memory
char	g_device[SZ_GDEVICE]		# device name for forced device output
char	g_pbdevice[SZ_GDEVICE]		# device name of playback script
char	g_hixy[TEK_XRES]		# lookup tables for tek encoding
char	g_lox[TEK_XRES]			# 	"		"
char	g_loy[TEK_YRES]			# 	"		"

common	/stgcom/ g_sg, g_tty, g_term, g_pbtty, g_nopen, g_active, g_enable,
	g_message, g_msgbuf, g_msgbuflen, g_msglen,
	g_keycol, g_keyline, g_xy, g_stream, g_in, g_out,
	g_ucaseout, g_xres, g_yres, g_dxres, g_dyres, g_dx, g_dy, g_x1,
	g_y1, g_x2, g_y2, g_lastx, g_lasty, g_hardchar, g_cursor, g_reg,
	g_mem, g_device, g_pbdevice, g_hixy, g_lox, g_loy
