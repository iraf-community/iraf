# CCP common.  A common is necessary since there is no graphics descriptor
# in the argument list of the kernel procedures.  The kernel data structures
# are designed along the lines of FIO: a small common is used to hold the time
# critical data elements, and an auxiliary dynamically allocated descriptor is
# used for everything else.

pointer	g_cc				# kernel graphics descriptor
pointer	g_tty				# graphcap descriptor
int	g_nframes			# number of frames written
int	g_maxframes			# max frames per device metafile
int	g_ndraw				# no draw instr. in current frame
int	g_in				# input file
real	g_xres				# x resolution of plotter
real	g_yres				# y resolution of plotter
real	g_max_x				# maximum x drawn, in plotter units
real	g_xndcto_p			# x(pltr) = GKI*g_xndcto_p; final scale
real	g_yndcto_p			# y(pltr) = GKI*g_yndcto_p; final scale
real	g_xtask_scale			# x scale determined from task params
real	g_ytask_scale			# y scale determined from task params
real	g_xdefault_scale		# x scale from graphcap or compile-time
real	g_ydefault_scale		# y scale from graphcap or compile-time
int	g_ltype				# line type
real	g_dashlen			# length of dash in dashed line, p_units
real	g_gaplen			# width of gap in dash/dot line, p_units
real	g_plwsep			# polyline width separation for ntracing
int	g_txquality			# text quality parameter
bool	g_ltover			# user override of line-type generator
bool	g_lwover			# user override of line width simulation
bool	g_lcover			# user override of line color generator
char	g_lwtype			# line width mode parameter
char	g_device[SZ_GDEVICE]		# force output to named device

common	/ccpcom/ g_cc, g_tty, g_nframes, g_maxframes, g_ndraw,
		 g_in, g_xres, g_yres, g_max_x, g_xndcto_p, g_yndcto_p,
		 g_xtask_scale, g_ytask_scale, 
		 g_xdefault_scale, g_ydefault_scale,
		 g_ltype, g_dashlen, g_gaplen, g_plwsep, g_txquality,
		 g_ltover, g_lwover, g_lcover, g_lwtype, g_device
