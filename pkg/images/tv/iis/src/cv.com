# common block for cv

pointer	cv_gp				# file descriptor to write
pointer	cv_stack			# working space for escape sequences
int	cv_maxframes			# device max frames
int	cv_maxgraph			# device max graph planes
int	cv_xcen, cv_ycen		# user pixel coords of center of dev.
int	cv_xres, cv_yres		# device resolution
int	cv_zres				# device z resolution
real	cv_xcon, cv_ycon		# conversion from NDC to GKI
int	cv_grch				# graphics channel
real	cv_xwinc, cv_ywinc		# cursor position for window command

common	/cvcom/ cv_gp, cv_stack, cv_maxframes, cv_maxgraph, cv_xcen, cv_ycen,
	cv_xres, cv_yres, cv_zres, cv_xcon, cv_ycon, cv_grch,
	cv_xwinc, cv_ywinc
