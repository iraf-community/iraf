# Common parameters for logging and the spectrum symbol table.

pointer	aps		# Pointer to aperture list
pointer	raps		# Pointer to reference aperture list
pointer	sort		# Pointer to sort keyword
pointer	group		# Pointer to group keyword
int	select		# Selection type
int	time		# Is sort keyword a time?
real	timewrap	# Timewrap parameter
int	verbose		# Verbose output?
int	logfiles	# List of log files
pointer	stp		# Symbol table for previously mapped spectra

common	/refcom/ aps, raps, sort, group, select, time, timewrap, verbose,
		logfiles, stp
