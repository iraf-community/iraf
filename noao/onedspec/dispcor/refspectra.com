# Common parameters for logging and the spectrum symbol table.

int	verbose		# Verbose output?
int	logfiles	# List of log files
pointer	stp		# Symbol table for previously mapped spectra

common	/refcom/ verbose, logfiles, stp
