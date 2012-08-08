# SCAN.COM -- Global common for the scan family of routines.

define	SZ_SCANBUF	4096

int	sc_ip			# char pointer into lbuf
int	sc_ntokens		# keep track of successful conversions
bool	sc_stopscan		# set if conversion is unsuccessful
char	sc_scanbuf[SZ_SCANBUF]	# line buffer for scan procedures

common	/scncom/ sc_ip, sc_ntokens, sc_stopscan, sc_scanbuf
