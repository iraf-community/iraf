# SCAN.COM -- Global common for the scan family of routines.

int	sc_ip			# char pointer into lbuf
int	sc_ntokens		# keep track of successful conversions
bool	sc_stopscan		# set if conversion is unsuccessful
char	sc_scanbuf[SZ_COMMAND]	# line buffer for scan procedures

common	/scncom/ sc_ip, sc_ntokens, sc_stopscan, sc_scanbuf
