int	sc_ip			# char pointer into lbuf
int	sc_ntokens		# keep track of successful conversions
bool	sc_stopscan		# set if conversion is unsuccessful
char	sc_scanbuf[SZ_LINE]	# line buffer for scan procedures
common	/scncom/ sc_ip, sc_ntokens, sc_stopscan, sc_scanbuf
