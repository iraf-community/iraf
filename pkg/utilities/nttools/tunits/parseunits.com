# PARSEUNITS.COM -- Global variables used by parse_units

common	/ parse / tun, abrev, tokbuf, nxttok, debug

pointer	tun		# descriptor containing results of parse
pointer	abrev		# hash table of unit abbreviations
pointer	tokbuf		# buffer holding tokens parsed from units string
int	nxttok		# index to next free space in token buffer
int	debug		# debugging message flag
