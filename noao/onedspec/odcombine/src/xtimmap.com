int	option
int	nopen
int	nopenpix
int	nalloc
int	last_flag
int	min_open
pointer	ims
common	/xtimmapcom/ option, ims, nopen, nopenpix, nalloc, last_flag, min_open
