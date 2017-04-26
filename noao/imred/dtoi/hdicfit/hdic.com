# Common block for hdtoi package/ icgfit package interface.

int	nraw
double	maxden
pointer	den, big_den
common	/raw/maxden, den, big_den, nraw
