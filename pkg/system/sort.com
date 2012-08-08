# Common for the SORT program.

int	column			# column to be sorted (if nonzero)
int	ignore_whitespace	# ignore leading whitespace
int	numeric_sort		# sort numerically rather than alphabetically
int	reverse_sort		# reverse the sense of the sort
int	use_strsrt		# ok to use standard STRSRT routine?

common	/srtcom/ column, ignore_whitespace, numeric_sort, reverse_sort,
	use_strsrt
