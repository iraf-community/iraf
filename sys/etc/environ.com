# Common for the environment list package.

pointer	envbuf			# buffer containing the environment list
int	len_envbuf		# length of the envbuf buffer
int	last			# index of the last list element entered
int	top			# index of the next list element
short	threads[NTHREADS]	# hashed threads through list
common	/envcom/ envbuf, len_envbuf, last, top, threads
