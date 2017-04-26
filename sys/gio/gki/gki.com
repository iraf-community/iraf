# Common for the GKI (graphics kernel interface) package.

int	gk_type[LAST_FD]		# type of output
int	gk_fd[LAST_FD]			# output file descriptor
int	gk_dd[LEN_GKIDD]		# local device driver
int	gk_prpsio			# EPA of pr_psio procedure

common	/gkicom/ gk_type, gk_fd, gk_dd, gk_prpsio
