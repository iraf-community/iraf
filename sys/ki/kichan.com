# KICHAN.COM -- Channel descriptor common for the kernel interface.

int	k_node[MAX_CHANNELS]		# kernel server node (NULL if local)
int	k_oschan[MAX_CHANNELS]		# iraf kernel (host) channel or PID
int	k_status[MAX_CHANNELS]		# status holding word for ZSTT
pointer	k_bufp[MAX_CHANNELS]		# buffer pointer

common	/kichan/ k_node, k_oschan, k_status, k_bufp
