# KINODE.COM -- Node descriptor table.  Contains one entry for each node listed
# in the host name table file.

int	n_nnodes				# number of nodes in table
int	n_local					# index of the local node
int	n_default				# index of the current defnode
int	n_kschan[MAX_NODES]			# server channel (init to NULL)
int	n_nrefs[MAX_NODES]			# number of k_oschan using node
int	n_status[MAX_NODES]			# status bits for channel
int	n_nalias[MAX_NODES]			# number of aliases
char	n_localnode[SZ_ALIAS]			# name of the local node
char	n_defaultnode[SZ_ALIAS]			# name of the default node
char	n_nodename[SZ_ALIAS]			# node name working storage
char	n_server[SZ_SERVER,MAX_NODES]		# kernel server names
char	n_alias[SZ_ALIAS,MAX_ALIAS,MAX_NODES]	# aliases for the node

common	/kinode/ n_nnodes, n_local, n_default, n_kschan, n_nrefs, n_status,
	n_nalias, n_localnode, n_defaultnode, n_nodename, n_server, n_alias
