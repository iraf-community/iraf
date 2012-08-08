# EVVEXPR common.

pointer	ev_oval				# pointer to expr value operand
int	ev_st				# symbol table
int	ev_getop			# user supplied get operand procedure
int	ev_getop_data			# client data for above
int	ev_ufcn				# user supplied function call procedure
int	ev_ufcn_data			# client data for above
int	ev_flags			# flag bits

common	/xvvcom/ ev_oval, ev_st, ev_getop, ev_getop_data, ev_ufcn,
	ev_ufcn_data, ev_flags
