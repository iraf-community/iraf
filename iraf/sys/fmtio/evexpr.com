# EVEXPR common.

pointer	ev_oval				# pointer to expr value operand
pointer	ev_getop			# user supplied get operand procedure
pointer	ev_ufcn				# user supplied function call procedure

common	/xevcom/ ev_oval, ev_getop, ev_ufcn
