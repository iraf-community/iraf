# TRSOPEN.COM -- Common block holding global variables for trsopen

pointer	tabptr		# table descriptor
pointer	tokbuf		# buffer to hold last tokens
pointer	errbuf		# token to hold error message
pointer	treebuf		# buffer to hold intermediate tree representation
pointer	pcode		# pseudocode structure
int	itop		# top of stack index
int	itok		# end of token index
int	ival		# next available location in value buffer
int	itree		# next available node in tree
int	stack[MAXSTACK]	# stack of pending file descriptors

common	/ trscom / tabptr, tokbuf, errbuf, treebuf, pcode, 
		   itop, itok, ival, itree, stack
