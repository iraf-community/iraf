# Variables needed by comparison routines used by the sort routines

int	lendata		# length of a data element in units of its type
pointer	dataptr		# pointer to the beginning of array holding 
			# data to be sorted

common	/compare/	lendata, dataptr
