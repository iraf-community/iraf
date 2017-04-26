# ISM I/O common.
int	fdin 					# input descriptor
int	fdout 					# output descriptor
int	mode 					# file mode
int	nbuf 					# no. chars in buffer
int	bp 					# begin buffer ptr
int	ep					# end buffer ptr
char	buffer[2*SZ_MESSAGE+1]			# text buffer

common	/ismfd/ fdin, fdout, mode, nbuf, buffer, bp, ep

