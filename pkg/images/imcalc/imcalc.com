# The image calculator common.

pointer	c_registers			# register array
pointer	c_images			# image descriptors
pointer	c_sbuf				# string buffer
pointer	c_nextch			# next char in string buffer
pointer	c_lastch			# last char in string buffer
int	c_nextreg			# next register
int	c_nextimage			# next image
int	c_ic				# instruction counter
int	c_op				# next instruction
int	c_ateof				# EOF reached on output image
int	c_callno			# call frame number
int	c_metacode[LEN_INSTRUCTION,MAX_INSTRUCTIONS]
short	c_arg[MAX_ARGS,MAX_CALLS]	# call frames (argument lists)
short	c_nargs[MAX_CALLS]		# number of args per frame

common	/imccom/ c_registers, c_images, c_sbuf, c_nextch, c_lastch, c_nextreg,
	c_nextimage, c_ic, c_op, c_ateof, c_callno, c_metacode, c_arg, c_nargs
