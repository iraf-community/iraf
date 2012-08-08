# VEX.COM -- Global variables used by vex parsing routine

pointer	line		# Buffer containing next line in expression
pointer	ch		# Pointer to next character in expression
int	ncode		# Length of code array
int	maxcode		# Maximum length of code array
pointer	code		# Pointer to next available code
pointer	stack		# Pointer to stack structure

common	/vex/  line, ch, ncode, maxcode, code, stack

