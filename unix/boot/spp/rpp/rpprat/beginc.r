
include	defs

# BEGINC -- Code that gets executed when the "begin" statement is encountered,
# at the beginning of the executable section of a procedure.


subroutine beginc

integer	labgen
include COMMON_BLOCKS

	body = YES				# in body of procedure
	ername = NO				# errchk name not encountered
	esp = 0					# error stack pointer
	label = FIRST_LABEL			# start over with labels
	retlab = labgen (1)			# label for return stmt
	logical_column = 6 + INDENT
	col = logical_column
end
