# Evaluator common. This common handles the code counter and the code
# buffer used by the code generator routines during an expression
# code generation.

int	cp			# next free instruction
pointer	code			# RPN code buffer

common	/prcodecom/	cp, code
