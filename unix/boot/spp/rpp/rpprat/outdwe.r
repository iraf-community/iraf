
include defs

# OUTDWE -- (outdon with error checking).
# Called by code generation routines to output a line of code,
# possibly followed by an error checking instruction.


subroutine outdwe

	call outdon
	call errgo
end
