
include  defs

# THENCO -- Generate code for the "then" part of a compound IFERR statement.


subroutine thenco (tok, lab)

integer lab, tok
include COMMON_BLOCKS
string	siferr "if (.not.xerpop()) "
string	sifnoerr "if (xerpop()) "

	xfer = NO
	call outnum (lab+2)
	call outtab
	if (tok == LEXIFERR)
	    call outstr (siferr)
	else
	    call outstr (sifnoerr)
	call outgo (lab)
	esp = esp - 1				# pop error stack
	call indent (1)
	return
end
