
include  defs

# OGOTOS - Output "goto	n", unconditionally.


subroutine ogotos (n, error_check)

integer n, error_check
include COMMON_BLOCKS
string sgoto "goto "

	call outtab
	call outstr (sgoto)
	call outnum (n)
	if (error_check == YES)
	    call outdwe
	else
	    call outdon
end
