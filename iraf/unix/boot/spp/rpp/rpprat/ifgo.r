include  defs

# IFGO - generate "if (.not.(...)) goto lab"

subroutine ifgo (lab)

integer lab
include COMMON_BLOCKS
string ifnot "if (.not."
string serrchk ".and.(.not.xerflg)) "

	call outtab			# get to column 7
	call outstr (ifnot)		# " if (.not. "
	call balpar			# collect and output condition
	if (ername == YES)		# add error checking?
	    call outstr (serrchk)
	else {
	    call outch (RPAREN)		# " ) "
	    call outch (BLANK)
	}
	call outgo (lab)		# " goto lab "
	call errgo
end
