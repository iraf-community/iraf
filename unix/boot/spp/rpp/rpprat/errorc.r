
include defs

# ERRORC -- Process an error statement.  "call error" already processed.


subroutine errorc (str)

character str(1)
include COMMON_BLOCKS

	xfer = YES
	call outstr (str)
	call balpar			# output "(errcod, errmsg)"
	ername = NO			# just to be safe
	call outdon
	call outtab
	call ogotos (retlab, NO)	# always return after error statement
	call outdon
end
