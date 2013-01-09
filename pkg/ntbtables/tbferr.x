# tbferr -- get FITSIO error message and call error
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbferr (status)

int	status		# i: FITSIO error number; zero is OK
#--
pointer sp
pointer errmess		# for error message
pointer mess2		# for additional error messages
bool	done

begin
	if (status == 0)
	    return

	call smark (sp)
	call salloc (errmess, SZ_LINE, TY_CHAR)
	call salloc (mess2, SZ_LINE, TY_CHAR)

	# Get the oldest error message.
	call fsgmsg (Memc[errmess])

	# Get more recent messages, if any.
	done = false
	while (!done) {
	    call fsgmsg (Memc[mess2])
	    if (Memc[mess2] == EOS) {
		done = true
	    } else {
		call strcat (" ", Memc[errmess], SZ_LINE)
		call strcat (Memc[mess2], Memc[errmess], SZ_LINE)
	    }
	}

	call error (status, Memc[errmess])
end
