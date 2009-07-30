# tbferr -- get FITSIO error message and call error
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbferr (status)

int	status		# i: FITSIO error number; zero is OK
#--
size_t	sz_val
pointer sp
pointer errmess		# for error message
pointer mess2		# for additional error messages
bool	done

begin
	if (status == 0)
	    return

	call smark (sp)
	sz_val = SZ_LINE
	call salloc (errmess, sz_val, TY_CHAR)
	call salloc (mess2, sz_val, TY_CHAR)

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
