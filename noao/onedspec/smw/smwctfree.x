include	<smw.h>


# SMW_CTFREE -- Free a spectral SMW coordinate transform pointer.

procedure smw_ctfree (ct)

pointer	ct		# SMW CT pointer
int	i

begin
	if (ct == NULL)
	    return

	do i = 0, SMW_NCT(ct)-1
	    call mw_ctfree (SMW_CT(ct,i))
	call mw_ctfree (SMW_CTL(ct))
	call mfree (ct, TY_STRUCT)
end
